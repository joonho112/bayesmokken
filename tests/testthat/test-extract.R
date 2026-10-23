# =============================================================================
# Extraction, preparation, diagnostics and reporting
#
# The schema tests read inst/extdata/spec/df-schemas.yml and compare it with
# what the functions actually return. Neither side is allowed to drift: if a
# column is renamed in the code the test fails, and if it is renamed only in
# the schema the test also fails.
# =============================================================================

skip_if_no_yaml <- function() skip_if_not_installed("yaml")

schema_document <- function() {
  path <- system.file("extdata", "spec", "df-schemas.yml",
                      package = "bayesmokken")
  skip_if(!nzchar(path), "schema file not installed")
  yaml::read_yaml(path)
}

schema_of <- function(name) {
  schema_document()$schemas[[name]]
}

expect_schema <- function(tab, name, mode, features = character()) {
  document <- schema_document()
  spec <- schema_of(name)
  expect_true(mode %in% unlist(spec$supported_modes),
              info = paste(name, mode, "is not a declared schema mode"))
  tags <- unique(c(unlist(document$modes[[mode]]$tags), features))
  wanted <- vapply(spec$columns, function(c) c$name, character(1))
  keep <- vapply(spec$columns, function(c) {
    required <- unlist(c$when)
    !length(required) || all(required %in% tags)
  }, logical(1))
  wanted <- wanted[keep]
  expected_types <- vapply(
    spec$columns[keep], function(column) column$type, character(1)
  )
  expect_identical(names(tab), wanted, info = paste(name, mode))
  observed_types <- vapply(tab, class, character(1))
  expect_identical(
    unname(observed_types), unname(expected_types),
    info = paste(name, mode, "column classes")
  )

  contract <- spec$mode_contracts[[mode]]
  if (!is.null(contract$required_non_missing)) {
    for (column in unlist(contract$required_non_missing)) {
      expect_false(anyNA(tab[[column]]),
                   info = paste(name, mode, column, "must be defined"))
    }
  }
  if (!is.null(contract$required_na)) {
    for (column in unlist(contract$required_na)) {
      expect_true(all(is.na(tab[[column]])),
                  info = paste(name, mode, column, "must be NA"))
    }
  }
  if (!is.null(contract$allowed_values)) {
    for (column in names(contract$allowed_values)) {
      observed <- unique(tab[[column]][!is.na(tab[[column]])])
      allowed <- unlist(contract$allowed_values[[column]])
      expect_true(
        all(observed %in% allowed),
        info = paste(name, mode, column, "has an undeclared state")
      )
    }
  }

  for (relation in spec$relations) {
    required <- unlist(relation$when)
    if (length(required) && !all(required %in% tags)) next
    if (identical(relation$kind, "zero_count_means_na")) {
      zero <- tab[[relation$count]] == 0L
      for (column in unlist(relation$values)) {
        expect_true(
          all(is.na(tab[[column]][zero])),
          info = paste(name, mode, column, "must be NA at zero draws")
        )
      }
    } else {
      fail(paste("Unknown executable schema relation:", relation$kind))
    }
  }
  invisible(tab)
}

bin_fit <- function(draws = 300) {
  bb_mokken_h(srs, draws = draws, seed = 1, warn = FALSE)
}
clu_fit <- function(target = "respondent", draws = 300) {
  items <- paste0("wb", 1:6)
  bb_mokken_h(classroom_wb[, items], item_type = "ordinal",
              supports = bm_supports(classroom_wb, items = items),
              cluster = as.character(classroom_wb$class_id),
              cluster_target = target, draws = draws, seed = 1, warn = FALSE)
}
# The historical two-stage lane of 0.9.0, declared explicitly; its warning is
# the documented behaviour and is silenced here by class only.
clu_two_stage_fit <- function(target = "respondent", draws = 300) {
  items <- paste0("wb", 1:6)
  suppressWarnings(
    bb_mokken_h(classroom_wb[, items], item_type = "ordinal",
                supports = bm_supports(classroom_wb, items = items),
                cluster = as.character(classroom_wb$class_id),
                cluster_target = target, weight_law = "hierarchical_cluster",
                draws = draws, seed = 1, warn = FALSE),
    classes = "bm_historical_method_warning"
  )
}
det_clu_fit <- function(target = "respondent") {
  items <- paste0("wb", 1:6)
  mokken_h(classroom_wb[, items], item_type = "ordinal",
           supports = bm_supports(classroom_wb, items = items),
           cluster = as.character(classroom_wb$class_id),
           cluster_target = target, weight_law = "hierarchical_cluster",
           warn = FALSE)
}
boundary_fit <- function() {
  suppressWarnings(bb_mokken_h(
    data.frame(a = rep(0, 5), b = c(0, 1, 0, 1, 1)),
    draws = 25, seed = 5
  ))
}

expect_object_mode <- function(fit, mode) {
  spec <- schema_document()$modes[[mode]]
  expect_s3_class(fit, spec$object_class)
  provenance <- bm_provenance(fit)
  expect_identical(provenance$item_type, spec$item_type)
  expect_identical(!is.na(bm_scale_h(fit)$n_clusters), spec$clustered)
  if (identical(spec$interval, "unavailable")) {
    expect_error(bm_interval(fit), class = "bm_input_error")
  }
  if (identical(spec$draws, "unavailable")) {
    expect_error(bm_draws(fit), class = "bm_input_error")
  }
}

# --- schema conformance -------------------------------------------------------

test_that("all declared object modes execute exact extractor schemas", {
  skip_if_no_yaml()
  fixtures <- list(
    binary_bb = bin_fit(),
    clustered_bb = clu_fit(),
    clustered_two_stage_bb = clu_two_stage_fit(),
    deterministic_binary = mokken_h(srs, warn = FALSE),
    deterministic_clustered = det_clu_fit(),
    failure_boundary = boundary_fit()
  )
  for (mode in names(fixtures)) {
    fit <- fixtures[[mode]]
    expect_object_mode(fit, mode)
    expect_schema(bm_scale_h(fit), "bm_scale_h", mode)
    expect_schema(bm_provenance(fit), "bm_provenance", mode)
    expect_schema(bm_item_h(fit), "bm_item_h", mode)
    expect_schema(bm_pair_h(fit), "bm_pair_h", mode)

    if (inherits(fit, "bm_bb_h")) {
      expect_schema(
        bm_item_h(fit, include_interval = TRUE),
        "bm_item_h", mode, features = "interval_columns"
      )
      expect_schema(
        bm_pair_h(fit, include_interval = TRUE),
        "bm_pair_h", mode, features = "interval_columns"
      )
      expect_schema(bm_interval(fit), "bm_interval", mode)
      for (what in c("scale", "item", "pair")) {
        expect_schema(
          bm_draws(fit, what = what, format = "long"),
          "bm_draws_long", mode, features = what
        )
      }
      expect_schema(
        fit$summaries$pair_draws, "bm_pair_draw_summary", mode
      )
    }
  }
})

test_that("the executable binary schema admits the actual partial-draw state", {
  skip_if_no_yaml()
  fit <- bin_fit(draws = 120)
  draws <- seq(0.05, 0.35, length.out = 120)
  draws[17L] <- NA_real_
  fit$interval <- bayesmokken:::.bm_interval_from_draws(
    bb_draws = draws,
    point_estimate = fit$empirical$scale$H,
    n_complete = fit$empirical$provenance$n_complete
  )
  expect_identical(fit$interval$status, "OK_WITH_UNDEFINED_DRAWS")
  expect_schema(bm_interval(fit), "bm_interval", "binary_bb")
  expect_schema(bm_scale_h(fit), "bm_scale_h", "binary_bb")
})

test_that("the uncalibrated label in the code equals the one in the schema", {
  skip_if_no_yaml()
  lit <- yaml::read_yaml(system.file("extdata", "spec", "df-schemas.yml",
                                     package = "bayesmokken"))$literals
  expect_identical(bayesmokken:::.bm_uncalibrated, lit$uncalibrated_label)
  expect_identical(unique(bm_item_h(
    bin_fit(), include_interval = TRUE
  )$interval_calibration),
                   lit$uncalibrated_label)
})

# --- values -------------------------------------------------------------------

test_that("extracted values agree with the fitted object", {
  fit <- bin_fit()
  s <- bm_scale_h(fit)
  expect_equal(s$H, fit$empirical$scale$H, tolerance = 0)
  expect_equal(s$lower, fit$interval$lower, tolerance = 0)
  expect_equal(s$upper, fit$interval$upper, tolerance = 0)
  expect_identical(s$interval_status, fit$interval$status)
  expect_identical(fit$interval$status, "OK")
  expect_false("status" %in% names(s))
  expect_identical(s$n, 337L)
  expect_identical(s$n_items, 11L)

  it <- bm_item_h(fit)
  expect_identical(nrow(it), 11L)
  expect_equal(sort(it$H), it$H, tolerance = 0)      # ascending
  expect_setequal(it$item, names(srs))

  pr <- bm_pair_h(fit)
  expect_identical(nrow(pr), 55L)
})

test_that("item intervals are the raw type-8 quantiles of the item draws", {
  fit <- bin_fit()
  it <- bm_item_h(fit, include_interval = TRUE)
  for (k in seq_len(nrow(it))) {
    d <- fit$draws$item[, it$item[k]]
    d <- d[is.finite(d)]
    q <- stats::quantile(d, c(0.025, 0.975), names = FALSE, type = 8)
    expect_equal(it$bb_lower[k], q[1], tolerance = 1e-12)
    expect_equal(it$bb_upper[k], q[2], tolerance = 1e-12)
  }
})

test_that("item and pair interval summaries are canonical opt-in fields", {
  fit <- bin_fit(draws = 120)
  expect_false("bb_lower" %in% names(bm_item_h(fit)))
  expect_false("bb_lower" %in% names(bm_pair_h(fit)))
  expect_true("bb_lower" %in% names(
    bm_item_h(fit, include_interval = TRUE)
  ))
  pair <- bm_pair_h(fit, include_interval = TRUE)
  expect_true("bb_lower" %in% names(pair))
  expect_identical(pair$defined_draws, as.integer(pair$defined_draws))
})

test_that("the deprecated interval alias migrates without ambiguity", {
  fit <- bin_fit(draws = 120)
  expect_warning(
    old_item <- bm_item_h(fit, interval = TRUE),
    "deprecated"
  )
  expect_warning(
    old_pair <- bm_pair_h(fit, interval = TRUE),
    "deprecated"
  )
  expect_identical(
    old_item,
    bm_item_h(fit, include_interval = TRUE)
  )
  expect_identical(
    old_pair,
    bm_pair_h(fit, include_interval = TRUE)
  )
  expect_error(
    bm_item_h(fit, include_interval = TRUE, interval = TRUE),
    class = "bm_input_error"
  )
  expect_error(
    bm_pair_h(fit, include_interval = TRUE, interval = TRUE),
    class = "bm_input_error"
  )
  expect_error(
    bm_item_h(fit, include_interval = NA),
    class = "bm_input_error"
  )
})

test_that("clustered fits carry their target and cluster count", {
  fit <- clu_fit("equal_cluster")
  s <- bm_scale_h(fit)
  expect_identical(s$cluster_target, "equal_cluster")
  expect_identical(s$n_clusters, 30L)
  expect_identical(s$method_id, "V5-C2-CRBB-ADJUSTED-CLUSTER-v1")
  expect_identical(bm_provenance(fit)$weight_law, "complete_record")
  historical <- clu_two_stage_fit("equal_cluster")
  expect_identical(bm_scale_h(historical)$method_id,
                   "V4-P15-HBB-CLUSTER-GN150-v1")
  expect_identical(bm_provenance(historical)$weight_law,
                   "hierarchical_cluster")
  expect_identical(bm_interval(historical)$construction,
                   "midpoint_halfwidth_scaling")
})

test_that("deterministic clustered provenance retains the declared law", {
  provenance <- bm_provenance(det_clu_fit("respondent"))
  expect_identical(provenance$cluster_target, "respondent")
  expect_identical(provenance$weight_law, "hierarchical_cluster")
  expect_true(is.na(provenance$method_id))
})

test_that("pair boundary NAs carry an integer defined-draw count", {
  pair <- bm_pair_h(boundary_fit(), include_interval = TRUE)
  expect_identical(pair$defined_draws, 0L)
  expect_true(all(is.na(pair[c("bb_mean", "bb_lower", "bb_upper")])))
})

test_that("missing draws fail with an explanation, not a subscript error", {
  det <- mokken_h(srs, warn = FALSE)
  expect_error(bm_draws(det), class = "bm_input_error")
  expect_error(bm_pair_h(det, include_interval = TRUE),
               class = "bm_input_error")
  expect_error(bm_interval(det), class = "bm_input_error")
  expect_error(bm_scale_h(list(a = 1)), class = "bm_input_error")
})

# --- preparation --------------------------------------------------------------

test_that("bm_supports() produces a usable supports argument", {
  items <- paste0("wb", 1:6)
  sup <- bm_supports(classroom_wb, items = items)
  expect_identical(names(sup), items)
  expect_true(all(vapply(sup, function(v) identical(v, sort(unique(v))),
                         logical(1))))
  expect_no_error(
    mokken_h(classroom_wb[, items], item_type = "ordinal", supports = sup,
             cluster = as.character(classroom_wb$class_id),
             cluster_target = "respondent",
             weight_law = "complete_record", warn = FALSE)
  )
})

test_that("bm_supports(from = 'range') fills unobserved middle categories", {
  d <- data.frame(a = c(1, 2, 4, 4), b = c(1, 1, 2, 4))
  expect_identical(bm_supports(d, from = "observed")$a, c(1, 2, 4))
  expect_identical(bm_supports(d, from = "range")$a, c(1, 2, 3, 4))
  expect_identical(bm_supports(d, from = "range", extend = 5)$a, c(1, 2, 3, 4, 5))
})

test_that("bm_score_map() reverses within the item's own range", {
  d <- data.frame(a = c(1, 2, 3, 4, 5), b = c(1, 1, 3, 5, 5))
  m <- bm_score_map(d, reverse = "a")
  expect_identical(unname(m$a[as.character(1:5)]), c(5, 4, 3, 2, 1))
  expect_null(m$b)                        # untouched items are omitted
})

test_that("bm_score_map() refuses to guess", {
  d <- data.frame(a = c(1, 2, 3), b = c(1, 2, 3))
  expect_error(bm_score_map(d, reverse = "zzz"), class = "bm_input_error")
  expect_error(bm_score_map(d, reverse = "a", to = c(1, 2)),
               class = "bm_input_error")
})

test_that("bm_cluster_info() reports sizes and both target weights", {
  info <- bm_cluster_info(classroom_wb$class_id,
                          data = classroom_wb[, paste0("wb", 1:6)])
  expect_identical(info$G, 30L)
  expect_equal(sum(info$table$weight_respondent), 1, tolerance = 1e-12)
  expect_equal(sum(info$table$weight_equal), 1, tolerance = 1e-12)
  expect_length(unique(info$table$weight_equal), 1L)
  expect_gt(info$informative_index, 0.4)   # classroom_wb is built this way
})

test_that("bm_check_data() screens without changing anything", {
  chk <- bm_check_data(srs)
  expect_s3_class(chk, "bm_data_check")
  expect_identical(chk$item_type, "binary")
  expect_identical(chk$rows$n_complete, 337L)
  expect_identical(nrow(chk$items), 11L)
  expect_true(nzchar(chk$next_call))

  chk2 <- bm_check_data(classroom_wb, items = paste0("wb", 1:6),
                        cluster = "class_id")
  expect_identical(chk2$item_type, "ordinal")
  expect_identical(chk2$clusters$G, 30L)
})

test_that("bm_check_data() flags a reverse-keyed item without fixing it", {
  d <- srs
  d$SRS3 <- 1L - d$SRS3
  chk <- bm_check_data(d)
  flagged <- chk$flags$item[chk$flags$level == "warning"]
  expect_true("SRS3" %in% flagged)
  expect_identical(d$SRS3, 1L - srs$SRS3)   # untouched
})

test_that("binary data with clusters is flagged as outside the calibrated lane", {
  # The engine resolves item_type = "auto" to "ordinal" as soon as `cluster` is
  # present, so this combination runs rather than erroring. The coefficient is
  # right; the clustered width calibration was fitted on polytomous designs.
  chk <- bm_check_data(srs, cluster = rep(1:8, length.out = nrow(srs)))
  msgs <- chk$flags$message[chk$flags$level == "warning"]
  expect_true(any(grepl("two-point", msgs)))
})

# --- diagnostics and reporting -------------------------------------------------

test_that("bm_diagnostics() covers every level", {
  rep <- bm_diagnostics(bin_fit())
  expect_s3_class(rep, "bm_diagnostic_report")
  expect_identical(rep$denominator$level, c("pair", "item", "scale"))
  expect_identical(sum(rep$denominator$positive), 55L + 11L + 1L)
  expect_true(nzchar(rep$overall))
  expect_false(grepl("PASS|FAIL", rep$overall))     # no verdicts
})

test_that("knot diagnostics exist for polytomous fits only", {
  expect_error(bm_knot_diagnostics(bin_fit()), class = "bm_input_error")
  expect_s3_class(bm_knot_diagnostics(clu_fit()), "data.frame")
})

test_that("bm_benchmark() reports relations and never verdicts", {
  b <- bm_benchmark(bin_fit())
  expect_schema(as.data.frame(b), "bm_benchmark", "binary_bb")
  expect_equal(b$difference, b$H - b$benchmark, tolerance = 0)
  expect_true(all(b$point_relation == "below"))
  expect_false("interval_relation" %in% names(b))
  txt <- tolower(paste(unlist(b), collapse = " "))
  for (word in c("pass", "fail", "significant", "acceptable", "valid")) {
    expect_false(grepl(word, txt), info = word)
  }
})

test_that("bm_compare() lines fits up and requires names", {
  a <- bin_fit()
  b <- bb_mokken_h(srs, draws = 300, seed = 1, interval = "equal_tail",
                   warn = FALSE)
  cmp <- bm_compare(calibrated = a, equal_tail = b)
  expect_identical(cmp$fit, c("calibrated", "equal_tail"))
  expect_gt(cmp$length[1], cmp$length[2])      # calibration widens
  expect_identical(
    attr(cmp, "comparability")$state,
    "same_items_same_rows"
  )
  expect_error(bm_compare(a, b), class = "bm_input_error")
  expect_error(bm_compare(only = a), class = "bm_input_error")
})

test_that("bm_report() renders and carries its notes", {
  plain <- bm_report(bin_fit(), what = "item", format = "markdown")
  expect_false(any(grepl("uncalibrated", plain$notes)))

  r <- bm_report(bin_fit(), what = "item", format = "markdown",
                 include_interval = TRUE)
  expect_s3_class(r, "bm_report")
  expect_true(any(grepl("uncalibrated", r$notes)))
  expect_true(any(grepl("^\\|", r$rendered)))
  expect_false(any(grepl("337\\.0", r$rendered)))   # counts stay counts

  l <- bm_report(bin_fit(), format = "latex")
  expect_true(any(grepl("\\\\begin\\{tabular\\}", l$rendered)))
})

test_that("bm_guardrails() describes the actual behaviour", {
  g <- bm_guardrails()
  expect_identical(names(g), c("situation", "reason", "what_to_do"))
  expect_true(any(grepl("two-category", g$situation)))
  expect_true(any(grepl("fewer than 22", g$situation)))
  expect_true(any(grepl("more than 80", g$situation)))
  expect_false(any(grepl("NOT guarded", g$reason)))
})
