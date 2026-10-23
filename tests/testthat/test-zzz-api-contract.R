# =============================================================================
# G-2  Additive-only API contract
#
# INV-2: both papers print a literal call to bb_mokken_h() in their Method
# section, and fit$interval is a published access path. Names, argument names,
# argument order, defaults, and the returned element names are therefore a
# contract with the published record. v1 may ADD; it may not rename, reorder,
# remove, or re-default.
# =============================================================================

# Frozen at import time from bayesmokken 0.0.0.9001.
v0_formals <- list(
  mokken_h = c(
    "data", "items", "score_map", "missing_codes", "weights", "row_ids",
    "missing", "zero_tol", "eta", "warn", "item_type", "supports", "cluster",
    "cluster_target", "weight_law"
  ),
  bb_mokken_h = c(
    "data", "items", "score_map", "missing_codes", "row_ids", "missing",
    "draws", "seed", "batch_size", "workers", "interval", "level", "zero_tol",
    "eta", "warn", "item_type", "supports", "cluster", "cluster_target",
    "weight_law"
  ),
  bayes_mokken_h = c(
    "data", "items", "score_map", "missing_codes", "row_ids", "missing",
    "draws", "seed", "batch_size", "workers", "threshold", "item_threshold",
    "pair_threshold", "interval", "level", "zero_tol", "eta", "warn",
    "item_type", "supports", "cluster", "cluster_target", "weight_law"
  )
)

test_that("published entry points keep their exact argument lists and order", {
  for (fn in names(v0_formals)) {
    expect_identical(
      names(formals(getExportedValue("bayesmokken", fn))),
      v0_formals[[fn]],
      info = fn
    )
  }
})

test_that("published defaults are unchanged", {
  f <- formals(bb_mokken_h)
  expect_identical(f$draws, 2000L)
  expect_identical(f$seed, 1L)
  expect_identical(f$batch_size, 250L)
  expect_identical(f$workers, 1L)
  expect_identical(f$missing, "complete_scale")
  expect_identical(f$level, 0.95)
  expect_identical(f$warn, TRUE)
  expect_identical(eval(f$interval), c("calibrated", "equal_tail"))
  expect_identical(eval(f$item_type), c("auto", "binary", "ordinal"))
  expect_identical(eval(f$cluster_target), c("respondent", "equal_cluster"))
  # 0.10.0 added a value; the published values and their order are kept.
  expect_identical(
    eval(f$weight_law),
    c("auto", "iid_respondent", "complete_record", "hierarchical_cluster")
  )

  g <- formals(mokken_h)
  expect_identical(g$missing, "complete_scale")
  expect_identical(g$warn, TRUE)
})

test_that("the call form printed in both papers still runs", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0), b = c(0, 1, 1, 1, 0, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, 0), d = c(1, 0, 1, 1, 1, 0, 1, 0)
  )
  # Paper A, Method section, verbatim shape.
  fit <- bb_mokken_h(x, draws = 200, seed = 1,
                     interval = "calibrated", level = 0.95)
  expect_s3_class(fit$interval, "bm_h_interval")
  expect_identical(fit$interval$method_id, "P7-FS-WIDTH-RAW-G025-v1")
})

test_that("returned element names are a superset of the v0 contract", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0), b = c(0, 1, 1, 1, 0, 0), c = c(0, 0, 1, 0, 1, 1)
  )
  fit <- bb_mokken_h(x, draws = 200, seed = 1, warn = FALSE)
  expect_true(all(
    c("empirical", "draws", "bb_type", "functional", "interpretation",
      "grouping", "rng", "computation", "interval", "summaries",
      "mc_diagnostics") %in% names(fit)
  ))
  expect_true(all(c("scale", "item", "pair") %in% names(fit$draws)))
  expect_true(all(
    c("method_id", "base_method_id", "status", "level", "point_estimate",
      "lower", "upper", "raw_lower", "raw_upper", "midpoint", "gamma",
      "width_multiplier", "n_complete", "defined_draws", "total_draws",
      "minimum_defined_draws", "quantile_type", "endpoint_clipping",
      "per_replication_method_cascade", "draw_filter_policy",
      "interpretation") %in% names(fit$interval)
  ))

  emp <- mokken_h(x, warn = FALSE)
  expect_true(all(
    c("pair", "item", "scale", "estimand", "provenance", "tolerances")
    %in% names(emp)
  ))
})


# =============================================================================
# G-2b  Behavioural contract
#
# The signature checks above compare formals(). That is necessary and it is not
# sufficient: during the round-1 review repair, four behaviours of published
# entry points changed while every formal stayed byte-identical --
# `cluster_target` became mandatory, ordinal `supports` became mandatory,
# calibrated clustered intervals began refusing G < 22, and clustered
# `n_complete` changed meaning. G-2 passed throughout, because a default value
# in a signature says nothing about whether the function honours it.
#
# These tests pin BEHAVIOUR for a canonical call set: which calls succeed,
# which raise, and what the published object reports. A future silent change
# of the same kind fails here instead of slipping through.
# =============================================================================

bin_data <- function() srs
clu_data <- function() {
  items <- paste0("wb", 1:6)
  list(x = classroom_wb[, items],
       supports = bm_supports(classroom_wb, items = items),
       cluster = as.character(classroom_wb$class_id))
}

test_that("the call form printed in Paper A still runs unchanged", {
  fit <- bb_mokken_h(bin_data(), draws = 200, seed = 1,
                     interval = "calibrated", level = 0.95)
  expect_s3_class(fit$interval, "bm_h_interval")
  expect_identical(fit$interval$method_id, "P7-FS-WIDTH-RAW-G025-v1")
})

test_that("the call form printed in Paper B v3 still runs, as the historical lane", {
  d <- clu_data()
  # The earlier version's printed call declares the two-stage law explicitly.
  # It must keep running and keep producing that version's identifiers; what
  # changed in 0.10.0 is that it now says so with a typed warning.
  expect_warning(
    fit <- bb_mokken_h(d$x, item_type = "ordinal", supports = d$supports,
                       cluster = d$cluster, cluster_target = "respondent",
                       weight_law = "hierarchical_cluster",
                       draws = 200, seed = 1, interval = "calibrated"),
    class = "bm_historical_method_warning"
  )
  expect_identical(fit$interval$method_id, "V4-P15-HBB-RESP-GN150-v1")
  expect_identical(fit$interval$construction, "midpoint_halfwidth_scaling")
})

test_that("the call form of the clustered article's current version runs on the default lane", {
  d <- clu_data()
  expect_no_warning(
    fit <- bb_mokken_h(d$x, item_type = "ordinal", supports = d$supports,
                       cluster = d$cluster, cluster_target = "respondent",
                       draws = 200, seed = 1, warn = FALSE)
  )
  expect_identical(fit$interval$method_id, "V5-C2-CRBB-ADJUSTED-RESP-v1")
  expect_identical(fit$sampling$weight_law, "complete_record")
  explicit <- bb_mokken_h(d$x, item_type = "ordinal", supports = d$supports,
                          cluster = d$cluster, cluster_target = "respondent",
                          weight_law = "complete_record",
                          draws = 200, seed = 1, warn = FALSE)
  expect_identical(explicit$draws$scale, fit$draws$scale)
  expect_identical(explicit$interval, fit$interval)
})

test_that("declarations the articles call mandatory are refused when omitted", {
  d <- clu_data()
  # cluster_target: silently defaulting would choose a population target for
  # the user, which both articles say the software must not do.
  expect_error(
    mokken_h(d$x, item_type = "ordinal", supports = d$supports,
             cluster = d$cluster, weight_law = "hierarchical_cluster",
             warn = FALSE),
    class = "bm_input_error"
  )
  # supports: inferring them would compute the maximum covariance at a support
  # the user never declared.
  expect_error(
    mokken_h(d$x, item_type = "ordinal", cluster = d$cluster,
             cluster_target = "respondent",
             weight_law = "hierarchical_cluster", warn = FALSE),
    class = "bm_input_error"
  )
})

test_that("the two analysis paths cannot be crossed, as both articles state", {
  # Paper A: "the package enforces the separation by refusing to cross the two
  # paths (binary data with a cluster argument ... produce validation errors)".
  # Paper B states the same. Every route into the clustered machinery with
  # dichotomous items must raise, not only the calibrated interval.
  cl <- rep(1:25, length.out = nrow(srs))
  sup2 <- stats::setNames(rep(list(0:1), ncol(srs)), names(srs))
  routes <- list(
    quote(mokken_h(srs, cluster = cl, cluster_target = "respondent",
                   warn = FALSE)),
    quote(mokken_h(srs, item_type = "ordinal", supports = sup2, cluster = cl,
                   cluster_target = "respondent",
                   weight_law = "hierarchical_cluster", warn = FALSE)),
    quote(bb_mokken_h(srs, item_type = "ordinal", supports = sup2,
                      cluster = cl, cluster_target = "respondent",
                      weight_law = "hierarchical_cluster",
                      draws = 200, seed = 1, warn = FALSE)),
    quote(bb_mokken_h(srs, item_type = "ordinal", supports = sup2,
                      cluster = cl, cluster_target = "respondent",
                      weight_law = "hierarchical_cluster",
                      interval = "equal_tail", draws = 200, seed = 1,
                      warn = FALSE))
  )
  for (r in routes) {
    expect_error(eval(r), class = "bm_input_error", info = deparse(r)[1])
  }
})

test_that("the historical clustered interval refuses its unvalidated range; the default does not", {
  d <- clu_data()
  keep <- classroom_wb$class_id %in% levels(classroom_wb$class_id)[1:10]
  small <- classroom_wb[keep, ]
  args <- list(small[, paste0("wb", 1:6)], item_type = "ordinal",
               supports = d$supports,
               cluster = as.character(droplevels(small$class_id)),
               cluster_target = "respondent",
               weight_law = "hierarchical_cluster",
               draws = 200, seed = 1, warn = FALSE)
  quiet <- function(expr) {
    suppressWarnings(expr, classes = "bm_historical_method_warning")
  }
  # G = 10 is below the two-stage calibration's confirmed range: that lane
  # must refuse. The package separates SCOPE errors (wrong kind of data, e.g.
  # path crossing) from RANGE errors (right kind of data, cluster count
  # outside the confirmed grid); this is the latter.
  expect_error(quiet(do.call(bb_mokken_h, c(args, list(interval = "calibrated")))),
               class = "bm_validation_range_error")
  expect_s3_class(do.call(mokken_h, args[1:6]), "bm_empirical_h")
  raw <- quiet(do.call(bb_mokken_h, c(args, list(interval = "equal_tail"))))
  expect_identical(raw$interval$validation_status, "NOT_APPLICABLE_EQUAL_TAIL")
  # The complete-record lane is computable for G >= 2 and reports its
  # confirmed range as metadata instead of refusing.
  default_args <- args
  default_args$weight_law <- NULL
  expect_no_warning(fit <- do.call(bb_mokken_h, default_args))
  expect_identical(fit$interval$status, "OK")
  expect_identical(fit$interval$cluster_count, 10L)
  expect_identical(fit$interval$validation_status,
                   "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
})

test_that("published objects report the quantities their names claim", {
  d <- clu_data()
  fit <- bb_mokken_h(d$x, item_type = "ordinal", supports = d$supports,
                     cluster = d$cluster, cluster_target = "respondent",
                     draws = 200, seed = 1, warn = FALSE)
  # n_complete counts respondents; the cluster count has its own field.
  expect_identical(fit$interval$n_complete, nrow(d$x))
  expect_identical(fit$interval$cluster_count, 30L)
  expect_identical(fit$interval$calibration_n, 30L)
  expect_false(identical(fit$interval$n_complete, fit$interval$cluster_count))
})

test_that("the frozen level and calibration constants stay unreachable", {
  expect_error(bb_mokken_h(bin_data(), draws = 200, seed = 1, level = 0.90),
               class = "bm_input_error")
  banned <- c("gamma", "kappa", "level_override", "allow_unvalidated")
  for (fn in c("mokken_h", "bb_mokken_h")) {
    expect_length(
      intersect(names(formals(getExportedValue("bayesmokken", fn))), banned),
      0L
    )
  }
})
