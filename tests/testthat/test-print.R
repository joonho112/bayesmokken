# =============================================================================
# Console output
#
# Snapshots are taken with the width and locale pinned, so a failure means the
# output really changed rather than the machine did.
# =============================================================================

local_console <- function(width = 80L, env = parent.frame()) {
  withr::local_options(list(width = width, digits = 7L), .local_envir = env)
}

capture_cli_output <- function(expr) {
  out <- utils::capture.output(force(expr), type = "message")
  paste(cli::ansi_strip(out), collapse = "\n")
}

normalize_console <- function(x) {
  trimws(gsub("[[:space:]]+", " ", paste(x, collapse = " ")))
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

# --- snapshots ----------------------------------------------------------------

test_that("binary fit prints as designed", {
  skip_if_not_installed("withr")
  local_console()
  expect_snapshot(print(bin_fit()))
})

test_that("clustered fit prints as designed", {
  skip_if_not_installed("withr")
  local_console()
  expect_snapshot(print(clu_fit()))
})

test_that("historical two-stage fit prints as designed", {
  skip_if_not_installed("withr")
  local_console()
  expect_snapshot(print(clu_two_stage_fit()))
  expect_snapshot(print(clu_two_stage_fit()$interval))
})

test_that("deterministic fit prints as designed", {
  skip_if_not_installed("withr")
  local_console()
  expect_snapshot(print(mokken_h(srs, warn = FALSE)))
})

test_that("interval, data check, cluster info and diagnostics print", {
  skip_if_not_installed("withr")
  local_console()
  expect_snapshot(print(bin_fit()$interval))
  expect_snapshot(print(bm_check_data(srs)))
  expect_snapshot(print(bm_cluster_info(
    classroom_wb$class_id, data = classroom_wb[, paste0("wb", 1:6)])))
  expect_snapshot(print(bm_diagnostics(bin_fit())))
  expect_snapshot(
    print(bm_benchmark(bin_fit())),
    transform = function(lines) sub("[[:blank:]]+$", "", lines)
  )
})

test_that("field labels have an explicit separator at widths 40 and 80", {
  skip_if_not_installed("withr")
  expect_snapshot({
    for (w in c(40L, 80L)) {
      withr::local_options(list(width = w))
      .bm_header(sprintf("Field separator, width %d", w))
      .bm_field("rows / items", "337 x 11")
      .bm_field("complete-scale", "614 of 614 complete")
      .bm_field("target weight", "respondent")
    }
  })
})

# --- properties that must hold whatever the numbers -----------------------------

test_that("every print method includes a closing navigation block", {
  local_console()
  for (obj in list(bin_fit(), mokken_h(srs, warn = FALSE),
                   bm_check_data(srs), bm_diagnostics(bin_fit()),
                   bm_cluster_info(classroom_wb$class_id))) {
    out <- utils::capture.output(print(obj))
    expect_true(any(grepl("^[[:space:]]*(See:|Next:)", out)),
                info = paste(class(obj)[1], collapse = ""))
  }
})

test_that("both interval endpoints and the multiplier are always shown", {
  local_console()
  for (fit in list(bin_fit(), clu_fit())) {
    out <- paste(utils::capture.output(print(fit)), collapse = "\n")
    expect_match(out, "raw")
    expect_match(out, "width x")
    expect_match(out, "95% CI")
  }
})

test_that("clustered prints separate numerical and validation status", {
  local_console()
  fit <- clu_fit()
  for (obj in list(fit, summary(fit), fit$interval)) {
    out <- paste(utils::capture.output(print(obj)), collapse = "\n")
    expect_match(out, "status: OK|status  OK|CONFIRMED_CLUSTER_COUNT")
    expect_match(out, "CONFIRMED_CLUSTER_COUNT")
    expect_match(out, "22-320")
    expect_match(out, "COMPLETE_RECORD")
  }
  historical <- clu_two_stage_fit()
  for (obj in list(historical, summary(historical), historical$interval)) {
    out <- paste(utils::capture.output(print(obj)), collapse = "\n")
    expect_match(out, "22-80")
    expect_match(out, "HIERARCHICAL")
  }
})

test_that("the binary correction widens, the adjustment expands, the historical shrinks", {
  local_console()
  b <- paste(utils::capture.output(print(bin_fit())), collapse = "\n")
  cl <- paste(utils::capture.output(print(clu_fit())), collapse = "\n")
  hs <- paste(utils::capture.output(print(clu_two_stage_fit())), collapse = "\n")
  b_mult <- as.numeric(sub(".*width x ([0-9.]+).*", "\\1", b))
  c_mult <- as.numeric(sub(".*width x ([0-9.]+).*", "\\1", cl))
  h_mult <- as.numeric(sub(".*width x ([0-9.]+).*", "\\1", hs))
  expect_gt(b_mult, 1)
  expect_gt(c_mult, 1)
  expect_match(cl, "reflected about H", fixed = TRUE)
  expect_match(cl, "complete-record Bayesian bootstrap", fixed = TRUE)
  expect_lt(h_mult, 1)
  expect_match(hs, "historical", fixed = TRUE)
})

test_that("item intervals are absent by default and cautioned when requested", {
  local_console()
  out <- paste(utils::capture.output(print(summary(bin_fit()))),
               collapse = " ")
  expect_false(grepl("uncalibrated", out, ignore.case = TRUE))

  skip_if_not_installed("cli")
  default <- capture_cli_output(format_bm_items_cli(bin_fit()))
  requested <- capture_cli_output(format_bm_items_cli(
    bin_fit(), include_interval = TRUE
  ))
  expect_false(grepl("uncalibrated", default, ignore.case = TRUE))
  expect_match(requested, "uncalibrated", ignore.case = TRUE)
  expect_match(default, "ascending coefficient order", ignore.case = TRUE)
  expect_match(default, "declared fixed scale", ignore.case = TRUE)
})

test_that("no print method emits a verdict about the scale", {
  local_console()
  for (obj in list(bin_fit(), summary(bin_fit()), bm_diagnostics(bin_fit()),
                   bm_benchmark(bin_fit()))) {
    out <- tolower(paste(utils::capture.output(print(obj)), collapse = " "))
    for (word in c("\\bpass\\b", "\\bfail\\b", "significant",
                   "the scale is (valid|acceptable)")) {
      expect_false(grepl(word, out), info = paste(class(obj)[1], word))
    }
  }
})

test_that("output respects the console width", {
  for (w in c(60L, 80L, 100L)) {
    withr::local_options(list(width = w))
    out <- utils::capture.output(print(bin_fit()))
    # The rule line is built to the width; body lines wrap under it.
    expect_lte(max(nchar(out)), max(w, 100L) + 2L)
  }
})

test_that("ASCII fallback produces no box-drawing characters", {
  # .bm_rule_char() picks the ASCII rule when the locale is not UTF-8; force
  # the branch directly rather than manipulating the session locale.
  local_mocked_bindings(.bm_rule_char = function() "-")
  out <- paste(utils::capture.output(print(bin_fit())), collapse = "")
  expect_false(grepl("\u2500", out))
})

test_that("UTF-8 console separators are assembled at runtime", {
  utf8 <- tryCatch(isTRUE(l10n_info()$`UTF-8`), error = function(e) FALSE)
  expect_identical(.bm_rule_char(), if (utf8) "\u2500" else "-")
  expect_identical(.bm_dot_separator(), " \u00b7 ")
  expect_identical(.bm_dot_join("left", "right"), "left \u00b7 right")
})

# --- cli formatters -------------------------------------------------------------

test_that("cli formatters work when cli is installed", {
  skip_if_not_installed("cli")
  expect_invisible(format_bm_fit_cli(bin_fit()))
  expect_invisible(format_bm_interval_cli(bin_fit()))
  expect_invisible(format_bm_items_cli(bin_fit()))
})

test_that("base and cli fit output share the scientific semantic lines", {
  skip_if_not_installed("cli")
  local_console()
  fits <- list(bin_fit(), clu_fit(), mokken_h(srs, warn = FALSE))
  for (fit in fits) {
    semantics <- .bm_fit_semantic_lines(fit)
    base <- normalize_console(utils::capture.output(print(fit)))
    rich <- normalize_console(capture_cli_output(format_bm_fit_cli(fit)))
    shared <- unlist(semantics[c(
      "numerical_status", "validation_status", "validation_scope",
      "validation_range", "interpretation", "navigation"
    )], use.names = FALSE)
    for (line in shared[nzchar(shared)]) {
      expect_match(base, line, fixed = TRUE)
      expect_match(rich, line, fixed = TRUE)
    }
  }

  deterministic <- capture_cli_output(format_bm_fit_cli(
    mokken_h(srs, warn = FALSE)
  ))
  expect_false(grepl("Bayesian-bootstrap", deterministic, fixed = TRUE))
})

test_that("cli interval output preserves interpretation and validation scope", {
  skip_if_not_installed("cli")
  fit <- clu_fit()
  semantics <- .bm_interval_semantic_lines(fit$interval)
  base <- normalize_console(utils::capture.output(print(fit$interval)))
  rich <- normalize_console(capture_cli_output(format_bm_interval_cli(fit)))
  expected <- unlist(semantics[c(
    "numerical_status", "validation_status", "validation_scope",
    "validation_range", "interpretation", "navigation"
  )], use.names = FALSE)
  for (line in expected[nzchar(expected)]) {
    expect_match(base, line, fixed = TRUE)
    expect_match(rich, line, fixed = TRUE)
  }
  expect_match(base, "See:", fixed = TRUE)
  expect_match(rich, "See:", fixed = TRUE)
})

test_that("the default print methods never call cli", {
  # INV-8: the package must be fully usable with nothing beyond its Imports.
  # Read the installed function bodies rather than source files, so this holds
  # wherever the tests run.
  ns <- asNamespace("bayesmokken")
  printers <- grep("^(print|summary|format)\\.", ls(ns, all.names = TRUE),
                   value = TRUE)
  printers <- c(printers, ".bm_header", ".bm_field", ".bm_para", ".bm_see",
                ".bm_num", ".bm_ci", ".bm_sparkline")
  for (fn in printers) {
    obj <- get(fn, envir = ns)
    if (!is.function(obj)) next
    body_txt <- paste(deparse(obj), collapse = " ")
    expect_false(grepl("\\bcli\\b", body_txt), info = fn)
  }
})
