test_that("public BB API executes the complete calibrated workflow", {
  raw <- data.frame(
    a = factor(c("no", "no", "yes", "yes", "yes", "no", "yes", "no")),
    b = c(0, 1, 1, 1, 0, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, NA)
  )
  fit <- bb_mokken_h(
    raw,
    score_map = list(a = c(no = 0, yes = 1)),
    row_ids = paste0("r", 1:8),
    draws = 120,
    seed = 55,
    batch_size = 16,
    warn = FALSE
  )
  expect_s3_class(fit, "bm_bb_h")
  expect_equal(fit$empirical$provenance$n_total, 8)
  expect_equal(fit$empirical$provenance$n_complete, 7)
  expect_equal(fit$empirical$provenance$excluded_row_ids, "r8")
  expect_equal(fit$summaries$scale_draws$total_draws, 120)
  expect_equal(fit$interval$method_id, "P7-FS-WIDTH-RAW-G025-v1")
  expect_equal(fit$interval$gamma, 0.25)
  expect_equal(fit$interval$quantile_type, 8L)
  expect_equal(fit$interval$endpoint_clipping, "NONE")
  expect_equal(fit$rng$seed, 55L)
  expect_equal(fit$grouping$n_respondents, 7)
})

test_that("only standard respondent law is exposed", {
  exports <- getNamespaceExports("bayesmokken")
  expect_true("bb_mokken_h" %in% exports)
  expect_true("bayes_mokken_h" %in% exports)
  expect_false("bm_generalized_h" %in% exports)
  expect_false("bm_predictive_h" %in% exports)
  expect_false("cluster_bayes_mokken_h" %in% exports)
})

test_that("BB API rejects unsupported methods, levels, and controls", {
  expect_false("weights" %in% names(formals(bb_mokken_h)))
  expect_false("threshold" %in% names(formals(bb_mokken_h)))
  x <- data.frame(a = c(0, 1, 0), b = c(0, 1, 1))
  expect_error(bb_mokken_h(x, draws = 1), "at least 2")
  expect_error(bb_mokken_h(x, draws = 10, batch_size = 0), "positive")
  expect_error(bb_mokken_h(x, draws = 10, workers = 0), "positive")
  expect_error(bb_mokken_h(x, interval = "basic"), "arg")
  expect_error(bb_mokken_h(x, level = 0.90), "exactly 0.95")
})

test_that("raw comparator and deprecated alias have bounded semantics", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0),
    b = c(0, 1, 1, 1, 0, 0),
    c = c(0, 0, 1, 0, 1, 1)
  )
  raw <- bb_mokken_h(
    x, draws = 120, seed = 21, interval = "equal_tail", warn = FALSE
  )
  expect_equal(raw$interval$method_id, "CMP-RAW-BB-EQUAL-TAIL-v1")
  expect_equal(raw$interval$width_multiplier, 1)
  expect_equal(raw$interval$lower, raw$interval$raw_lower)
  expect_equal(raw$interval$upper, raw$interval$raw_upper)

  expect_warning(
    legacy <- bayes_mokken_h(x, draws = 120, seed = 21, warn = FALSE),
    "deprecated"
  )
  expect_s3_class(legacy, "bm_bb_h")
  expect_false("threshold_probability" %in% names(legacy$summaries))
})
