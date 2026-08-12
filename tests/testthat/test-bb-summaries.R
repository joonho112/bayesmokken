.bb_summary_test_data <- data.frame(
  a = c(0, 0, 1, 1, 1, 0, 1, 0),
  b = c(0, 1, 1, 1, 0, 0, 1, 0),
  c = c(0, 0, 1, 0, 1, 1, 1, 0)
)

test_that("BB summaries report the calibrated CI without threshold output", {
  fit <- bb_mokken_h(
    .bb_summary_test_data, draws = 160, seed = 18, warn = FALSE
  )
  sm <- summary(fit)
  expect_s3_class(sm, "summary_bm_bb_h")
  expect_named(
    sm$scale_draws,
    c("variable", "mean", "median", "sd", "q025", "q975",
      "defined_draws", "total_draws", "defined_fraction")
  )
  expect_equal(sm$scale_draws$total_draws, 160)
  expect_s3_class(sm$interval, "bm_h_interval")
  expect_equal(sm$interval$method_id, "P7-FS-WIDTH-RAW-G025-v1")
  expect_false("threshold_probability" %in% names(sm))
  expect_false("admissibility" %in% names(sm))
  expect_match(sm$interpretation, "not a posterior probability")
  # v1 note: the console layout was redesigned in Phase 5 (log 006).
  # The STRUCTURE of the printed information is unchanged and the
  # method identifier is still shown; only the wording and layout moved.
  # The original assertion is kept beside the new one so the change is
  # visible in the diff rather than silently dropped.
  # was: expect_output(print(fit), "BB-based confidence interval")
  expect_output(print(fit), "Bayesian-bootstrap-based confidence interval")
  expect_output(print(fit), "not a posterior credible interval")
  expect_output(print(sm), "P7-FS-WIDTH-RAW-G025-v1")
})

test_that("pair summaries add coordinates without changing legacy columns", {
  x <- as.data.frame(expand.grid(rep(list(0:1), 4L)))
  names(x) <- c("a", "b::c", "a::b", "c")
  fit <- bb_mokken_h(x, draws = 80, seed = 746, warn = FALSE)
  pair <- fit$summaries$pair_draws
  legacy_columns <- c(
    "variable", "mean", "median", "sd", "q025", "q975",
    "defined_draws", "total_draws", "defined_fraction"
  )
  empirical <- fit$empirical$pair

  expect_identical(names(pair)[seq_along(legacy_columns)], legacy_columns)
  expect_identical(
    names(pair), c(legacy_columns, "item_i", "item_j")
  )
  expect_identical(pair$item_i, as.character(empirical$item_i))
  expect_identical(pair$item_j, as.character(empirical$item_j))
  expect_identical(
    pair$variable,
    bayesmokken:::.bm_pair_key(
      as.character(empirical$item_i),
      as.character(empirical$item_j)
    )
  )
  expect_equal(
    pair$mean,
    vapply(seq_len(ncol(fit$draws$pair)), function(k) {
      mean(fit$draws$pair[, k], na.rm = TRUE)
    }, numeric(1)),
    tolerance = 0
  )
  expect_identical(summary(fit)$pair_draws, pair)
})

test_that("calibrated endpoints are the exact frozen transform", {
  fit <- bb_mokken_h(
    .bb_summary_test_data, draws = 180, seed = 19, warn = FALSE
  )
  raw <- stats::quantile(
    fit$draws$scale,
    c(0.025, 0.975),
    names = FALSE,
    type = 8L
  )
  multiplier <- 1 + 0.25 / sqrt(nrow(.bb_summary_test_data))
  midpoint <- mean(raw)
  expected <- midpoint + c(-1, 1) * multiplier * diff(raw) / 2
  expect_equal(
    c(fit$interval$raw_lower, fit$interval$raw_upper),
    raw,
    tolerance = 1e-15
  )
  expect_equal(
    c(fit$interval$lower, fit$interval$upper),
    expected,
    tolerance = 1e-15
  )
  expect_equal(fit$interval$gamma, 0.25)
  expect_equal(fit$interval$endpoint_clipping, "NONE")
})

test_that("undefined draws are not silently discarded from diagnostics", {
  x <- data.frame(a = rep(0, 5), b = c(0, 1, 0, 1, 1))
  fit <- suppressWarnings(
    bb_mokken_h(x, draws = 25, seed = 5)
  )
  expect_equal(fit$summaries$scale_draws$defined_draws, 0)
  expect_equal(fit$summaries$scale_draws$total_draws, 25)
  expect_equal(fit$interval$status, "UNDEFINED_POINT_ESTIMATE")
  expect_true(is.na(fit$interval$lower))
  expect_equal(fit$interval$defined_draws, 0)
  expect_equal(fit$interval$total_draws, 25)
})
