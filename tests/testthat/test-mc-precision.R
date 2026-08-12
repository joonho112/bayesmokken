.mc_test_data <- data.frame(
  a = c(0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0),
  b = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0),
  c = c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0)
)

test_that("MC diagnostics expose endpoint numerical error only", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 240, seed = 900,
    warn = FALSE
  )
  d <- fit$mc_diagnostics
  expect_equal(d$total_draws, 240)
  expect_equal(d$defined_fraction, 1)
  expect_gte(d$lower_endpoint_mcse, 0)
  expect_gte(d$upper_endpoint_mcse, 0)
  expect_true(is.finite(d$interval_batch_max_deviation))
  expect_true(is.finite(d$prefix_half_endpoint_max_difference))
  expect_false("probability" %in% names(d))
  expect_match(d$contract, "not interval-calibration")
  expect_identical(d$status, "OK")
})

test_that("endpoint diagnostics honor requested batch count", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 180, seed = 901, warn = FALSE
  )
  d <- bayesmokken:::bm_mc_diagnostics(fit, level = 0.95, batches = 3)
  expect_true(is.finite(d$lower_endpoint_mcse))
  expect_true(is.finite(d$upper_endpoint_mcse))
  expect_equal(d$batches, 3)
  expect_equal(d$method_id, "P7-FS-WIDTH-RAW-G025-v1")
  expect_identical(d$status, "OK")
})

test_that("all-undefined draws produce explicit NA diagnostics", {
  x <- data.frame(a = rep(0, 5), b = c(0, 1, 0, 1, 1))
  fit <- suppressWarnings(
    bb_mokken_h(x, draws = 30, seed = 902)
  )
  d <- fit$mc_diagnostics
  expect_equal(d$defined_draws, 0)
  expect_equal(d$defined_fraction, 0)
  expect_true(is.na(d$lower_endpoint_mcse))
  expect_true(is.na(d$upper_endpoint_mcse))
  expect_identical(d$status, "ALL_DRAWS_UNDEFINED")
  expect_identical(d$batches, 4L)
  expect_identical(d$quantile_type, 8L)
  expect_identical(d$method_id, fit$interval$method_id)
  expect_match(d$contract, "not interval-calibration")
})

test_that("every non-error state has one exact schema and field types", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 40, seed = 903, warn = FALSE
  )
  normal <- bayesmokken:::bm_mc_diagnostics(fit)

  partial_fit <- fit
  partial_fit$draws$scale[1L] <- NA_real_
  partial <- bayesmokken:::bm_mc_diagnostics(partial_fit)

  all_undefined_fit <- fit
  all_undefined_fit$draws$scale[] <- NA_real_
  all_undefined <- bayesmokken:::bm_mc_diagnostics(all_undefined_fit)

  too_short_fit <- fit
  too_short_fit$draws$scale[] <- NA_real_
  too_short_fit$draws$scale[seq_len(3L)] <- c(0.1, 0.2, 0.3)
  too_short <- bayesmokken:::bm_mc_diagnostics(
    too_short_fit, batches = 4L
  )

  expected_names <- c(
    "defined_draws", "total_draws", "defined_fraction", "level",
    "lower_endpoint_mcse", "upper_endpoint_mcse",
    "interval_batch_max_deviation",
    "prefix_half_endpoint_max_difference", "batches", "quantile_type",
    "method_id", "contract", "status"
  )
  expected_types <- c(
    defined_draws = "integer",
    total_draws = "integer",
    defined_fraction = "double",
    level = "double",
    lower_endpoint_mcse = "double",
    upper_endpoint_mcse = "double",
    interval_batch_max_deviation = "double",
    prefix_half_endpoint_max_difference = "double",
    batches = "integer",
    quantile_type = "integer",
    method_id = "character",
    contract = "character",
    status = "character"
  )
  for (diagnostic in list(normal, partial, all_undefined, too_short)) {
    expect_identical(names(diagnostic), expected_names)
    expect_identical(vapply(diagnostic, typeof, character(1L)), expected_types)
  }
  expect_identical(partial$status, "OK_WITH_UNDEFINED_DRAWS")
  expect_identical(too_short$status, "TOO_FEW_DEFINED_DRAWS")
  expect_identical(too_short$defined_draws, 3L)
  expect_identical(too_short$batches, 4L)
  expect_true(all(vapply(
    too_short[c(
      "lower_endpoint_mcse", "upper_endpoint_mcse",
      "interval_batch_max_deviation",
      "prefix_half_endpoint_max_difference"
    )],
    is.na,
    logical(1L)
  )))
})

test_that("invalid MC diagnostic arguments fail with typed errors", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 40, seed = 904, warn = FALSE
  )
  invalid_batches <- list(
    1L, 1.5, NA_real_, NaN, Inf, "4", c(2L, 3L)
  )
  for (value in invalid_batches) {
    expect_error(
      bayesmokken:::bm_mc_diagnostics(fit, batches = value),
      class = "bm_input_error"
    )
  }
  expect_error(
    bayesmokken:::bm_mc_diagnostics(fit, batchess = 4L),
    "Unused argument.*`batchess`",
    class = "bm_input_error"
  )
  expect_error(
    bayesmokken:::bm_mc_diagnostics(fit, level = 0.90),
    class = "bm_input_error"
  )
  expect_error(
    bayesmokken:::bm_mc_diagnostics(list()),
    class = "bm_input_error"
  )
})

test_that("finite draws fewer than batches never trigger a silent fallback", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 40, seed = 905, warn = FALSE
  )
  fit$draws$scale[] <- NA_real_
  fit$draws$scale[seq_len(3L)] <- c(0.1, 0.2, 0.3)

  diagnostic <- bayesmokken:::bm_mc_diagnostics(fit, batches = 12L)
  expect_identical(diagnostic$status, "TOO_FEW_DEFINED_DRAWS")
  expect_identical(diagnostic$batches, 12L)
  expect_identical(diagnostic$defined_draws, 3L)
})

test_that("bm_diagnostics propagates invalid MC arguments", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 40, seed = 906, warn = FALSE
  )
  expect_error(
    bm_diagnostics(fit, batches = 1L),
    class = "bm_input_error"
  )
  expect_error(
    bm_diagnostics(fit, batchess = 4L),
    "Unused argument.*`batchess`",
    class = "bm_input_error"
  )
  expect_error(
    bm_diagnostics(mokken_h(.mc_test_data, warn = FALSE), batches = 4L),
    "bootstrap fits only",
    class = "bm_input_error"
  )
})

test_that("bm_diagnostics never turns an unavailable MC result into reassurance", {
  fit <- bb_mokken_h(
    .mc_test_data, draws = 40, seed = 907, warn = FALSE
  )
  fit$draws$scale[] <- NA_real_
  fit$draws$scale[seq_len(3L)] <- c(0.1, 0.2, 0.3)
  diagnostic <- bm_diagnostics(fit, batches = 4L)
  expect_identical(diagnostic$mc$status, "TOO_FEW_DEFINED_DRAWS")
  expect_match(diagnostic$overall, "only 3 finite draw")
  expect_false(grepl("Nothing numerically awkward found", diagnostic$overall))
})
