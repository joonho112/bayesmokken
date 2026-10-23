# =============================================================================
# G-4  Frozen calibration
#
# INV-4: gamma (binary, .25) and kappa (clustered, 1.5) were fixed by a frozen
# train-validate-lock procedure, and coverage at any other value is simply
# unknown. Paper A states that the package "deliberately exposes no user-tunable
# gamma, no confidence level other than 95%". A user-facing knob would let
# someone report an interval no simulation ever checked.
# =============================================================================

x <- data.frame(
  a = c(0, 0, 1, 1, 1, 0, 1, 0), b = c(0, 1, 1, 1, 0, 0, 1, 0),
  c = c(0, 0, 1, 0, 1, 1, 1, 0), d = c(1, 0, 1, 1, 1, 0, 1, 0)
)

test_that("no calibration constant is exposed as an argument", {
  banned <- c("gamma", "kappa", "width", "width_multiplier", "shrinkage",
              "calibration", "tune", "gamma_cluster")
  for (fn in c("mokken_h", "bb_mokken_h", "bayes_mokken_h")) {
    args <- names(formals(getExportedValue("bayesmokken", fn)))
    expect_length(intersect(args, banned), 0L)
  }
})

test_that("level is fixed at exactly 0.95", {
  expect_error(bb_mokken_h(x, draws = 200, seed = 1, level = 0.90),
               class = "bm_input_error")
  expect_error(bb_mokken_h(x, draws = 200, seed = 1, level = 0.99),
               class = "bm_input_error")
  expect_error(bb_mokken_h(x, draws = 200, seed = 1, level = c(0.95, 0.95)),
               class = "bm_input_error")
  expect_no_error(bb_mokken_h(x, draws = 200, seed = 1, level = 0.95,
                              warn = FALSE))
})

test_that("interval methods are limited to the two frozen choices", {
  expect_identical(eval(formals(bb_mokken_h)$interval),
                   c("calibrated", "equal_tail"))
  expect_error(bb_mokken_h(x, draws = 200, seed = 1, interval = "bca"))
  expect_error(bb_mokken_h(x, draws = 200, seed = 1, interval = "percentile"))
})

test_that("the locked width multipliers follow their published formulas", {
  fit <- bb_mokken_h(x, draws = 300, seed = 1, warn = FALSE)
  n <- fit$interval$n_complete
  expect_equal(fit$interval$gamma, 0.25, tolerance = 0)
  expect_equal(fit$interval$width_multiplier, 1 + 0.25 / sqrt(n),
               tolerance = 0)
  # Calibration widens the raw interval about a fixed midpoint.
  expect_equal((fit$interval$lower + fit$interval$upper) / 2,
               (fit$interval$raw_lower + fit$interval$raw_upper) / 2)
  expect_gt(fit$interval$upper - fit$interval$lower,
            fit$interval$raw_upper - fit$interval$raw_lower)

  raw <- bb_mokken_h(x, draws = 300, seed = 1, interval = "equal_tail",
                     warn = FALSE)
  expect_equal(raw$interval$width_multiplier, 1, tolerance = 0)
  expect_equal(raw$interval$gamma, 0, tolerance = 0)
})

test_that("the historical clustered multiplier shrinks and uses -1.5/sqrt(G)", {
  itv <- bayesmokken:::.bm_cluster_interval_from_draws(
    bb_draws = seq(0.4, 0.8, length.out = 500), point_estimate = 0.6,
    n_complete = 614L,
    cluster_count = 30L, cluster_target = "respondent",
    interval = "calibrated", level = 0.95
  )
  expect_equal(itv$gamma, -1.5, tolerance = 0)
  expect_equal(itv$width_multiplier, 1 - 1.5 / sqrt(30), tolerance = 0)
  expect_lt(itv$width_multiplier, 1)
  expect_identical(itv$n_complete, 614L)
  expect_identical(itv$cluster_count, 30L)
  expect_identical(itv$calibration_n, 30L)
  expect_identical(itv$calibration_unit, "clusters")
  expect_identical(itv$validation_status, "CONFIRMED_CLUSTER_COUNT")
  expect_identical(itv$validated_cluster_min, 22L)
  expect_identical(itv$validated_cluster_max, 80L)
})

test_that("the complete-record multiplier is the prespecified k_G, not a tuned constant", {
  draws <- seq(0.4, 0.8, length.out = 500)
  for (G in c(22L, 30L, 80L, 320L)) {
    itv <- bayesmokken:::.bm_crbb_interval_from_draws(
      bb_draws = draws, point_estimate = 0.6, n_complete = 614L,
      cluster_count = G, cluster_target = "respondent",
      interval = "calibrated", level = 0.95
    )
    expect_identical(
      itv$width_multiplier,
      sqrt((G + 1) / (G - 1)) * stats::qt(0.975, G - 1) / stats::qnorm(0.975)
    )
    expect_identical(itv$root_multiplier, itv$width_multiplier)
    expect_gt(itv$width_multiplier, 1)
    expect_true(is.na(itv$gamma))
    expect_identical(itv$multiplier_formula,
                     "sqrt((G + 1) / (G - 1)) * qt(0.975, G - 1) / qnorm(0.975)")
  }
  # Reflection about the point estimate, not midpoint scaling.
  itv <- bayesmokken:::.bm_crbb_interval_from_draws(
    bb_draws = draws, point_estimate = 0.55, n_complete = 614L,
    cluster_count = 30L, cluster_target = "respondent"
  )
  k <- itv$width_multiplier
  expect_equal(itv$lower, 0.55 - k * (itv$raw_upper - 0.55))
  expect_equal(itv$upper, 0.55 - k * (itv$raw_lower - 0.55))
  expect_false(isTRUE(all.equal((itv$lower + itv$upper) / 2,
                                (itv$raw_lower + itv$raw_upper) / 2)))
})

test_that("frozen policy fields are unchanged", {
  fit <- bb_mokken_h(x, draws = 300, seed = 1, warn = FALSE)
  expect_identical(fit$interval$quantile_type, 8L)
  expect_identical(fit$interval$endpoint_clipping, "NONE")
  expect_identical(fit$interval$per_replication_method_cascade, "FORBIDDEN")
})
