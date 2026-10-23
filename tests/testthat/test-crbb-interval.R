# =============================================================================
# The adjusted complete-record interval: arithmetic and failure semantics
#
# Every draw vector below goes through the package constructor and through the
# vendored copy of the archive's arithmetic (helper-archive-interval.R). The
# two must agree exactly, status by status, because the article names the
# archive implementation as the reference for this procedure.
# =============================================================================

crbb_interval <- function(draws, point, G, interval = "calibrated",
                          target = "respondent", point_defined = TRUE) {
  bayesmokken:::.bm_crbb_interval_from_draws(
    bb_draws = draws, point_estimate = point, n_complete = 100L,
    cluster_count = G, cluster_target = target, interval = interval,
    level = 0.95, point_defined = point_defined
  )
}

expect_archive_agreement <- function(draws, point, G) {
  ours <- crbb_interval(draws, point, G)
  theirs <- .archive_candidate_interval(draws, point, G,
                                        "one_stage_basic_G_adjusted")
  expect_identical(ours$root_multiplier, theirs$root_multiplier)
  expect_identical(ours$width_multiplier, theirs$root_multiplier)
  expect_identical(ours$defined_draws, theirs$defined_draws)
  expect_identical(ours$total_draws, theirs$total_draws)
  expect_identical(ours$boundary_draws, theirs$boundary_draws)
  expect_identical(ours$bound_violation_draws, theirs$bound_violation_draws)
  expect_identical(ours$point_boundary, theirs$point_boundary)
  expect_identical(ours$point_bound_violation, theirs$point_bound_violation)
  if (theirs$success) {
    expect_true(ours$status %in% c("OK", "OK_WITH_UNDEFINED_DRAWS"))
    expect_identical(ours$raw_lower, theirs$quantile_lower)
    expect_identical(ours$raw_upper, theirs$quantile_upper)
    expect_identical(ours$reflected_lower, theirs$lower_raw)
    expect_identical(ours$reflected_upper, theirs$upper_raw)
    expect_identical(ours$lower, theirs$lower)
    expect_identical(ours$upper, theirs$upper)
    expect_identical(ours$upper_intersection, theirs$upper_intersection)
    expect_identical(ours$zero_width, theirs$zero_width)
    expect_false(ours$set_empty)
  } else {
    expect_false(ours$status %in% c("OK", "OK_WITH_UNDEFINED_DRAWS"))
    expect_true(is.na(ours$lower) && is.na(ours$upper))
    if (identical(theirs$status, "EMPTY_PARAMETER_INTERSECTION")) {
      expect_identical(ours$status, "EMPTY_PARAMETER_INTERSECTION")
      expect_true(ours$set_empty)
      expect_identical(ours$reflected_lower, theirs$lower_raw)
      expect_identical(ours$reflected_upper, theirs$upper_raw)
    }
  }
  invisible(list(ours = ours, theirs = theirs))
}

test_that("k_G equals the archive's root multiplier at the documented counts", {
  k <- bayesmokken:::.bm_cluster_k_multiplier
  for (G in c(22L, 30L, 80L, 320L)) {
    expect_identical(
      k(G),
      sqrt((G + 1) / (G - 1)) * stats::qt(.975, df = G - 1) / stats::qnorm(.975)
    )
  }
  expect_equal(k(22L), 1.1104, tolerance = 5e-5)
  expect_equal(k(30L), 1.0789, tolerance = 5e-5)
  expect_equal(k(80L), 1.0283, tolerance = 5e-5)
  expect_equal(k(320L), 1.0070, tolerance = 5e-5)
  expect_equal(k(30L), 1.07888669019652, tolerance = 1e-14)
  expect_true(all(diff(k(c(2L, 5L, 10L, 50L, 1000L))) < 0))
  expect_gt(k(100000L), 1)
})

test_that("the package arithmetic agrees exactly with the archive's on regular draws", {
  set.seed(4201)
  for (G in c(2L, 5L, 22L, 30L, 320L)) {
    for (rep in 1:5) {
      point <- stats::runif(1, 0.2, 0.8)
      draws <- point + stats::rnorm(999, sd = 0.03)
      out <- expect_archive_agreement(draws, point, G)
      expect_identical(out$ours$status, "OK")
      # The width before the cap is exactly k_G times the percentile width.
      expect_equal(out$ours$reflected_upper - out$ours$reflected_lower,
                   out$ours$width_multiplier *
                     (out$ours$raw_upper - out$ours$raw_lower),
                   tolerance = 1e-12)
    }
  }
})

test_that("the reflection is about the point estimate, not the midpoint", {
  draws <- c(seq(0.40, 0.55, length.out = 150), seq(0.55, 0.80, length.out = 50))
  point <- 0.50
  itv <- crbb_interval(draws, point, 30L)
  k <- itv$width_multiplier
  expect_equal(itv$reflected_lower, point - k * (itv$raw_upper - point))
  expect_equal(itv$reflected_upper, point - k * (itv$raw_lower - point))
  # Midpoint scaling would give a different interval here.
  mid <- (itv$raw_lower + itv$raw_upper) / 2
  half <- (itv$raw_upper - itv$raw_lower) / 2
  expect_false(isTRUE(all.equal(itv$lower, mid - k * half)))
  expect_identical(itv$construction, "reflected_error_expansion_about_point")
  expect_identical(itv$method_id, "V5-C2-CRBB-ADJUSTED-RESP-v1")
  expect_identical(itv$base_method_id, "V5-CMP-CRBB-PERCENTILE-RESP-v1")
  expect_identical(itv$archive_method_id, "one_stage_basic_G_adjusted")
  expect_identical(itv$protocol_id, "C2")
  expect_true(is.na(itv$gamma))
  expect_identical(itv$endpoint_clipping, "UPPER_PARAMETER_BOUND_ONE")
  expect_identical(itv$per_replication_method_cascade, "FORBIDDEN")
  expect_identical(itv$quantile_type, 8L)
  expect_identical(itv$validated_cluster_min, 22L)
  expect_identical(itv$validated_cluster_max, 320L)
  expect_identical(itv$validation_scope, "POLYTOMOUS_CLUSTERED_COMPLETE_RECORD_BB")
  equal <- crbb_interval(draws, point, 30L, target = "equal_cluster")
  expect_identical(equal$method_id, "V5-C2-CRBB-ADJUSTED-CLUSTER-v1")
})

test_that("the percentile comparator is the archive's percentile interval", {
  set.seed(4202)
  draws <- 0.5 + stats::rnorm(2000, sd = 0.05)
  draws[c(3, 77)] <- NA_real_
  ours <- crbb_interval(draws, 0.5, 30L, interval = "equal_tail")
  theirs <- .archive_percentile(draws)
  # The archive's comparator derives its probabilities from the level,
  # (1 - .95) / 2, which differs from the literal .025 in the last bit; the
  # package uses the literal for both constructions so that the comparator is
  # exactly the raw interval of the adjusted construction. Agreement with the
  # archive's comparator is therefore to the last bit, not bit for bit.
  expect_equal(ours$lower, theirs$lower, tolerance = 1e-14)
  expect_equal(ours$upper, theirs$upper, tolerance = 1e-14)
  adjusted <- crbb_interval(draws, 0.5, 30L)
  expect_identical(ours$lower, adjusted$raw_lower)
  expect_identical(ours$upper, adjusted$raw_upper)
  expect_identical(ours$defined_draws, as.integer(theirs$defined_draws))
  expect_identical(ours$status, "OK_WITH_UNDEFINED_DRAWS")
  expect_identical(ours$construction, "percentile")
  expect_identical(ours$width_multiplier, 1)
  expect_identical(ours$method_id, "V5-CMP-CRBB-PERCENTILE-RESP-v1")
  expect_identical(ours$archive_method_id, "one_stage_bb")
  expect_identical(ours$validation_status, "NOT_APPLICABLE_EQUAL_TAIL")
  expect_identical(ours$endpoint_clipping, "NONE")
  expect_true(is.na(ours$reflected_lower))
})

test_that("failure semantics are typed, never substituted, and match the archive", {
  base <- 0.5 + seq(-0.1, 0.1, length.out = 200)

  undefined_point <- crbb_interval(base, NA_real_, 30L)
  expect_identical(undefined_point$status, "UNDEFINED_POINT_ESTIMATE")
  expect_identical(.archive_candidate_interval(base, NA_real_, 30L,
                   "one_stage_basic_G_adjusted")$status, "POINT_UNDEFINED")
  expect_true(all(is.na(c(undefined_point$lower, undefined_point$upper,
                          undefined_point$raw_lower, undefined_point$raw_upper))))
  # A point that is finite but typed undefined by the 1e-12 rule.
  expect_identical(crbb_interval(base, 0.5, 30L, point_defined = FALSE)$status,
                   "UNDEFINED_POINT_ESTIMATE")

  over <- crbb_interval(base, 1.2, 30L)
  expect_identical(over$status, "POINT_UPPER_BOUND_VIOLATION")
  expect_true(over$point_bound_violation)
  expect_archive_agreement(base, 1.2, 30L)

  few <- base
  few[1:5] <- NA_real_                        # 195 < ceiling(.99 * 200) = 198
  expect_identical(crbb_interval(few, 0.5, 30L)$status,
                   "INSUFFICIENT_DEFINED_BB_DRAWS")
  expect_identical(crbb_interval(few, 0.5, 30L)$minimum_defined_draws, 198L)
  expect_archive_agreement(few, 0.5, 30L)
  expect_identical(crbb_interval(rep(0.5, 10), 0.5, 30L)$minimum_defined_draws,
                   20L)

  violated <- base
  violated[100] <- 1.0001
  expect_identical(crbb_interval(violated, 0.5, 30L)$status,
                   "DRAW_UPPER_BOUND_VIOLATION")
  expect_identical(crbb_interval(violated, 0.5, 30L)$bound_violation_draws, 1L)
  expect_archive_agreement(violated, 0.5, 30L)

  # Values exactly one are valid and flagged, not violations. With enough
  # draws at one the upper quantile is one, so the reflected lower endpoint
  # is exactly one and the reported set is the usable zero-width [1, 1].
  boundary <- c(rep(1, 100), seq(0.9, 1, length.out = 100))
  on_boundary <- crbb_interval(boundary, 1, 30L)
  expect_identical(on_boundary$status, "OK")
  expect_true(on_boundary$point_boundary)
  expect_identical(on_boundary$boundary_draws, 101L)
  expect_identical(on_boundary$bound_violation_draws, 0L)
  expect_true(on_boundary$upper_intersection)
  expect_true(on_boundary$zero_width)
  expect_identical(c(on_boundary$lower, on_boundary$upper), c(1, 1))
  expect_archive_agreement(boundary, 1, 30L)

  # Reflecting draws that sit far below the point pushes the reflected
  # interval above 1: a computed empty set, recorded as such, never [1, 1].
  low <- seq(0.1, 0.3, length.out = 200)
  empty <- crbb_interval(low, 0.95, 30L)
  expect_identical(empty$status, "EMPTY_PARAMETER_INTERSECTION")
  expect_true(empty$set_empty)
  expect_true(is.na(empty$lower) && is.na(empty$upper))
  expect_gt(empty$reflected_lower, 1)
  expect_archive_agreement(low, 0.95, 30L)

  # Only the upper endpoint crosses 1: intersect, flag, keep the raw value.
  high <- seq(0.90, 0.99, length.out = 200)
  capped <- crbb_interval(high, 0.95, 30L)
  expect_identical(capped$status, "OK")
  expect_true(capped$upper_intersection)
  expect_identical(capped$upper, 1)
  expect_gt(capped$reflected_upper, 1)
  expect_lt(capped$lower, 1)
  expect_archive_agreement(high, 0.95, 30L)

  # Negative coefficients and endpoints are retained.
  negative <- seq(-0.30, -0.10, length.out = 200)
  kept <- crbb_interval(negative, -0.2, 30L)
  expect_identical(kept$status, "OK")
  expect_lt(kept$lower, 0)
  expect_lt(kept$upper, 0)
  expect_archive_agreement(negative, -0.2, 30L)

  # Zero width is usable and flagged.
  flat <- rep(0.5, 200)
  zero <- crbb_interval(flat, 0.5, 30L)
  expect_identical(zero$status, "OK")
  expect_true(zero$zero_width)
  expect_identical(zero$lower, zero$upper)
  expect_archive_agreement(flat, 0.5, 30L)
})

test_that("G = 2 is computable and the confirmed range is reported, not enforced", {
  draws <- 0.5 + seq(-0.1, 0.1, length.out = 100)
  two <- crbb_interval(draws, 0.5, 2L)
  expect_identical(two$status, "OK")
  expect_identical(two$validation_status, "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
  expect_gt(two$width_multiplier, 10)         # t_{1,.975} is large
  expect_identical(crbb_interval(draws, 0.5, 22L)$validation_status,
                   "CONFIRMED_CLUSTER_COUNT")
  expect_identical(crbb_interval(draws, 0.5, 320L)$validation_status,
                   "CONFIRMED_CLUSTER_COUNT")
  expect_identical(crbb_interval(draws, 0.5, 321L)$validation_status,
                   "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
  expect_error(crbb_interval(draws, 0.5, 1L), class = "bm_input_error")
})

test_that("Monte Carlo diagnostics and the stability figure rebuild reflected endpoints", {
  block <- data.frame(
    a = c(0, 1, 2, 3, 0, 1, 3, 2),
    b = c(0, 1, 1, 3, 0, 2, 3, 2),
    c = c(0, 0, 2, 3, 1, 2, 3, 1)
  )
  x <- block[rep(seq_len(nrow(block)), times = 25L), , drop = FALSE]
  cluster <- rep(sprintf("g%02d", seq_len(25L)), each = nrow(block))
  fit <- bb_mokken_h(x, item_type = "ordinal",
                     supports = list(a = 0:3, b = 0:3, c = 0:3),
                     cluster = cluster, cluster_target = "respondent",
                     draws = 200, seed = 11, warn = FALSE)
  mc <- bm_mc_diagnostics(fit, batches = 4L)
  expect_identical(mc$status, "OK")
  values <- fit$draws$scale[is.finite(fit$draws$scale)]
  q <- stats::quantile(values, c(0.025, 0.975), names = FALSE, type = 8L)
  point <- fit$interval$point_estimate
  k <- fit$interval$width_multiplier
  # The prefix-half comparison uses the reflected construction.
  half <- stats::quantile(values[seq_len(length(values) %/% 2)],
                          c(0.025, 0.975), names = FALSE, type = 8L)
  expected <- max(abs(c(point - k * (half[2L] - point),
                        point - k * (half[1L] - point)) -
                      c(point - k * (q[2L] - point),
                        point - k * (q[1L] - point))))
  expect_equal(mc$prefix_half_endpoint_max_difference, expected,
               tolerance = 1e-12)
  p <- bm_plot_mc_stability(fit)
  last <- Filter(function(d) "shape" %in% names(d),
                 ggplot2::ggplot_build(p)$data)[[1L]]
  expect_equal(sort(last$y),
               sort(c(fit$interval$reflected_lower,
                      fit$interval$reflected_upper)),
               tolerance = 1e-9)
})
