reference_max_covariance <- function(x, px, y, py) {
  breakpoints <- sort(unique(c(0, cumsum(px), cumsum(py), 1)))
  width <- diff(breakpoints)
  midpoint <- head(breakpoints, -1L) + width / 2
  qx <- x[pmin(findInterval(midpoint, cumsum(px)) + 1L, length(x))]
  qy <- y[pmin(findInterval(midpoint, cumsum(py)) + 1L, length(y))]
  sum(width * qx * qy) - sum(x * px) * sum(y * py)
}

reference_ordinal_h <- function(x, weights, supports) {
  weights <- weights / sum(weights)
  means <- as.numeric(crossprod(weights, x))
  numerator <- 0
  denominator <- 0
  pair <- utils::combn(seq_len(ncol(x)), 2L)
  for (k in seq_len(ncol(pair))) {
    i <- pair[1L, k]
    j <- pair[2L, k]
    covariance <- sum(weights * x[, i] * x[, j]) -
      means[i] * means[j]
    mass_i <- vapply(
      supports[[i]], function(z) sum(weights[x[, i] == z]), numeric(1)
    )
    mass_j <- vapply(
      supports[[j]], function(z) sum(weights[x[, j] == z]), numeric(1)
    )
    numerator <- numerator + covariance
    denominator <- denominator + reference_max_covariance(
      supports[[i]], mass_i, supports[[j]], mass_j
    )
  }
  numerator / denominator
}

test_that("finite-support transport matches an independent quantile oracle", {
  set.seed(18021)
  for (iteration in seq_len(50L)) {
    x <- sort(runif(sample(2:6, 1L), -2, 4))
    y <- sort(runif(sample(2:6, 1L), -3, 5))
    px <- rexp(length(x))
    py <- rexp(length(y))
    px <- px / sum(px)
    py <- py / sum(py)
    observed <- bayesmokken:::bm_ordinal_max_covariance_transport(
      x, px, y, py
    )$maximum_covariance
    expected <- reference_max_covariance(x, px, y, py)
    expect_lt(abs(observed - expected), 2e-13)
  }
})

test_that("ordinal point H matches an independent formula reference", {
  x <- cbind(
    item_a = c(0, 1, 2, 3, 0, 2, 3, 1),
    item_b = c(-1, 0, 0, 2, -1, 2, 2, 0),
    item_c = c(0, 2, 4, 4, 0, 2, 4, 2)
  )
  supports <- list(
    item_a = 0:3,
    item_b = c(-1, 0, 2),
    item_c = c(0, 2, 4)
  )
  weights <- c(1, 2, 3, 4, 2, 1, 5, 2)
  fit <- mokken_h(
    x,
    item_type = "ordinal",
    supports = supports,
    weights = weights,
    warn = FALSE
  )
  expect_equal(
    fit$scale$H,
    reference_ordinal_h(x, weights, supports),
    tolerance = 2e-13
  )
  expect_identical(fit$estimand$item_type, "ordinal")
  expect_identical(fit$estimand$scale_aggregation, "ratio_of_sums")
  expect_equal(fit$provenance$supports, supports)
  expect_match(fit$estimand$pair_denominator, "comonotone")
})

test_that("ordinal binary-support specialization equals binary H", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0),
    b = c(0, 1, 1, 1, 0, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, 0)
  )
  binary <- mokken_h(x, warn = FALSE)
  ordinal <- mokken_h(
    x,
    item_type = "ordinal",
    supports = list(a = c(0, 1), b = c(0, 1), c = c(0, 1)),
    warn = FALSE
  )
  expect_equal(ordinal$pair$cov, binary$pair$cov, tolerance = 1e-15)
  expect_equal(
    ordinal$pair$cov_max,
    binary$pair$cov_max,
    tolerance = 1e-15
  )
  expect_equal(ordinal$item$H_i, binary$item$H_i, tolerance = 1e-15)
  expect_equal(ordinal$scale$H, binary$scale$H, tolerance = 1e-15)
})

test_that("ordinal score maps, missing filtering, and supports are explicit", {
  x <- data.frame(
    a = factor(c("low", "mid", "high", "missing", "high")),
    b = c(0, 1, 2, 1, 2),
    c = c(0, 0, 2, 1, 2)
  )
  fit <- mokken_h(
    x,
    item_type = "ordinal",
    score_map = list(a = c(low = 0, mid = 1, high = 2)),
    missing_codes = list(a = "missing"),
    supports = list(a = 0:2, b = 0:2, c = 0:2),
    warn = FALSE
  )
  expect_identical(fit$provenance$n_complete, 4L)
  expect_identical(fit$provenance$excluded_index, 4L)
  expect_true(nzchar(fit$provenance$support_hash))
  expect_true(is.list(fit$knot_diagnostics$pair))
})

test_that("arbitrary weights are invariant to a matched row permutation", {
  x <- data.frame(
    a = c(0, 1, 2, 3, 1, 0, 3),
    b = c(0, 1, 1, 3, 2, 0, 2),
    c = c(0, 0, 2, 3, 1, 1, 3)
  )
  weights <- c(1, 5, 2, 7, 3, 4, 6)
  order <- c(7, 2, 5, 1, 6, 4, 3)
  supports <- list(a = 0:3, b = 0:3, c = 0:3)
  before <- mokken_h(
    x, item_type = "ordinal", supports = supports,
    weights = weights, warn = FALSE
  )
  after <- mokken_h(
    x[order, ],
    item_type = "ordinal",
    supports = supports,
    weights = weights[order],
    warn = FALSE
  )
  expect_equal(before$pair$H_ij, after$pair$H_ij, tolerance = 2e-15)
  expect_equal(before$item$H_i, after$item$H_i, tolerance = 2e-15)
  expect_equal(before$scale$H, after$scale$H, tolerance = 2e-15)
})

test_that("valid tiny positive masses are shipped, never dropped", {
  # Regression for the pre-release tolerance-skip defect: a valid mass at or
  # below the old 1e-12 drop threshold must still be transported exactly.
  # Absolute-error bounds: the old defect lost mass at the 1e-12 scale;
  # legitimate floating-point cancellation sits at the 1e-16 scale.
  for (eps in c(5e-13, 1e-12, 1e-14)) {
    out <- bayesmokken:::bm_ordinal_max_covariance_transport(
      c(0, 1), c(1 - eps, eps), c(0, 1), c(0.5, 0.5)
    )
    expect_lt(abs(out$maximum_product - eps), 1e-15)
    expect_lt(abs(out$maximum_covariance - eps * 0.5), 1e-15)
    # Both orientations, and the mirrored margin assignment.
    out2 <- bayesmokken:::bm_ordinal_max_covariance_transport(
      c(0, 1), c(0.5, 0.5), c(0, 1), c(1 - eps, eps)
    )
    expect_lt(abs(out2$maximum_product - eps), 1e-15)
  }
})

test_that("tiny-mass configurations match the independent quantile oracle", {
  set.seed(48044)
  for (iteration in seq_len(25L)) {
    x <- sort(runif(sample(2:5, 1L), -2, 4))
    y <- sort(runif(sample(2:5, 1L), -3, 5))
    px <- rexp(length(x))
    py <- rexp(length(y))
    # Plant one adversarially tiny (but valid) mass in each margin.
    px[sample(length(px), 1L)] <- 5e-13
    py[sample(length(py), 1L)] <- 1e-13
    px <- px / sum(px)
    py <- py / sum(py)
    observed <- bayesmokken:::bm_ordinal_max_covariance_transport(
      x, px, y, py
    )$maximum_covariance
    expected <- reference_max_covariance(x, px, y, py)
    expect_lt(abs(observed - expected), 2e-13)
  }
})

test_that("unused categories (exact zero mass) and large scores are exact", {
  out <- bayesmokken:::bm_ordinal_max_covariance_transport(
    c(0, 1, 2, 3), c(0.5, 0, 0, 0.5), c(0, 10, 1000), c(0.25, 0, 0.75)
  )
  # Comonotone by hand: 0.25 -> (0, 0), 0.25 -> (0, 1000), 0.5 -> (3, 1000).
  expect_equal(out$maximum_product, 0.25 * 0 + 0.25 * 0 + 0.5 * 3 * 1000)
  expect_equal(out$mean_x, 1.5)
  expect_equal(out$mean_y, 750)
  expect_equal(out$maximum_covariance, 1500 - 1.5 * 750)
})
