test_that("weighted covariance uses the probability-measure divisor", {
  x <- rbind(c(a = 0, b = 0), c(a = 0, b = 1),
             c(a = 1, b = 0), c(a = 1, b = 1))
  masses <- bayesmokken:::bm_weighted_masses(x, c(1, 2, 0, 1))
  z <- bayesmokken:::bm_weighted_covariance(masses)
  expect_equal(z$mean, c(a = 0.25, b = 0.75))
  expect_equal(z$expected_product["a", "b"], 0.25)
  expect_equal(z$covariance["a", "b"], 0.0625)
  expect_equal(z$convention, "probability_measure")
})

test_that("equal-weight kernel is n-divisor covariance, not sample covariance", {
  x <- rbind(c(a = 0, b = 0), c(a = 1, b = 1), c(a = 1, b = 0))
  z <- bayesmokken:::bm_weighted_covariance(
    bayesmokken:::bm_weighted_masses(x)
  )
  expect_equal(
    z$covariance,
    stats::cov(x) * (nrow(x) - 1) / nrow(x),
    tolerance = 1e-15
  )
  expect_false(isTRUE(all.equal(z$covariance, stats::cov(x))))
})
