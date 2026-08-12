test_that("pair state returns covariance, cmax, H, margins, and status", {
  x <- rbind(c(a = 0, b = 0), c(a = 0, b = 1),
             c(a = 1, b = 0), c(a = 1, b = 1))
  z <- bayesmokken:::bm_h_pair(
    bayesmokken:::bm_weighted_masses(x, c(0.30, 0.45, 0.10, 0.15))
  )$pair
  expect_named(
    z,
    c("item_i", "item_j", "p_i", "p_j", "cov", "cov_max_raw",
      "cov_max", "H_ij", "denominator_status")
  )
  expect_equal(z$p_i, 0.25)
  expect_equal(z$p_j, 0.60)
  expect_equal(z$cov, 0)
  expect_equal(z$cov_max, 0.10)
  expect_equal(z$H_ij, 0)
  expect_equal(z$denominator_status, "positive")
})

test_that("negative pair H is retained without clipping", {
  x <- rbind(c(a = 0, b = 0), c(a = 0, b = 1),
             c(a = 1, b = 0), c(a = 1, b = 1))
  z <- bayesmokken:::bm_h_pair(
    bayesmokken:::bm_weighted_masses(x, c(0, 0.5, 0.5, 0))
  )$pair
  expect_equal(z$H_ij, -1)
  expect_lt(z$H_ij, 0)
})

test_that("zero pair denominator yields structured NA", {
  x <- rbind(c(a = 0, b = 0), c(a = 0, b = 1),
             c(a = 1, b = 0), c(a = 1, b = 1))
  z <- bayesmokken:::bm_h_pair(
    bayesmokken:::bm_weighted_masses(x, c(0.5, 0.5, 0, 0))
  )$pair
  expect_true(is.na(z$H_ij))
  expect_equal(z$denominator_status, "zero")
})
