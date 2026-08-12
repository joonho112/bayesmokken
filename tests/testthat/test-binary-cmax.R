test_that("binary maximum covariance is analytic and status typed", {
  p <- c(a = 0.25, b = 0.60, c = 0, d = 1)
  z <- bayesmokken:::bm_binary_cmax(p)
  expect_equal(z$value["a", "b"], 0.10)
  expect_equal(z$value["a", "a"], 0.1875)
  expect_equal(z$value["c", "a"], 0)
  expect_equal(z$status["c", "a"], "zero")
  expect_equal(z$status["d", "a"], "zero")
})

test_that("near-zero and invalid cmax states remain distinct", {
  z <- bayesmokken:::bm_binary_cmax(c(a = 1e-9, b = 0.5))
  expect_equal(z$value["a", "b"], 5e-10, tolerance = 1e-20)
  expect_equal(z$status["a", "b"], "near_zero")
  expect_error(
    bayesmokken:::.bm_denominator_state(-1e-3),
    "materially negative",
    class = "bm_invariant_error"
  )
})
