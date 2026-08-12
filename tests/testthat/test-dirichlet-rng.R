test_that("respondent BB weights are positive and sum to one", {
  z <- bayesmokken:::bm_bb_weights(n = 7, draws = 200, seed = 42)
  expect_s3_class(z, "bm_bb_weight_draws")
  expect_true(all(z$weights > 0))
  expect_equal(rowSums(z$weights), rep(1, 200), tolerance = 1e-14)
  expect_equal(z$alpha, rep(1, 7))
  expect_equal(z$seed, 42L)
  expect_equal(z$rng_kind, "L'Ecuyer-CMRG")
  expect_match(z$stream_map_hash, "^[0-9a-f]{64}$")
})

test_that("Dirichlet first two moments match theory", {
  alpha <- c(a = 1, b = 2, c = 4)
  z <- bayesmokken:::bm_dirichlet_draws(alpha, draws = 12000, seed = 99)
  a0 <- sum(alpha)
  expected_mean <- alpha / a0
  expected_var <- alpha * (a0 - alpha) / (a0^2 * (a0 + 1))
  expect_lt(max(abs(colMeans(z$weights) - expected_mean)), 0.006)
  expect_lt(
    max(abs(apply(z$weights, 2, stats::var) - expected_var)),
    0.002
  )
})

test_that("Dirichlet generator preserves caller RNG state", {
  set.seed(123)
  before <- .Random.seed
  invisible(bayesmokken:::bm_bb_weights(4, 10, 77))
  expect_identical(.Random.seed, before)
})

test_that("invalid BB parameters fail", {
  expect_error(bayesmokken:::bm_bb_weights(1, 10, 1), "n >= 2")
  expect_error(
    bayesmokken:::bm_dirichlet_draws(c(1, 0), 10, 1),
    "strictly positive"
  )
  expect_error(bayesmokken:::bm_bb_weights(3, 0, 1), "positive integer")
})
