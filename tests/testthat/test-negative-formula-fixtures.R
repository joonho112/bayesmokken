test_that("scale H is not the mean of pair H values", {
  pair <- data.frame(
    item_i = c("a", "a"),
    item_j = c("b", "c"),
    cov = c(0.49, 0.01),
    cov_max = c(0.50, 0.50)
  )
  correct <- bayesmokken:::bm_h_aggregate(pair, c("a", "b", "c"))$scale$H
  wrong <- mean(pair$cov / pair$cov_max)
  expect_equal(correct, 0.5)
  expect_equal(wrong, 0.5)

  trap <- data.frame(
    item_i = c("a", "a"),
    item_j = c("b", "c"),
    cov = c(0.45, 0.02),
    cov_max = c(0.50, 0.10)
  )
  correct_trap <- bayesmokken:::bm_h_aggregate(
    trap, c("a", "b", "c")
  )$scale$H
  wrong_trap <- mean(trap$cov / trap$cov_max)
  expect_equal(correct_trap, 0.47 / 0.60)
  expect_equal(wrong_trap, 0.55)
  expect_false(isTRUE(all.equal(correct_trap, wrong_trap)))
})

test_that("correlation and min variance are not binary H denominators", {
  support <- data.frame(
    a = c(0, 0, 1, 1),
    b = c(0, 1, 0, 1)
  )
  fit <- mokken_h(
    support,
    weights = c(0.50, 0.25, 0, 0.25),
    warn = FALSE
  )
  p <- fit$pair
  corr_wrong <- p$cov / sqrt(p$p_i * (1 - p$p_i) *
                               p$p_j * (1 - p$p_j))
  min_var_wrong <- p$cov / min(
    p$p_i * (1 - p$p_i),
    p$p_j * (1 - p$p_j)
  )
  expect_equal(p$H_ij, 1)
  expect_equal(corr_wrong, 1 / sqrt(3), tolerance = 1e-15)
  expect_equal(min_var_wrong, 2 / 3, tolerance = 1e-15)
  expect_false(isTRUE(all.equal(p$H_ij, corr_wrong)))
  expect_false(isTRUE(all.equal(p$H_ij, min_var_wrong)))
})

test_that("sorting rows independently under unequal weights changes the law", {
  x <- data.frame(a = c(0, 1, 0), b = c(1, 1, 0))
  w <- c(0.7, 0.2, 0.1)
  fit <- mokken_h(x, weights = w, warn = FALSE)
  sorted_wrong <- data.frame(a = sort(x$a), b = sort(x$b))
  wrong <- mokken_h(sorted_wrong, weights = w, warn = FALSE)
  expect_false(isTRUE(all.equal(fit$pair$cov, wrong$pair$cov)))
})

test_that("sample covariance and pairwise completion cannot silently replace target", {
  x <- data.frame(
    a = c(0, 1, 1, 1),
    b = c(0, 1, NA, NA),
    c = c(NA, NA, 0, 1)
  )
  expect_error(mokken_h(x, missing = "pairwise"), "pseudo-estimand")

  complete_x <- data.frame(a = c(0, 1, 1), b = c(0, 1, 0))
  fit <- mokken_h(complete_x, warn = FALSE)
  sample_cov_wrong <- stats::cov(complete_x)[1, 2]
  expect_equal(fit$pair$cov, sample_cov_wrong * 2 / 3)
  expect_false(isTRUE(all.equal(fit$pair$cov, sample_cov_wrong)))
})
