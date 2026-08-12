test_that("public API alone computes typed fixed-scale coefficients", {
  x <- data.frame(
    i1 = c(0, 0, 1, 1, 1, 0, 1, 0),
    i2 = c(0, 1, 1, 1, 0, 0, 1, 0),
    i3 = c(0, 0, 1, 0, 1, 1, 1, 0)
  )
  fit <- mokken_h(x, warn = FALSE)
  expect_s3_class(fit, "bm_empirical_h")
  expect_equal(fit$item$H_i, c(0.5, 0.25, 0.25), tolerance = 1e-15)
  expect_equal(fit$scale$H, 1 / 3, tolerance = 1e-15)
  expect_equal(nrow(fit$pair), choose(3, 2))
})

test_that("fixed item set controls output and unknown items fail", {
  x <- data.frame(
    a = c(0, 0, 1, 1),
    b = c(0, 1, 1, 1),
    c = c(0, 0, 1, 0)
  )
  fit <- mokken_h(x, items = c("c", "a"), warn = FALSE)
  expect_equal(fit$item$item, c("c", "a"))
  expect_equal(fit$pair$item_i, "c")
  expect_equal(fit$pair$item_j, "a")
  expect_error(mokken_h(x, items = c("a", "z")), "requested item")
})

test_that("deterministic API remains type-separated after BB export", {
  expect_false("draws" %in% names(formals(mokken_h)))
  expect_false("seed" %in% names(formals(mokken_h)))
  x <- data.frame(a = c(0, 0, 1, 1), b = c(0, 1, 1, 1))
  fit <- mokken_h(x, warn = FALSE)
  expect_s3_class(fit, "bm_empirical_h")
  expect_false(inherits(fit, "bm_bb_h"))
})
