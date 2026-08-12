.binary_support <- data.frame(
  item_i = c(0, 0, 1, 1),
  item_j = c(0, 1, 0, 1)
)

test_that("frozen binary hand catalogue matches internal and public paths", {
  fixture <- read.csv(
    testthat::test_path("fixtures", "binary-hand", "expected.csv"),
    na.strings = "NA",
    check.names = FALSE
  )
  for (k in seq_len(nrow(fixture))) {
    cells <- unlist(fixture[k, c("p00", "p01", "p10", "p11")])
    internal <- bayesmokken:::bm_h_pair(
      bayesmokken:::bm_weighted_masses(as.matrix(.binary_support), cells)
    )$pair
    public <- suppressWarnings(mokken_h(.binary_support, weights = cells))

    expect_equal(internal$p_i, fixture$p_i[k], tolerance = 1e-15,
                 info = fixture$id[k])
    expect_equal(internal$p_j, fixture$p_j[k], tolerance = 1e-15,
                 info = fixture$id[k])
    expect_equal(internal$cov, fixture$covariance[k], tolerance = 1e-15,
                 info = fixture$id[k])
    expect_equal(internal$cov_max, fixture$denominator[k], tolerance = 1e-15,
                 info = fixture$id[k])
    expect_equal(internal$H_ij, fixture$H[k], tolerance = 1e-12,
                 info = fixture$id[k])
    expect_equal(internal$denominator_status, fixture$state[k],
                 info = fixture$id[k])
    expect_equal(public$pair$H_ij, internal$H_ij, tolerance = 1e-15,
                 info = fixture$id[k])
    expect_equal(public$scale$H, internal$H_ij, tolerance = 1e-15,
                 info = fixture$id[k])
  }
})

test_that("frozen multivariate ratio-of-sums truth matches public wrapper", {
  x <- data.frame(
    i1 = c(0, 0, 1, 1, 1, 0, 1, 0),
    i2 = c(0, 1, 1, 1, 0, 0, 1, 0),
    i3 = c(0, 0, 1, 0, 1, 1, 1, 0)
  )
  fit <- mokken_h(x, warn = FALSE)
  expect_equal(fit$item$H_i, c(0.5, 0.25, 0.25), tolerance = 1e-15)
  expect_equal(fit$scale$H, 1 / 3, tolerance = 1e-15)
})
