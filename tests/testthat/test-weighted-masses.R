test_that("weighted mass kernel preserves zero-weight rows and all binary cells", {
  x <- rbind(
    c(a = 0, b = 0),
    c(a = 0, b = 1),
    c(a = 1, b = 0),
    c(a = 1, b = 1)
  )
  z <- bayesmokken:::bm_weighted_masses(x, c(1, 2, 0, 1))
  expect_equal(z$weights, c(0.25, 0.5, 0, 0.25))
  expect_equal(z$zero_weight_rows, 3)
  expect_equal(z$marginals, c(a = 0.25, b = 0.75))
  expect_equal(
    z$pair_tables[[bayesmokken:::.bm_pair_key("a", "b")]],
    c(p00 = 0.25, p01 = 0.5, p10 = 0, p11 = 0.25)
  )
  expect_equal(z$category_mass["a", ], c(`0` = 0.75, `1` = 0.25))
})

test_that("weighted pair tables keep delimiter-containing names distinct", {
  x <- as.matrix(expand.grid(rep(list(0:1), 4L)))
  colnames(x) <- c("a", "b::c", "a::b", "c")

  z <- bayesmokken:::bm_weighted_masses(x)
  expected <- utils::combn(colnames(x), 2L)
  expected_keys <- bayesmokken:::.bm_pair_key(expected[1L, ], expected[2L, ])

  expect_identical(names(z$pair_tables), expected_keys)
  expect_identical(anyDuplicated(names(z$pair_tables)), 0L)
})

test_that("weight validation rejects invalid measures", {
  x <- matrix(c(0, 1, 1, 0), ncol = 2,
              dimnames = list(NULL, c("a", "b")))
  expect_error(bayesmokken:::bm_weighted_masses(x, c(0, 0)), "positive")
  expect_error(bayesmokken:::bm_weighted_masses(x, c(1, -1)), "nonnegative")
  expect_error(bayesmokken:::bm_weighted_masses(x, c(1, Inf)), "finite")
})
