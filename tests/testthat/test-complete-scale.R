test_that("complete-scale preprocessing fixes one common row distribution", {
  path <- testthat::test_path("fixtures", "missingness", "complete-scale.csv")
  raw <- read.csv(path, na.strings = "NA", check.names = FALSE)
  row_ids <- raw$row_id
  x <- raw[, -1]
  validated <- bayesmokken:::bm_validate_binary_data(x)
  cc <- bayesmokken:::bm_complete_scale(validated, row_ids = row_ids)

  expect_equal(cc$included_index, c(5L, 6L))
  expect_equal(cc$excluded_index, 1:4)
  expect_equal(cc$included_row_ids, c("E", "F"))
  expect_equal(cc$n_total, 6)
  expect_equal(cc$n_complete, 2)
  expect_equal(cc$row_target, "complete_scale")
  expect_match(cc$target_statement, "F_cc")
  expect_identical(
    cc$row_hash,
    bayesmokken:::bm_complete_scale(validated, row_ids = row_ids)$row_hash
  )
})

test_that("pairwise target requests and invalid row ids fail", {
  x <- data.frame(a = c(0, 1, NA), b = c(0, 1, 1))
  validated <- bayesmokken:::bm_validate_binary_data(x)
  expect_error(
    bayesmokken:::bm_complete_scale(validated, missing = "pairwise"),
    "pseudo-estimand"
  )
  expect_error(
    bayesmokken:::bm_complete_scale(validated, row_ids = c("x", "x", "z")),
    "unique"
  )
  expect_error(
    mokken_h(data.frame(a = c(NA, 1), b = c(NA, 1))),
    "Fewer than two"
  )
})

test_that("original-row weights are subset only after complete-scale filtering", {
  x <- data.frame(a = c(0, NA, 1), b = c(0, 1, 1))
  fit <- mokken_h(x, weights = c(1, 100, 3), warn = FALSE)
  expect_equal(fit$provenance$n_complete, 2)
  expect_equal(fit$provenance$input_weight_sum, 4)
  expect_equal(fit$pair$p_i, 0.75)
})
