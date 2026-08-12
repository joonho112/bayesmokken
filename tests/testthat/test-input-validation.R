test_that("numeric binary matrices are validated without implicit recoding", {
  x <- matrix(c(0, 1, 1, 0, 0, 1), ncol = 2)
  colnames(x) <- c("a", "b")
  z <- bayesmokken:::bm_validate_binary_data(x)
  expect_s3_class(z, "bm_validated_binary")
  expect_equal(z$data, x * 1)
  expect_false(z$score_manifest$a$reverse_coded)
})

test_that("semantic classes require explicit maps", {
  x <- data.frame(
    a = factor(c("no", "yes", "yes")),
    b = c(0, 1, 0)
  )
  expect_error(
    bayesmokken:::bm_validate_binary_data(x),
    "requires an explicit named score map",
    class = "bm_input_error"
  )
  z <- bayesmokken:::bm_validate_binary_data(
    x,
    score_map = list(a = c(no = 0, yes = 1))
  )
  expect_equal(unname(z$data[, "a"]), c(0, 1, 1))

  logical_x <- data.frame(a = c(FALSE, TRUE, TRUE), b = c(0, 1, 0))
  expect_error(bayesmokken:::bm_validate_binary_data(logical_x), "requires")
  logical_z <- bayesmokken:::bm_validate_binary_data(
    logical_x,
    score_map = list(a = c(`FALSE` = 0, `TRUE` = 1))
  )
  expect_equal(unname(logical_z$data[, "a"]), c(0, 1, 1))
})

test_that("reverse scoring and missing-code conversion are recorded", {
  x <- data.frame(a = c(0, 1, -99), b = c(0, 1, 1))
  z <- bayesmokken:::bm_validate_binary_data(
    x,
    score_map = list(a = c(`0` = 1, `1` = 0)),
    missing_codes = list(a = -99)
  )
  expect_equal(unname(z$data[, "a"]), c(1, 0, NA))
  expect_true(z$score_manifest$a$reverse_coded)
  expect_equal(unlist(z$score_manifest$a$missing_codes), -99)
})

test_that("invalid shapes, names, scores, maps, and nonfinite values fail", {
  expect_error(mokken_h(data.frame(a = 0, b = 1)), "at least two respondent")
  expect_error(mokken_h(data.frame(a = c(0, 1))), "at least two")
  bad_names <- matrix(c(0, 1, 1, 0), ncol = 2)
  expect_error(bayesmokken:::bm_validate_binary_data(bad_names), "unique")
  expect_error(mokken_h(data.frame(a = c(0, 2), b = c(0, 1))), "not scored")
  expect_error(mokken_h(data.frame(a = c(0, Inf), b = c(0, 1))), "NaN or infinite")
  expect_error(
    mokken_h(
      data.frame(a = c("x", "y"), b = c(0, 1)),
      score_map = list(a = c(x = 0))
    ),
    "does not cover"
  )
  expect_error(
    mokken_h(
      data.frame(a = c("x", "y"), b = c(0, 1)),
      score_map = list(a = c(x = 0, y = 2))
    ),
    "map to numeric 0/1"
  )
})

test_that("`warn` must be one nonmissing logical in both entry points", {
  x <- data.frame(a = c(0, 1, 1, 0), b = c(0, 1, 0, 1), c = c(1, 1, 0, 0))
  for (bad in list(1, "TRUE", NA, c(TRUE, TRUE), logical(0))) {
    expect_error(mokken_h(x, warn = bad), class = "bm_input_error")
    expect_error(bb_mokken_h(x, draws = 120, seed = 1, warn = bad),
                 class = "bm_input_error")
  }
  expect_s3_class(mokken_h(x, warn = FALSE), "bm_empirical_h")
})
