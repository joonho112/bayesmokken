test_that("new optional arguments leave deterministic binary output unchanged", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0),
    b = c(0, 1, 1, 1, 0, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, 0)
  )
  legacy <- mokken_h(x, warn = FALSE)
  explicit <- mokken_h(
    x,
    warn = FALSE,
    item_type = "binary",
    cluster_target = "respondent",
    weight_law = "iid_respondent"
  )
  expect_identical(legacy, explicit)
  expect_equal(legacy$scale$H, 1 / 3, tolerance = 1e-15)
  expect_identical(legacy$estimand$version, "paperA-HF-binary-v1")
})

test_that("new optional arguments leave binary BB draws and IDs unchanged", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0),
    b = c(0, 1, 1, 1, 0, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, 0)
  )
  legacy <- bb_mokken_h(
    x, draws = 125, seed = 18024, batch_size = 19, workers = 5,
    warn = FALSE
  )
  explicit <- bb_mokken_h(
    x, draws = 125, seed = 18024, batch_size = 19, workers = 5,
    warn = FALSE, item_type = "binary", weight_law = "iid_respondent"
  )
  expect_identical(legacy, explicit)
  expect_identical(
    legacy$interval$method_id,
    "P7-FS-WIDTH-RAW-G025-v1"
  )
  expect_identical(legacy$interval$gamma, 0.25)
  expect_identical(
    legacy$bb_type,
    "standard_respondent_bayesian_bootstrap"
  )
})
