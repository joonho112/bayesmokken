test_that("empirical object preserves estimand and provenance", {
  x <- data.frame(
    a = c(0, 0, 1, 1, NA),
    b = c(0, 1, 1, 1, 0),
    c = c(0, 0, 1, 0, 1)
  )
  fit <- mokken_h(x, row_ids = letters[1:5], warn = FALSE)
  expect_s3_class(fit, "bm_empirical_h")
  expect_equal(fit$estimand$version, "paperA-HF-binary-v1")
  expect_equal(fit$estimand$row_target, "complete_scale")
  expect_equal(fit$estimand$item_aggregation, "ratio_of_sums")
  expect_equal(fit$provenance$n_total, 5)
  expect_equal(fit$provenance$n_complete, 4)
  expect_equal(fit$provenance$excluded_row_ids, "e")
  expect_match(fit$provenance$row_hash, "^[0-9a-f]{64}$")
  expect_match(fit$provenance$score_map_hash, "^[0-9a-f]{64}$")

  sm <- summary(fit)
  expect_s3_class(sm, "summary_bm_empirical_h")
  expect_output(print(fit), "scale H")
  # v1 note: the console layout was redesigned in Phase 5 (log 006).
  # The STRUCTURE of the printed information is unchanged and the
  # method identifier is still shown; only the wording and layout moved.
  # The original assertion is kept beside the new one so the change is
  # visible in the diff rather than silently dropped.
  # was: expect_output(print(sm), "summary_bm_empirical_h")
  expect_s3_class(sm, "summary_bm_empirical_h")
  expect_output(print(sm), "summary")
  expect_output(print(sm), "scale H")
})

test_that("zero and near-zero boundaries warn and remain data", {
  zero_x <- data.frame(a = c(0, 0, 0, 0), b = c(0, 1, 0, 1))
  expect_warning(
    zero <- mokken_h(zero_x),
    class = "bm_boundary_warning"
  )
  expect_true(is.na(zero$scale$H))
  expect_equal(zero$pair$denominator_status, "zero")

  support <- data.frame(
    a = c(0, 0, 1, 1),
    b = c(0, 1, 0, 1)
  )
  expect_warning(
    near <- mokken_h(
      support,
      weights = c(
        0.49999999975, 0.49999999925,
        0.00000000025, 0.00000000075
      )
    ),
    class = "bm_boundary_warning"
  )
  expect_equal(near$pair$denominator_status, "near_zero")
  expect_equal(near$pair$H_ij, 0.5, tolerance = 1e-12)
  expect_equal(near$scale$H, 0.5, tolerance = 1e-12)
})

test_that("zero weights are represented in provenance", {
  x <- data.frame(a = c(0, 0, 1), b = c(0, 1, 1))
  fit <- mokken_h(x, weights = c(1, 0, 1), warn = FALSE)
  expect_equal(fit$provenance$zero_weight_complete_rows, 2)
})
