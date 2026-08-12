.bb_test_data <- data.frame(
  a = c(0, 0, 1, 1, 1, 0, 1, 0),
  b = c(0, 1, 1, 1, 0, 0, 1, 0),
  c = c(0, 0, 1, 0, 1, 1, 1, 0)
)

test_that("sparse engine returns joint functional draws and typed metadata", {
  fit <- bb_mokken_h(
    .bb_test_data, draws = 120, seed = 71,
    batch_size = 17, warn = FALSE
  )
  expect_s3_class(fit, "bm_bb_h")
  expect_s3_class(fit$empirical, "bm_empirical_h")
  expect_equal(dim(fit$draws$pair), c(120, 3))
  expect_equal(dim(fit$draws$item), c(120, 3))
  expect_length(fit$draws$scale, 120)
  expect_equal(fit$bb_type, "standard_respondent_bayesian_bootstrap")
  expect_match(fit$functional, "fixed item set")
  expect_false(fit$computation$dense_pattern_space)
  expect_false(fit$computation$replicated_response_matrices)
  expect_lt(fit$grouping$n_patterns, 2^ncol(.bb_test_data))
  expect_null(fit$draws$y_rep)
})

test_that("first sparse draw matches direct deterministic grouped evaluation", {
  fit <- bb_mokken_h(
    .bb_test_data, draws = 20, seed = 82,
    batch_size = 5, warn = FALSE
  )
  validated <- bayesmokken:::bm_validate_binary_data(.bb_test_data)
  complete <- bayesmokken:::bm_complete_scale(validated)
  grouping <- bayesmokken:::bm_group_patterns(complete$data)
  weights <- bayesmokken:::bm_grouped_bb_weights(
    grouping, draws = 20, seed = 82
  )$weights[1, ]
  direct <- mokken_h(
    as.data.frame(grouping$patterns),
    weights = weights,
    warn = FALSE
  )
  expect_equal(
    unname(fit$draws$pair[1, ]),
    direct$pair$H_ij,
    tolerance = 1e-14
  )
  expect_equal(
    unname(fit$draws$item[1, ]),
    direct$item$H_i,
    tolerance = 1e-14
  )
  expect_equal(fit$draws$scale[1], direct$scale$H, tolerance = 1e-14)
})

test_that("single observed response pattern is retained as undefined data", {
  x <- data.frame(a = rep(1, 5), b = rep(1, 5))
  fit <- suppressWarnings(bb_mokken_h(x, draws = 20, seed = 2))
  expect_true(all(is.na(fit$draws$scale)))
  expect_equal(fit$draws$scale_status, rep("zero", 20))
  expect_equal(fit$summaries$scale_draws$defined_fraction, 0)
  expect_equal(fit$interval$status, "UNDEFINED_POINT_ESTIMATE")
})
