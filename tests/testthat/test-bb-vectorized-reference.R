test_that("vectorized BB kernel matches direct core on every frozen draw", {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0, 1, 0),
    b = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1),
    c = c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1),
    d = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0)
  )
  fit <- bb_mokken_h(
    x, draws = 37, seed = 9012, batch_size = 11, warn = FALSE
  )
  validated <- bayesmokken:::bm_validate_binary_data(x)
  complete <- bayesmokken:::bm_complete_scale(validated)
  grouping <- bayesmokken:::bm_group_patterns(complete$data)
  weights <- bayesmokken:::bm_grouped_bb_weights(
    grouping, draws = 37, seed = 9012
  )$weights

  for (draw in seq_len(nrow(weights))) {
    direct <- mokken_h(
      as.data.frame(grouping$patterns),
      weights = weights[draw, ],
      warn = FALSE
    )
    expect_equal(
      unname(fit$draws$pair[draw, ]),
      direct$pair$H_ij,
      tolerance = 1e-12
    )
    expect_equal(
      unname(fit$draws$item[draw, ]),
      direct$item$H_i,
      tolerance = 1e-12
    )
    expect_equal(
      fit$draws$scale[draw],
      direct$scale$H,
      tolerance = 1e-12
    )
    expect_identical(
      unname(fit$draws$pair_status[draw, ]),
      direct$pair$denominator_status
    )
    expect_identical(
      unname(fit$draws$item_status[draw, ]),
      direct$item$denominator_status
    )
    expect_identical(
      fit$draws$scale_status[draw],
      direct$scale$denominator_status
    )
  }
})
