test_that("grouped row weights give pathwise identical deterministic H", {
  x <- rbind(
    c(a = 0, b = 0, c = 0),
    c(a = 0, b = 0, c = 0),
    c(a = 1, b = 1, c = 1),
    c(a = 1, b = 1, c = 1),
    c(a = 1, b = 0, c = 1)
  )
  grouping <- bayesmokken:::bm_group_patterns(x)
  row_draw <- bayesmokken:::bm_bb_weights(nrow(x), 1, seed = 51)$weights[1, ]
  grouped <- bayesmokken:::bm_group_row_weights(row_draw, grouping)

  row_fit <- mokken_h(as.data.frame(x), weights = row_draw, warn = FALSE)
  pattern_fit <- mokken_h(
    as.data.frame(grouping$patterns),
    weights = grouped,
    warn = FALSE
  )
  expect_equal(row_fit$pair$H_ij, pattern_fit$pair$H_ij, tolerance = 1e-14)
  expect_equal(row_fit$item$H_i, pattern_fit$item$H_i, tolerance = 1e-14)
  expect_equal(row_fit$scale$H, pattern_fit$scale$H, tolerance = 1e-14)
})

test_that("grouped standard BB uses pattern counts, not one per pattern", {
  x <- rbind(
    c(a = 0, b = 0), c(a = 0, b = 0), c(a = 0, b = 0),
    c(a = 1, b = 1)
  )
  grouping <- bayesmokken:::bm_group_patterns(x)
  expect_equal(grouping$counts, c(pattern_1 = 3L, pattern_2 = 1L))
  z <- bayesmokken:::bm_grouped_bb_weights(grouping, 10000, seed = 8)
  expect_lt(max(abs(unname(colMeans(z$weights)) - c(0.75, 0.25))), 0.008)

  wrong_mean <- rep(1 / grouping$n_patterns, grouping$n_patterns)
  expect_equal(wrong_mean, c(0.5, 0.5))
  expect_false(isTRUE(all.equal(colMeans(z$weights), wrong_mean,
                                tolerance = 0.05)))
})

test_that("row-aggregated and grouped Dirichlet laws have matching moments", {
  x <- rbind(
    c(a = 0, b = 0), c(a = 0, b = 0), c(a = 0, b = 0),
    c(a = 1, b = 1), c(a = 1, b = 1),
    c(a = 1, b = 0)
  )
  grouping <- bayesmokken:::bm_group_patterns(x)
  row <- bayesmokken:::bm_bb_weights(nrow(x), 7000, 101)$weights
  row_grouped <- t(apply(
    row, 1L, bayesmokken:::bm_group_row_weights, grouping = grouping
  ))
  direct <- bayesmokken:::bm_grouped_bb_weights(
    grouping, 7000, 202
  )$weights
  expect_lt(
    max(abs(colMeans(row_grouped) - colMeans(direct))),
    0.012
  )
  expect_lt(
    max(abs(
      apply(row_grouped, 2, stats::var) -
        apply(direct, 2, stats::var)
    )),
    0.004
  )
})
