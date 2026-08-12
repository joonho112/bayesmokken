.three_item_hand <- rbind(
  c(0, 0, 0),
  c(0, 1, 0),
  c(1, 1, 1),
  c(1, 1, 0),
  c(1, 0, 1),
  c(0, 0, 1),
  c(1, 1, 1),
  c(0, 0, 0)
)
colnames(.three_item_hand) <- c("a", "b", "c")

test_that("item and scale H are ratios of sums", {
  pair <- bayesmokken:::bm_h_pair(
    bayesmokken:::bm_weighted_masses(.three_item_hand)
  )$pair
  z <- bayesmokken:::bm_h_aggregate(pair, colnames(.three_item_hand))
  expect_equal(z$item$H_i, c(0.5, 0.25, 0.25), tolerance = 1e-15)
  expect_equal(z$scale$H, 1 / 3, tolerance = 1e-15)
  expect_equal(
    z$scale$H,
    sum(pair$cov) / sum(pair$cov_max),
    tolerance = 1e-15
  )
})

test_that("item H is recomputed for each candidate scale", {
  full <- mokken_h(as.data.frame(.three_item_hand), warn = FALSE)
  subset <- mokken_h(as.data.frame(.three_item_hand), items = c("a", "b"),
                     warn = FALSE)
  expect_equal(subset$item$H_i, rep(subset$pair$H_ij, 2))
  expect_false(isTRUE(all.equal(
    full$item$H_i[full$item$item %in% c("a", "b")],
    subset$item$H_i
  )))
})

test_that("aggregate zero denominator remains NA with status", {
  pair <- data.frame(
    item_i = "a", item_j = "b", cov = 0, cov_max = 0,
    stringsAsFactors = FALSE
  )
  z <- bayesmokken:::bm_h_aggregate(pair, c("a", "b"))
  expect_true(all(is.na(z$item$H_i)))
  expect_true(is.na(z$scale$H))
  expect_equal(z$scale$denominator_status, "zero")
})
