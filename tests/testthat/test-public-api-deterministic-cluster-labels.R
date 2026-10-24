test_that("numeric cluster IDs cannot merge under character conversion", {
  ids <- c(1000000000000000, 1000000000000001, 2000000000000000)
  expect_length(unique(ids), 3L)
  expect_length(unique(as.character(ids)), 2L)
  expect_error(bayesmokken:::bm_validate_cluster(ids),
               "collide under character conversion", class = "bm_input_error")
  expect_error(bayesmokken:::bm_validate_cluster(ids[1:2]),
               "collide under character conversion", class = "bm_input_error")
  ordinary <- bayesmokken:::bm_validate_cluster(c(2L, 2L, 1L, 3L))
  expect_identical(ordinary$index, c(1L, 1L, 2L, 3L))
  expect_identical(ordinary$sizes, c(2L, 1L, 1L))
})

test_that("public clustered paths reject colliding IDs and retain explicit IDs", {
  x <- data.frame(
    a = c(0,0,1,1,2,2,3,3,0,1,2,3),
    b = c(0,1,0,1,2,3,2,3,3,2,1,0),
    c = c(0,1,2,3,1,2,1,2,3,1,0,2)
  )
  supports <- list(a = 0:3, b = 0:3, c = 0:3)
  numeric_id <- rep(c(1000000000000000, 1000000000000001,
                      2000000000000000), each = 4)
  explicit <- rep(c("1000000000000000", "1000000000000001",
                    "2000000000000000"), each = 4)
  for (target in c("respondent", "equal_cluster")) {
    expect_error(mokken_h(x, supports = supports, cluster = numeric_id,
                          cluster_target = target, warn = FALSE),
                 "collide under character conversion", class = "bm_input_error")
    expect_error(bb_mokken_h(x, supports = supports, cluster = numeric_id,
                             cluster_target = target, draws = 40,
                             seed = 1, warn = FALSE),
                 "collide under character conversion", class = "bm_input_error")
    good <- bb_mokken_h(x, supports = supports, cluster = explicit,
                        cluster_target = target, draws = 40,
                        seed = 1, warn = FALSE)
    same <- bb_mokken_h(x, supports = supports, cluster = rep(1:3, each = 4),
                        cluster_target = target, draws = 40,
                        seed = 1, warn = FALSE)
    expect_identical(good$interval$cluster_count, 3L)
    expect_identical(good$interval$status, "OK")
    expect_identical(good$draws, same$draws)
    expect_identical(good$interval, same$interval)
  }
})
