# =============================================================================
# bm_simulate() and the bundled datasets
# =============================================================================

test_that("a seed makes the result reproducible", {
  a <- bm_simulate(n = 120, n_items = 5, seed = 42)
  b <- bm_simulate(n = 120, n_items = 5, seed = 42)
  expect_identical(a, b)
  expect_false(identical(a, bm_simulate(n = 120, n_items = 5, seed = 43)))
})

test_that("the caller's random stream is left alone", {
  set.seed(99)
  before <- stats::runif(3)
  set.seed(99)
  invisible(bm_simulate(n = 50, n_items = 3, seed = 7))
  after <- stats::runif(3)
  expect_identical(before, after)
})

test_that("binary output is 0/1 with the requested shape", {
  d <- bm_simulate(n = 80, n_items = 4, type = "binary", seed = 1)
  expect_identical(dim(d), c(80L, 4L))
  expect_identical(names(d), paste0("item", 1:4))
  expect_true(all(vapply(d, function(x) all(x %in% 0:1), logical(1))))
})

test_that("ordinal output uses 1..n_categories", {
  d <- bm_simulate(n = 150, n_items = 4, type = "ordinal", n_categories = 5,
                   seed = 1)
  expect_true(all(as.matrix(d) %in% 1:5))
  expect_true(max(as.matrix(d)) > 1)
})

test_that("clustered output carries a cluster column of the right size", {
  d <- bm_simulate(n = 200, n_items = 4, clusters = 10, icc = 0.2, seed = 1)
  expect_identical(names(d)[1], "cluster")
  expect_identical(nlevels(d$cluster), 10L)
  expect_true(min(table(d$cluster)) >= 2L)
  expect_equal(nrow(d), sum(attr(d, "truth")$cluster_sizes))
})

test_that("informative_size makes the two cluster targets differ", {
  # The mechanism ties cluster size to level AND spread. Level alone is not
  # enough: H is a ratio of covariance sums, so moving a cluster up or down
  # barely touches it. This test is the reason the generator does both.
  items <- paste0("item", 1:5)
  h_of <- function(d, target) {
    sup <- stats::setNames(rep(list(1:5), length(items)), items)
    mokken_h(d[, items], item_type = "ordinal", supports = sup,
             cluster = as.character(d$cluster), cluster_target = target,
             weight_law = "complete_record", warn = FALSE)$scale$H
  }
  informative <- bm_simulate(n = 500, n_items = 5, type = "ordinal",
                             clusters = 25, icc = 0.15,
                             informative_size = TRUE, seed = 3)
  gap <- abs(h_of(informative, "respondent") -
               h_of(informative, "equal_cluster"))
  expect_gt(gap, 0.003)

  sizes <- as.integer(table(informative$cluster))
  means <- tapply(rowSums(informative[, items]), informative$cluster, mean)
  expect_gt(stats::cor(sizes, as.numeric(means), method = "spearman"), 0.4)
})

test_that("invalid input fails with a typed error rather than a bad dataset", {
  expect_error(bm_simulate(n = 2), class = "bm_input_error")
  expect_error(bm_simulate(n = 100, n_items = 1), class = "bm_input_error")
  expect_error(bm_simulate(n = 100, discrimination = 0), class = "bm_input_error")
  expect_error(bm_simulate(n = 100, clusters = 10, icc = 1), class = "bm_input_error")
  expect_error(bm_simulate(n = 10, clusters = 10), class = "bm_input_error")
  expect_error(bm_simulate(n = 100, seed = 1.5), class = "bm_input_error")
  expect_error(bm_simulate(n = 100, n_items = 4, difficulty = c(1, 2)),
               class = "bm_input_error")
})

test_that("the truth attribute records what generated the data", {
  d <- bm_simulate(n = 100, n_items = 4, clusters = 8, icc = 0.1, seed = 5)
  tr <- attr(d, "truth")
  expect_identical(tr$generator, "bm_simulate")
  expect_match(tr$note, "Not the simulation engine", fixed = TRUE)
  expect_identical(tr$clusters, 8L)
  expect_equal(tr$icc, 0.1)
  expect_length(tr$theta, nrow(d))
})

# --- bundled data -------------------------------------------------------------

test_that("srs is the published 337 x 11 binary matrix", {
  data("srs", package = "bayesmokken", envir = environment())
  expect_identical(dim(srs), c(337L, 11L))
  expect_identical(names(srs), paste0("SRS", 1:11))
  expect_false(anyNA(srs))
  expect_true(all(vapply(srs, function(x) all(x %in% 0:1), logical(1))))
})

test_that("srs reproduces the published scale coefficient", {
  data("srs", package = "bayesmokken", envir = environment())
  expect_equal(mokken_h(srs, warn = FALSE)$scale$H, 0.130205929962575,
               tolerance = 1e-12)
})

test_that("classroom_wb has the structure its documentation promises", {
  data("classroom_wb", package = "bayesmokken", envir = environment())
  items <- paste0("wb", 1:6)
  expect_identical(names(classroom_wb), c("class_id", items))
  expect_identical(nlevels(classroom_wb$class_id), 30L)
  expect_true(all(as.matrix(classroom_wb[, items]) %in% 1:5))
  expect_false(anyNA(classroom_wb))
  expect_gte(min(table(classroom_wb$class_id)), 2L)
})

test_that("classroom_wb keeps the two cluster targets apart", {
  # If this ever stops holding, the dataset stops teaching the distinction it
  # exists to teach, and a3/m4 lose their worked example.
  data("classroom_wb", package = "bayesmokken", envir = environment())
  items <- paste0("wb", 1:6)
  sup <- stats::setNames(rep(list(1:5), length(items)), items)
  h <- vapply(c("respondent", "equal_cluster"), function(tg) {
    mokken_h(classroom_wb[, items], item_type = "ordinal", supports = sup,
             cluster = as.character(classroom_wb$class_id),
             cluster_target = tg, weight_law = "complete_record",
             warn = FALSE)$scale$H
  }, numeric(1))
  expect_gt(abs(diff(h)), 0.005)
})
