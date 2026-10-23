reference_cluster_hbb <- function(cluster, draws, seed, target) {
  label <- as.character(cluster)
  levels <- unique(label)
  index <- match(label, levels)
  sizes <- tabulate(index, nbins = length(levels))
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  draw_dirichlet <- function(k) {
    out <- matrix(rexp(k * draws), nrow = k, ncol = draws)
    sweep(out, 2L, colSums(out), "/")
  }
  a <- draw_dirichlet(length(levels))
  out <- matrix(0, length(cluster), draws)
  denominator <- if (target == "respondent") {
    colSums(sizes * a)
  } else {
    rep(1, draws)
  }
  for (g in seq_along(levels)) {
    rows <- which(index == g)
    b <- draw_dirichlet(length(rows))
    if (target == "respondent") {
      out[rows, ] <- sweep(
        b, 2L, sizes[g] * a[g, ] / denominator, "*"
      )
    } else {
      out[rows, ] <- sweep(b, 2L, a[g, ], "*")
    }
  }
  out
}

test_that("point targets implement respondent and equal-cluster masses", {
  cluster <- c("g1", "g1", "g2", "g2", "g2", "g3")
  info <- bayesmokken:::bm_validate_cluster(cluster)
  respondent <- bayesmokken:::bm_cluster_point_weights(info, "respondent")
  equal_cluster <- bayesmokken:::bm_cluster_point_weights(
    info, "equal_cluster"
  )
  expect_equal(respondent, rep(1 / 6, 6))
  expect_equal(
    as.numeric(tapply(equal_cluster, cluster, sum)),
    rep(1 / 3, 3),
    tolerance = 1e-15
  )
  expect_gt(max(abs(respondent - equal_cluster)), 0.1)
})

test_that("two-stage HBB weights match the locked independent oracle", {
  cluster <- c("g1", "g1", "g2", "g2", "g2", "g3")
  info <- bayesmokken:::bm_validate_cluster(cluster)
  for (target in c("respondent", "equal_cluster")) {
    observed <- bayesmokken:::bm_cluster_hbb_weights(
      info, draws = 37, seed = 18022, target = target
    )
    expected <- reference_cluster_hbb(
      cluster, draws = 37, seed = 18022, target = target
    )
    expect_identical(observed$weights, expected)
    expect_equal(colSums(observed$weights), rep(1, 37), tolerance = 1e-15)
    expect_match(observed$law_id, "TWO-STAGE-HBB")
    expect_match(observed$normalization_formula, "W_gi")
    expect_true(nzchar(observed$draw_identity_hash))
  }
})

test_that("cluster relabeling preserves the exact weight path", {
  cluster <- c("a", "a", "b", "b", "b", "c")
  relabeled <- c(a = "z", b = "x", c = "q")[cluster]
  info_a <- bayesmokken:::bm_validate_cluster(cluster)
  info_b <- bayesmokken:::bm_validate_cluster(relabeled)
  a <- bayesmokken:::bm_cluster_hbb_weights(
    info_a, 25, 812, "respondent"
  )
  b <- bayesmokken:::bm_cluster_hbb_weights(
    info_b, 25, 812, "respondent"
  )
  expect_identical(a$weights, b$weights)
})

test_that("same seed is exact and different seeds separate", {
  info <- bayesmokken:::bm_validate_cluster(
    c("g1", "g1", "g2", "g2", "g3", "g3")
  )
  a <- bayesmokken:::bm_cluster_hbb_weights(info, 20, 44, "respondent")
  b <- bayesmokken:::bm_cluster_hbb_weights(info, 20, 44, "respondent")
  c <- bayesmokken:::bm_cluster_hbb_weights(info, 20, 45, "respondent")
  expect_identical(a, b)
  expect_false(identical(a$weights, c$weights))
})

test_that("the historical two-stage draws equal deterministic weighted H", {
  block <- data.frame(
    a = c(0, 1, 2, 3, 0, 1, 3, 2),
    b = c(0, 1, 1, 3, 0, 2, 3, 2),
    c = c(0, 0, 2, 3, 1, 2, 3, 1)
  )
  x <- block[rep(seq_len(nrow(block)), times = 22L), , drop = FALSE]
  rownames(x) <- NULL
  cluster <- rep(sprintf("g%02d", seq_len(22L)), each = nrow(block))
  supports <- list(a = 0:3, b = 0:3, c = 0:3)
  info <- bayesmokken:::bm_validate_cluster(cluster)
  weights <- bayesmokken:::bm_cluster_hbb_weights(
    info, draws = 40, seed = 119, target = "equal_cluster"
  )$weights
  fit <- suppressWarnings(
    bb_mokken_h(
      x,
      item_type = "ordinal",
      supports = supports,
      cluster = cluster,
      cluster_target = "equal_cluster",
      weight_law = "hierarchical_cluster",
      draws = 40,
      seed = 119,
      batch_size = 7,
      warn = FALSE
    ),
    classes = "bm_historical_method_warning"
  )
  expect_identical(fit$sampling$weight_law, "hierarchical_cluster")
  expect_identical(fit$bb_type,
                   "two_stage_hierarchical_cluster_bayesian_bootstrap")
  reference <- vapply(seq_len(ncol(weights)), function(b) {
    mokken_h(
      x,
      item_type = "ordinal",
      supports = supports,
      weights = weights[, b],
      warn = FALSE
    )$scale$H
  }, numeric(1))
  expect_equal(fit$draws$scale, reference, tolerance = 3e-14)
})
