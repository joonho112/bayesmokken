# =============================================================================
# The complete-record Bayesian-bootstrap law (default clustered lane)
#
# One Dirichlet weight per cluster record, nothing reweighted inside a record,
# and a draw stream that is the clustered article's archive stream. The oracle
# in helper-archive-interval.R reimplements that stream independently of the
# package generator, with the archive's block size of 128, which the package
# does not use.
# =============================================================================

crbb_fixture <- function() {
  list(
    cluster = c("g1", "g1", "g2", "g2", "g2", "g3", "g4", "g4", "g4", "g4"),
    draws = 300L,
    seed = 1883301060L
  )
}

test_that("complete-record weights reproduce the archive stream for both targets", {
  fx <- crbb_fixture()
  info <- bayesmokken:::bm_validate_cluster(fx$cluster)
  oracle <- .archive_crbb_weights(fx$cluster, fx$draws, fx$seed)
  for (target in c("respondent", "equal_cluster")) {
    observed <- bayesmokken:::bm_cluster_crbb_weights(
      info, draws = fx$draws, seed = fx$seed, target = target
    )
    expect_identical(observed$weights, oracle[[target]])
    expect_equal(colSums(observed$weights), rep(1, fx$draws), tolerance = 1e-14)
    expect_true(all(observed$weights > 0))
    expect_identical(observed$rng_mapping, "V4R-DRAW-MAJOR-EXP-GPLUSN-CMRG-v1")
    expect_false(observed$within_record_reweighting)
    expect_match(observed$law_id, "^V5-CRBB-")
  }
})

test_that("weights are flat within a record and follow A_g / m_g or A_g / sum m_h A_h", {
  fx <- crbb_fixture()
  info <- bayesmokken:::bm_validate_cluster(fx$cluster)
  w <- bayesmokken:::bm_cluster_crbb_weights(
    info, draws = 50L, seed = 7L, target = "equal_cluster"
  )
  a <- w$record_mass
  expect_equal(dim(a), c(info$G, 50L))
  expect_equal(colSums(a), rep(1, 50L), tolerance = 1e-14)
  # Every respondent of record g carries A_g / m_g: no second reweighting.
  for (g in seq_len(info$G)) {
    rows <- which(info$index == g)
    block <- w$weights[rows, , drop = FALSE]
    expect_equal(block, matrix(rep(a[g, ] / info$sizes[g], each = length(rows)),
                               nrow = length(rows)), tolerance = 0)
  }
  r <- bayesmokken:::bm_cluster_crbb_weights(
    info, draws = 50L, seed = 7L, target = "respondent"
  )
  expect_identical(r$record_mass, a)   # the same outer draws for both targets
  denominator <- colSums(info$sizes * a)
  expect_equal(r$weights, sweep(a[info$index, ], 2L, denominator, "/"),
               tolerance = 0)
  # Mean substitution recovers the plug-in target weights exactly.
  mean_a <- rep(1 / info$G, info$G)
  expect_equal(mean_a[info$index] / info$sizes[info$index],
               bayesmokken:::bm_cluster_point_weights(info, "equal_cluster"),
               tolerance = 1e-15)
  expect_equal(mean_a[info$index] / sum(info$sizes * mean_a),
               bayesmokken:::bm_cluster_point_weights(info, "respondent"),
               tolerance = 1e-15)
})

test_that("the stream is draw-major: prefixes, block sizes and relabeling agree", {
  fx <- crbb_fixture()
  info <- bayesmokken:::bm_validate_cluster(fx$cluster)
  long <- bayesmokken:::bm_cluster_crbb_weights(info, 700L, fx$seed, "respondent")
  short <- bayesmokken:::bm_cluster_crbb_weights(info, 300L, fx$seed, "respondent")
  expect_identical(short$weights, long$weights[, 1:300])
  # 700 draws cross the package's 256-draw stream block and the oracle's 128.
  expect_identical(long$weights,
                   .archive_crbb_weights(fx$cluster, 700L, fx$seed, block = 128L)$respondent)
  expect_identical(long$weights,
                   .archive_crbb_weights(fx$cluster, 700L, fx$seed, block = 1L)$respondent)
  relabeled <- c(g1 = "z", g2 = "x", g3 = "q", g4 = "a")[fx$cluster]
  info_b <- bayesmokken:::bm_validate_cluster(relabeled)
  expect_identical(
    bayesmokken:::bm_cluster_crbb_weights(info_b, 40L, 812L, "respondent")$weights,
    bayesmokken:::bm_cluster_crbb_weights(info, 40L, 812L, "respondent")$weights
  )
  a <- bayesmokken:::bm_cluster_crbb_weights(info, 20L, 44L, "respondent")
  b <- bayesmokken:::bm_cluster_crbb_weights(info, 20L, 44L, "respondent")
  c <- bayesmokken:::bm_cluster_crbb_weights(info, 20L, 45L, "respondent")
  expect_identical(a, b)
  expect_false(identical(a$weights, c$weights))
})

test_that("the generator leaves the caller's RNG state untouched", {
  info <- bayesmokken:::bm_validate_cluster(crbb_fixture()$cluster)
  set.seed(123)
  before <- .Random.seed
  invisible(bayesmokken:::bm_cluster_crbb_weights(info, 10L, 77L, "respondent"))
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1L], "Mersenne-Twister")
})

test_that("the clustered engine evaluates every complete-record draw exactly", {
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
  for (target in c("respondent", "equal_cluster")) {
    weights <- bayesmokken:::bm_cluster_crbb_weights(
      info, draws = 40L, seed = 119L, target = target
    )$weights
    fit <- bb_mokken_h(
      x,
      item_type = "ordinal",
      supports = supports,
      cluster = cluster,
      cluster_target = target,
      draws = 40,
      seed = 119,
      batch_size = 7,
      warn = FALSE
    )
    expect_identical(fit$sampling$weight_law, "complete_record")
    expect_identical(fit$bb_type, "complete_record_cluster_bayesian_bootstrap")
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
    expect_true(all(fit$draws$scale_denominator > 1e-12))
    expect_true(all(fit$draws$minimum_pair_denominator > 1e-12))
  }
})

test_that("auto resolves to the complete-record law and the historical law warns", {
  fixture <- list(
    data = data.frame(
      a = c(0, 1, 2, 3, 0, 1, 3, 2, 1, 0, 2, 3),
      b = c(0, 1, 1, 3, 0, 2, 3, 2, 1, 0, 2, 3),
      c = c(0, 0, 2, 3, 1, 2, 3, 1, 1, 0, 2, 3)
    ),
    cluster = rep(paste0("g", 1:4), c(2, 3, 4, 3)),
    supports = list(a = 0:3, b = 0:3, c = 0:3)
  )
  det <- mokken_h(fixture$data, supports = fixture$supports,
                  cluster = fixture$cluster, cluster_target = "respondent",
                  warn = FALSE)
  expect_identical(det$estimand$weight_law, "complete_record")
  expect_identical(det$provenance$weight_law, "complete_record")
  explicit <- mokken_h(fixture$data, supports = fixture$supports,
                       cluster = fixture$cluster, cluster_target = "respondent",
                       weight_law = "hierarchical_cluster", warn = FALSE)
  expect_identical(explicit$estimand$weight_law, "hierarchical_cluster")
  expect_identical(det$scale$H, explicit$scale$H)   # the law is provenance only

  # G = 4 is computable on the default lane; the historical lane refuses it.
  expect_no_warning(
    fit <- bb_mokken_h(fixture$data, supports = fixture$supports,
                       cluster = fixture$cluster, cluster_target = "respondent",
                       draws = 40, seed = 3, warn = TRUE)
  )
  expect_identical(fit$interval$validation_status,
                   "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
  expect_identical(fit$interval$cluster_count, 4L)
  expect_identical(fit$interval$status, "OK")
  expect_warning(
    expect_error(
      bb_mokken_h(fixture$data, supports = fixture$supports,
                  cluster = fixture$cluster, cluster_target = "respondent",
                  weight_law = "hierarchical_cluster",
                  draws = 40, seed = 3, warn = FALSE),
      class = "bm_cluster_count_below_validation_range_error"
    ),
    class = "bm_historical_method_warning"
  )
})
