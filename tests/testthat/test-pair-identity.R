pair_identity_data <- function() {
  x <- as.data.frame(expand.grid(rep(list(0:1), 7L)))
  names(x) <- c("a", "b::c", "a::b", "c", "pipe|item", "pct%item", "amp&item")
  x
}

test_that("length-prefixed pair keys are collision-safe", {
  item <- names(pair_identity_data())
  pair <- utils::combn(item, 2L)
  key <- bayesmokken:::.bm_pair_key(pair[1L, ], pair[2L, ])

  expect_length(key, choose(length(item), 2L))
  expect_identical(anyDuplicated(key), 0L)
  expect_false(identical(
    bayesmokken:::.bm_pair_key("a", "b::c"),
    bayesmokken:::.bm_pair_key("a::b", "c")
  ))
})

test_that("pair intervals match the positional draw oracle", {
  x <- pair_identity_data()
  fit <- bb_mokken_h(x, draws = 200, seed = 741, warn = FALSE)
  observed <- bm_pair_h(fit, include_interval = TRUE)

  expected_key <- bayesmokken:::.bm_pair_key(
    fit$empirical$pair$item_i,
    fit$empirical$pair$item_j
  )
  expect_identical(colnames(fit$draws$pair), expected_key)
  expect_identical(colnames(fit$draws$pair_status), expected_key)
  expect_identical(anyDuplicated(expected_key), 0L)

  oracle <- lapply(seq_len(nrow(observed)), function(k) {
    value <- fit$draws$pair[, k]
    value <- value[is.finite(value)]
    c(
      mean = mean(value),
      stats::quantile(
        value, c(0.025, 0.975), names = FALSE, type = 8
      )
    )
  })
  oracle <- do.call(rbind, oracle)

  expect_equal(observed$bb_mean, oracle[, 1L], tolerance = 0)
  expect_equal(observed$bb_lower, oracle[, 2L], tolerance = 0)
  expect_equal(observed$bb_upper, oracle[, 3L], tolerance = 0)

  colliding_display_pairs <- c(
    which(observed$item_i == "a" & observed$item_j == "b::c"),
    which(observed$item_i == "a::b" & observed$item_j == "c")
  )
  expect_false(isTRUE(all.equal(
    oracle[colliding_display_pairs[1L], 2:3],
    oracle[colliding_display_pairs[2L], 2:3],
    tolerance = 0
  )))
})

test_that("pair-long draws expose coordinates and keep target opaque", {
  x <- pair_identity_data()[, c("a", "b::c", "a::b", "c")]
  fit <- bb_mokken_h(x, draws = 40, seed = 745, warn = FALSE)
  long <- bm_draws(fit, what = "pair", format = "long")
  mapping <- unique(long[c("target", "item_i", "item_j")])
  rownames(mapping) <- NULL
  empirical <- fit$empirical$pair
  expected_key <- bayesmokken:::.bm_pair_key(
    as.character(empirical$item_i),
    as.character(empirical$item_j)
  )

  expect_identical(
    names(long),
    c("draw_id", "level", "target", "item_i", "item_j", "value")
  )
  expect_identical(mapping$target, expected_key)
  expect_identical(mapping$item_i, as.character(empirical$item_i))
  expect_identical(mapping$item_j, as.character(empirical$item_j))
  expect_true(all(startsWith(mapping$target, "bm-pair-v1:")))
  for (k in seq_len(nrow(mapping))) {
    expect_equal(
      long$value[long$target == mapping$target[k]],
      fit$draws$pair[, k],
      tolerance = 0
    )
  }
})

test_that("version-0 pair draws use only an exact positional fallback", {
  x <- pair_identity_data()[, c("a", "b::c", "a::b", "c")]
  fit <- bb_mokken_h(x, draws = 120, seed = 744, warn = FALSE)
  legacy_key <- paste(
    fit$empirical$pair$item_i,
    fit$empirical$pair$item_j,
    sep = "::"
  )
  expect_gt(anyDuplicated(legacy_key), 0L)

  colnames(fit$draws$pair) <- legacy_key
  colnames(fit$draws$pair_status) <- legacy_key
  observed <- bm_pair_h(fit, include_interval = TRUE)
  oracle <- lapply(seq_len(ncol(fit$draws$pair)), function(k) {
    value <- fit$draws$pair[, k]
    value <- value[is.finite(value)]
    c(
      mean = mean(value),
      stats::quantile(
        value, c(0.025, 0.975), names = FALSE, type = 8
      )
    )
  })
  oracle <- do.call(rbind, oracle)

  expect_equal(observed$bb_mean, oracle[, 1L], tolerance = 0)
  expect_equal(observed$bb_lower, oracle[, 2L], tolerance = 0)
  expect_equal(observed$bb_upper, oracle[, 3L], tolerance = 0)

  long <- bm_draws(fit, what = "pair", format = "long")
  mapping <- unique(long[c("target", "item_i", "item_j")])
  rownames(mapping) <- NULL
  expect_identical(
    mapping$target,
    bayesmokken:::.bm_pair_key(
      as.character(fit$empirical$pair$item_i),
      as.character(fit$empirical$pair$item_j)
    )
  )
  expect_identical(
    mapping[c("item_i", "item_j")],
    data.frame(
      item_i = as.character(fit$empirical$pair$item_i),
      item_j = as.character(fit$empirical$pair$item_j),
      stringsAsFactors = FALSE
    )
  )

  reordered <- fit
  reordered$draws$pair <- reordered$draws$pair[
    , rev(seq_len(ncol(reordered$draws$pair))), drop = FALSE
  ]
  expect_error(
    bm_pair_h(reordered, include_interval = TRUE),
    class = "bm_invariant_error"
  )
})

test_that("clustered pair intervals use the same collision-safe identity", {
  x <- as.data.frame(expand.grid(rep(list(0:2), 4L)))
  names(x) <- c("a", "b::c", "a::b", "c")
  supports <- stats::setNames(rep(list(0:2), ncol(x)), names(x))
  cluster <- rep(sprintf("g%02d", seq_len(22L)), length.out = nrow(x))
  fit <- bb_mokken_h(
    x,
    item_type = "ordinal",
    supports = supports,
    cluster = cluster,
    cluster_target = "respondent",
    weight_law = "hierarchical_cluster",
    draws = 40,
    seed = 743,
    warn = FALSE
  )
  observed <- bm_pair_h(fit, include_interval = TRUE)
  expected_key <- bayesmokken:::.bm_pair_key(
    observed$item_i,
    observed$item_j
  )

  expect_identical(colnames(fit$draws$pair), expected_key)
  expect_identical(colnames(fit$draws$pair_status), expected_key)
  for (k in seq_len(nrow(observed))) {
    value <- fit$draws$pair[, k]
    value <- value[is.finite(value)]
    quantile <- stats::quantile(
      value, c(0.025, 0.975), names = FALSE, type = 8
    )
    expect_equal(observed$bb_mean[k], mean(value), tolerance = 0)
    expect_equal(observed$bb_lower[k], quantile[1L], tolerance = 0)
    expect_equal(observed$bb_upper[k], quantile[2L], tolerance = 0)
  }
})

test_that("pair extraction rejects non-bijective draw identities", {
  x <- pair_identity_data()[, c("a", "b::c", "a::b", "c")]
  fit <- bb_mokken_h(x, draws = 40, seed = 742, warn = FALSE)
  colnames(fit$draws$pair)[2L] <- colnames(fit$draws$pair)[1L]

  expect_error(
    bm_pair_h(fit, include_interval = TRUE),
    class = "bm_invariant_error"
  )
})
