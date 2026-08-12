regularity_ordinal_fixture <- function() {
  data <- data.frame(
    a = c(0, 0, 1, 2, 2, 1),
    b = c(1, 2, 2, 3, 3, 1),
    c = c(-2, 0, 1, 1, 0, -2)
  )
  list(
    data = data,
    supports = list(a = 0:3, b = 1:3, c = c(-2, 0, 1)),
    weights = c(1, 2, 3, 4, 5, 6)
  )
}

regularity_headroom_oracle <- function(fit, item) {
  rows <- fit$pair$item_i == item | fit$pair$item_j == item
  min(fit$pair$cov_max[rows])
}

test_that("ordinal objects retain exact target-weighted category masses", {
  fixture <- regularity_ordinal_fixture()
  fit <- mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    weights = fixture$weights,
    warn = FALSE
  )
  normalized <- fixture$weights / sum(fixture$weights)

  expect_identical(tail(names(fit), 1L), "category_mass")
  expect_identical(fit$estimand$weight_law, "iid_respondent")
  expect_identical(fit$provenance$weight_law, "iid_respondent")
  for (item in names(fixture$supports)) {
    support <- fixture$supports[[item]]
    expected <- vapply(
      support,
      function(score) sum(normalized[fixture$data[[item]] == score]),
      numeric(1)
    )
    names(expected) <- as.character(support)
    expect_equal(fit$category_mass[[item]], expected, tolerance = 1e-15)
    expect_equal(sum(fit$category_mass[[item]]), 1, tolerance = 1e-15)
    expect_identical(names(fit$category_mass[[item]]), as.character(support))
  }
  # Category 3 belongs to a's declared support even though it is unobserved.
  expect_identical(unname(fit$category_mass$a["3"]), 0)
})

test_that("bm_regularity agrees with binary and ordinal engine objects", {
  fixture <- regularity_ordinal_fixture()
  ordinal <- mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    weights = fixture$weights,
    warn = FALSE
  )
  reg <- bm_regularity(ordinal)
  reg <- reg[match(names(fixture$supports), reg$item), , drop = FALSE]
  for (k in seq_len(nrow(reg))) {
    item <- reg$item[k]
    support <- fixture$supports[[item]]
    mass <- ordinal$category_mass[[item]]
    expect_equal(reg$mean_score[k], sum(support * mass), tolerance = 1e-15)
    expect_equal(reg$min_score[k], min(support), tolerance = 0)
    expect_equal(reg$max_score[k], max(support), tolerance = 0)
    expect_equal(
      reg$denominator[k],
      ordinal$item$cov_max_sum[ordinal$item$item == item],
      tolerance = 0
    )
    expect_equal(
      reg$denominator_headroom[k],
      regularity_headroom_oracle(ordinal, item),
      tolerance = 0
    )
  }
  expect_identical(
    reg$denominator_headroom,
    reg$min_pair_denominator
  )

  binary <- mokken_h(srs, warn = FALSE)
  binary_reg <- bm_regularity(binary)
  binary_reg <- binary_reg[match(names(srs), binary_reg$item), , drop = FALSE]
  # The engine accumulates the mean through its weighted-sum path and
  # colMeans() does not, so the two land ~1e-15 apart on some platforms. The
  # claim is that they agree, not that two summation orders round alike.
  expect_equal(binary_reg$mean_score, unname(colMeans(srs)), tolerance = 1e-12)
  expect_identical(binary_reg$min_score, rep(0, ncol(srs)))
  expect_identical(binary_reg$max_score, rep(1, ncol(srs)))
  expect_equal(
    binary_reg$denominator_headroom,
    unname(vapply(names(srs), function(item) {
      regularity_headroom_oracle(binary, item)
    }, numeric(1))),
    tolerance = 0
  )
})

test_that("ordinal marginal plot uses the stored category shares", {
  fixture <- regularity_ordinal_fixture()
  fit <- mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    weights = fixture$weights,
    warn = FALSE
  )
  plot <- bm_plot_margins(fit, type = "marginal")
  expect_s3_class(plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot))
  expect_equal(
    as.numeric(tapply(plot$data$share, plot$data$item, sum)),
    rep(1, length(fixture$supports)),
    tolerance = 1e-15
  )
  for (item in names(fixture$supports)) {
    shown <- plot$data[as.character(plot$data$item) == item, , drop = FALSE]
    shown <- shown[match(as.character(fixture$supports[[item]]),
                         shown$category), , drop = FALSE]
    expect_equal(
      shown$share,
      unname(fit$category_mass[[item]]),
      tolerance = 0
    )
  }
  expect_match(plot$labels$x, "category share")
  normalized_caption <- gsub("[[:space:]]+", " ", plot$labels$caption)
  expect_match(normalized_caption, "category shares sum to one", fixed = TRUE)
  expect_match(bm_alt_text(plot), "Stacked category-share bars")
  expect_false(grepl("ordered margins|gaps", plot$labels$caption))
})

test_that("headroom is canonical and gap is a deprecated alias", {
  fixture <- regularity_ordinal_fixture()
  fit <- mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    weights = fixture$weights,
    warn = FALSE
  )
  reg <- bm_regularity(fit)
  plot <- bm_plot_margins(fit, type = "headroom")
  expect_equal(
    sort(plot$data$denominator_headroom),
    sort(reg$denominator_headroom),
    tolerance = 0
  )
  expect_match(plot$labels$x, "denominator headroom")
  expect_match(plot$labels$caption, "smallest pair maximum covariance")
  expect_match(bm_alt_text(plot), "denominator headroom")
  expect_false(grepl("ordered margins|gaps", paste(
    plot$labels$x, plot$labels$caption, bm_alt_text(plot)
  )))

  expect_warning(
    legacy <- bm_plot_margins(fit, type = "gap"),
    "deprecated"
  )
  expect_equal(
    legacy$data$denominator_headroom,
    plot$data$denominator_headroom,
    tolerance = 0
  )
})

test_that("classroom ordinal regularity is finite and plots category shares", {
  items <- paste0("wb", 1:6)
  fit <- mokken_h(
    classroom_wb[, items],
    item_type = "ordinal",
    supports = bm_supports(classroom_wb, items = items),
    cluster = classroom_wb$class_id,
    cluster_target = "respondent",
    warn = FALSE
  )
  expect_identical(fit$estimand$weight_law, "hierarchical_cluster")
  expect_identical(fit$provenance$weight_law, "hierarchical_cluster")
  reg <- bm_regularity(fit)
  expect_true(all(is.finite(reg$mean_score)))
  expect_true(all(is.finite(reg$min_score)))
  expect_true(all(is.finite(reg$max_score)))
  expect_true(all(is.finite(reg$denominator_headroom)))

  plot <- bm_plot_margins(fit, type = "marginal")
  expect_no_error(ggplot2::ggplot_build(plot))
  expect_equal(
    as.numeric(tapply(plot$data$share, plot$data$item, sum)),
    rep(1, length(items)),
    tolerance = 1e-12
  )
})
