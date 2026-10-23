make_target_diagnostic_cluster <- function(id, n, kind, shift) {
  pattern <- if (identical(kind, "high")) {
    rbind(c(0, 0, 0), c(2, 2, 2))
  } else {
    rbind(c(0, 1, 2), c(1, 2, 0), c(2, 0, 1))
  }
  x <- pattern[rep(seq_len(nrow(pattern)), length.out = n), , drop = FALSE]
  x <- x + shift
  data.frame(cluster = id, i1 = x[, 1], i2 = x[, 2], i3 = x[, 3])
}

target_diagnostic_counterexample <- function() {
  sizes <- c(6, 9, 12, 24, 30)
  mean_ranks <- c(2, 3, 4, 5, 1)
  kinds <- c("low", "low", "low", "high", "high")
  parts <- Map(
    function(k, n, r, kind) {
      make_target_diagnostic_cluster(
        paste0("g", k), n, kind, 0.001 * r
      )
    },
    seq_along(sizes), sizes, mean_ranks, kinds
  )
  do.call(rbind, parts)
}

test_that("size--mean-score correlation is not treated as target agreement", {
  d <- target_diagnostic_counterexample()
  items <- paste0("i", 1:3)
  supports <- lapply(d[, items], function(z) sort(unique(z)))
  info <- bm_cluster_info(d$cluster, d[, items])

  respondent <- mokken_h(
    d[, items], item_type = "ordinal", supports = supports,
    cluster = d$cluster, cluster_target = "respondent",
    weight_law = "complete_record", warn = FALSE
  )
  equal_cluster <- mokken_h(
    d[, items], item_type = "ordinal", supports = supports,
    cluster = d$cluster, cluster_target = "equal_cluster",
    weight_law = "complete_record", warn = FALSE
  )

  expect_equal(info$size_mean_score_spearman, 0, tolerance = 1e-12)
  expect_identical(info$informative_index, info$size_mean_score_spearman)
  expect_equal(respondent$scale$H, 0.6250012, tolerance = 1e-6)
  expect_equal(equal_cluster$scale$H, 0.2500019, tolerance = 1e-6)
  expect_gt(abs(respondent$scale$H - equal_cluster$scale$H), 0.37)

  printed <- tolower(paste(utils::capture.output(print(info)), collapse = " "))
  expect_false(grepl("nearly the same|choice.*hardly matters", printed))
  expect_match(printed, "does not measure agreement")

  plotted <- bm_plot_clusters(info)
  plot_text <- tolower(paste(
    plotted$labels$subtitle, plotted$labels$caption, bm_alt_text(plotted),
    collapse = " "
  ))
  expect_false(grepl("nearly the same|estimate different quantities", plot_text))
  expect_match(plot_text, "does not compare target h")
})
