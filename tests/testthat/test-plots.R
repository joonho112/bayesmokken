# =============================================================================
# Figures
#
# These tests check the RELATIONSHIP between the data and the built plot, which
# is the thing that goes wrong silently. Twice before, in other projects, a
# chart shipped with its axis mapping and its caption saying opposite things,
# and every check passed: the data check looked at the numbers going in, the
# render check looked at the picture coming out, and nothing looked at the
# relationship between them.
#
# So: pull the real boundary rows out of the data, and find them in
# ggplot_build(p)$data. Never "the figure looks right".
#
# The assertions are the ones written down in
# inst/extdata/spec/visual-spec.yml, moved into code.
# =============================================================================

bin_fit <- function(draws = 400) {
  bb_mokken_h(srs, draws = draws, seed = 1, warn = FALSE)
}
clu_fit <- function(target = "respondent", draws = 400) {
  items <- paste0("wb", 1:6)
  bb_mokken_h(classroom_wb[, items], item_type = "ordinal",
              supports = bm_supports(classroom_wb, items = items),
              cluster = as.character(classroom_wb$class_id),
              cluster_target = target, weight_law = "hierarchical_cluster",
              draws = draws, seed = 1, warn = FALSE)
}
built <- function(p) ggplot2::ggplot_build(p)$data

# --- every figure is a ggplot and renders ------------------------------------

test_that("all eight figures build without error", {
  f <- bin_fit(); cf <- clu_fit()
  plots <- list(
    bm_plot_scale_h(f), bm_plot_draws(f), bm_plot_item_h(f),
    bm_plot_pair_h(f), bm_plot_calibration(f), bm_plot_mc_stability(f),
    bm_plot_clusters(cf), bm_plot_margins(f),
    bm_plot_margins(f, type = "headroom")
  )
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
    expect_true(nzchar(bm_alt_text(p)))
  }
})

# --- bm_plot_item_h: the ordering assertion ----------------------------------

test_that("ascending item coefficients run from bottom to top", {
  f <- bin_fit()
  it <- bm_item_h(f)
  p <- bm_plot_item_h(f)
  pts <- built(p)[[length(built(p))]]          # the point layer is drawn last

  # Boundary rows from the DATA, then located in the BUILT plot.
  smallest <- it$item[which.min(it$H)]
  largest <- it$item[which.max(it$H)]
  levs <- levels(factor(it$item, levels = it$item))

  expect_identical(levs[1L], smallest)
  expect_identical(levs[length(levs)], largest)
  # y is the discrete position: 1 is the bottom of the panel.
  expect_equal(pts$y[which.min(pts$x)], 1)
  expect_equal(pts$y[which.max(pts$x)], length(levs))
})

test_that("item intervals are drawn dashed and the caption says uncalibrated", {
  p <- bm_plot_item_h(bin_fit(), show_interval = TRUE)
  layers <- built(p)
  ranges <- Filter(function(d) "xmin" %in% names(d), layers)
  expect_gt(length(ranges), 0L)
  for (d in ranges) expect_true(all(d$linetype == "dashed"))
  expect_match(p$labels$caption, "uncalibrated")
})

test_that("item intervals and benchmark lines are opt-in", {
  p <- bm_plot_item_h(bin_fit())
  expect_false(any(vapply(
    built(p), function(layer) "xmin" %in% names(layer), logical(1)
  )))
  expect_false(grepl("uncalibrated", p$labels$caption, ignore.case = TRUE))
  expect_match(p$labels$caption, "declared fixed scale", ignore.case = TRUE)
})

test_that("boundary items get a non-filled shape", {
  # A constant item forces a zero denominator.
  d <- srs
  d$SRS1 <- 0L
  f <- suppressWarnings(mokken_h(d, warn = FALSE))
  p <- bm_plot_item_h(f, show_interval = FALSE)
  pts <- built(p)[[length(built(p))]]
  it <- bm_item_h(f, include_interval = FALSE)
  bad <- which(it$denominator_status != "positive")
  expect_gt(length(bad), 0L)
  expect_false(any(pts$shape[bad] == 16L))
})

# --- bm_plot_scale_h ----------------------------------------------------------

test_that("the scale plot marks the point estimate exactly", {
  f <- bin_fit()
  p <- bm_plot_scale_h(f)
  pts <- built(p)[[length(built(p))]]
  expect_equal(pts$x[1L], f$interval$point_estimate, tolerance = 1e-12)
})

test_that("raw and calibrated segments share a midpoint", {
  f <- bin_fit()
  itv <- f$interval
  expect_equal((itv$lower + itv$upper) / 2,
               (itv$raw_lower + itv$raw_upper) / 2, tolerance = 1e-12)
  p <- bm_plot_scale_h(f)
  segs <- Filter(function(d) "xmin" %in% names(d), built(p))
  expect_length(segs, 2L)                       # raw and calibrated
})

test_that("one reference rule is drawn per benchmark", {
  p <- bm_plot_scale_h(bin_fit(), benchmarks = c(0.2, 0.3, 0.4))
  rules <- Filter(function(d) "xintercept" %in% names(d), built(p))
  expect_identical(sort(rules[[1L]]$xintercept), c(0.2, 0.3, 0.4))
})

# --- bm_plot_draws ------------------------------------------------------------

test_that("the draws plot uses every finite draw and marks four endpoints", {
  f <- bin_fit()
  finite <- sum(is.finite(f$draws$scale))
  p <- bm_plot_draws(f, type = "histogram")
  layers <- built(p)
  expect_equal(sum(layers[[1L]]$count), finite)

  rules <- Filter(function(d) "xintercept" %in% names(d), layers)
  xs <- sort(unlist(lapply(rules, function(d) d$xintercept)))
  expect_length(xs, 5L)                        # 2 raw, 2 calibrated, 1 point
  expect_true(all(c(f$interval$lower, f$interval$upper,
                    f$interval$raw_lower, f$interval$raw_upper,
                    f$interval$point_estimate) %in% xs))
})

test_that("the calibrated rules sit outside the raw rules for binary fits", {
  itv <- bin_fit()$interval
  expect_lt(itv$lower, itv$raw_lower)
  expect_gt(itv$upper, itv$raw_upper)
})

test_that("the calibrated rules sit inside the raw rules for clustered fits", {
  itv <- clu_fit()$interval
  expect_gt(itv$lower, itv$raw_lower)
  expect_lt(itv$upper, itv$raw_upper)
})

# --- bm_plot_pair_h -----------------------------------------------------------

test_that("the pair heatmap really is a solid lower triangle", {
  # Counting tiles is not enough: the first version of this figure had the
  # right NUMBER of tiles scattered into a ragged shape, because pairs are
  # stored in the original item order while the axes are sorted by
  # coefficient. Check the geometry.
  f <- bin_fit()
  pr <- bm_pair_h(f)
  p <- bm_plot_pair_h(f)
  tiles <- built(p)[[1L]]
  expect_identical(nrow(tiles), nrow(pr))

  # Every drawn cell must sit strictly below the diagonal in display order,
  # and every such cell must be present exactly once.
  n <- length(levels(p$data$row))
  row_i <- as.integer(p$data$row)
  col_j <- as.integer(p$data$col)
  expect_true(all(row_i > col_j))
  expect_identical(nrow(unique(cbind(row_i, col_j))), as.integer(choose(n, 2)))

  rng <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_limits()
  expect_equal(mean(rng), 0, tolerance = 1e-12)
})

test_that("both plots use the same item order", {
  f <- bin_fit()
  item_levels <- bm_item_h(f, include_interval = FALSE)$item
  p <- bm_plot_pair_h(f)
  expect_identical(levels(p$data$col), item_levels)
  expect_identical(levels(p$data$row), item_levels)
})

# --- bm_plot_calibration ------------------------------------------------------

test_that("the calibration curves run in the documented directions", {
  b <- built(bm_plot_calibration(mode = "binary"))
  bl <- Filter(function(d) "y" %in% names(d) && nrow(d) > 10, b)[[1L]]
  expect_true(all(bl$y > 1))
  expect_true(bl$y[1L] > bl$y[nrow(bl)])       # decreasing toward 1

  cl <- built(bm_plot_calibration(mode = "clustered"))
  cll <- Filter(function(d) "y" %in% names(d) && nrow(d) > 10, cl)[[1L]]
  expect_true(all(cll$y < 1))
  expect_true(cll$y[1L] < cll$y[nrow(cll)])    # increasing toward 1
})

test_that("a supplied fit is marked at its own multiplier", {
  f <- bin_fit()
  p <- bm_plot_calibration(f)
  pts <- Filter(function(d) "shape" %in% names(d) && nrow(d) == 1L, built(p))
  expect_gt(length(pts), 0L)
  expect_equal(pts[[1L]]$y, f$interval$width_multiplier, tolerance = 1e-9)
})

test_that("the calibration figure works with no fit at all", {
  expect_s3_class(bm_plot_calibration(mode = "clustered"), "ggplot")
})

# --- bm_plot_mc_stability -----------------------------------------------------

test_that("the running endpoints end at the reported interval", {
  f <- bin_fit()
  p <- bm_plot_mc_stability(f)
  last <- Filter(function(d) "shape" %in% names(d), built(p))[[1L]]
  expect_equal(sort(last$y), sort(c(f$interval$lower, f$interval$upper)),
               tolerance = 1e-9)
})

# --- bm_plot_clusters ---------------------------------------------------------

test_that("cluster weights sum to one under both targets", {
  info <- bm_cluster_info(classroom_wb$class_id,
                          data = classroom_wb[, paste0("wb", 1:6)])
  expect_equal(sum(info$table$weight_respondent), 1, tolerance = 1e-12)
  expect_equal(sum(info$table$weight_equal), 1, tolerance = 1e-12)
  expect_length(unique(info$table$weight_equal), 1L)
  expect_identical(which.max(info$table$weight_respondent), 1L)  # size-sorted
})

test_that("a fit-derived cluster figure does not claim an index it lacks", {
  # A fitted object keeps cluster sizes but not labels or item data, so the
  # informative-size index cannot be computed from it. The subtitle must not
  # print "NA" as if it were a value.
  p <- bm_plot_clusters(clu_fit())
  expect_false(grepl("NA", p$labels$subtitle))
  expect_match(p$labels$caption, "does not keep the cluster labels")

  q <- bm_plot_clusters(bm_cluster_info(
    classroom_wb$class_id, data = classroom_wb[, paste0("wb", 1:6)]))
  expect_match(q$labels$subtitle, "size/mean-score Spearman")
})

# --- bm_plot_margins ----------------------------------------------------------

test_that("binary margins equal the column means of the scored data", {
  f <- mokken_h(srs, warn = FALSE)
  p <- bm_plot_margins(f)
  expect_equal(sort(p$data$mean_score), sort(unname(colMeans(srs))),
               tolerance = 1e-12)
})

test_that("headroom and marginal modes work for both item types", {
  fit <- clu_fit()
  expect_s3_class(bm_plot_margins(fit, type = "headroom"), "ggplot")
  expect_s3_class(bm_plot_margins(fit, type = "marginal"), "ggplot")
  expect_warning(
    expect_s3_class(bm_plot_margins(fit, type = "gap"), "ggplot"),
    "deprecated"
  )
})

# --- dispatch, alt text, theme ------------------------------------------------

test_that("autoplot and plot dispatch to the right figures", {
  f <- bin_fit()
  expect_s3_class(ggplot2::autoplot(f), "ggplot")
  expect_s3_class(ggplot2::autoplot(f, what = "item"), "ggplot")
  expect_s3_class(ggplot2::autoplot(mokken_h(srs, warn = FALSE)), "ggplot")
  withr::with_pdf(
    tempfile(fileext = ".pdf"),
    expect_invisible(plot(f))
  )
})

test_that("every figure carries generated alt text", {
  f <- bin_fit()
  for (p in list(bm_plot_scale_h(f), bm_plot_draws(f), bm_plot_item_h(f),
                 bm_plot_pair_h(f), bm_plot_calibration(f),
                 bm_plot_mc_stability(f), bm_plot_margins(f))) {
    alt <- bm_alt_text(p)
    expect_true(is.character(alt) && nchar(alt) > 30)
  }
})

test_that("captions never exceed one line's worth without wrapping", {
  for (p in list(bm_plot_scale_h(bin_fit()), bm_plot_draws(bin_fit()),
                 bm_plot_item_h(bin_fit()))) {
    cap <- p$labels$caption
    if (is.null(cap)) next
    expect_lte(max(nchar(strsplit(cap, "\n")[[1L]])), 110L)
  }
})

test_that("caption = NA removes the caption", {
  expect_null(bm_plot_scale_h(bin_fit(), caption = NA)$labels$caption)
})

test_that("figures remain plain ggplots that users can extend", {
  p <- bm_plot_item_h(bin_fit()) + ggplot2::labs(title = "custom")
  expect_identical(p$labels$title, "custom")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("the palette is the documented length and format", {
  expect_length(bm_palette(3), 3L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", bm_palette(5))))
  expect_length(bm_palette(10, type = "sequential"), 10L)
})

test_that("deterministic fits refuse bootstrap figures with an explanation", {
  det <- mokken_h(srs, warn = FALSE)
  expect_error(bm_plot_draws(det), class = "bm_input_error")
  expect_error(bm_plot_mc_stability(det), class = "bm_input_error")
})
