# Branch-complete accessibility checks. These assert the numerical and
# structural content of the generated descriptions, not merely their length.

.alt_binary_fit <- function(draws = 120L) {
  bb_mokken_h(srs, draws = draws, seed = 41, warn = FALSE)
}

.alt_cluster_fit <- function(draws = 120L) {
  items <- paste0("wb", 1:6)
  bb_mokken_h(
    classroom_wb[, items],
    item_type = "ordinal",
    supports = bm_supports(classroom_wb, items = items),
    cluster = as.character(classroom_wb$class_id),
    cluster_target = "respondent",
    weight_law = "hierarchical_cluster",
    draws = draws,
    seed = 41,
    warn = FALSE
  )
}

.alt_build <- function(p) ggplot2::ggplot_build(p)$data


test_that("scale alt covers deterministic, interval, and every comparison row", {
  deterministic <- mokken_h(srs, warn = FALSE)
  det_plot <- bm_plot_scale_h(deterministic, benchmarks = NULL)
  det_alt <- bm_alt_text(det_plot)
  expect_match(det_alt, paste0("H = ", .bm_num(deterministic$scale$H)),
               fixed = TRUE)
  expect_match(det_alt, "No interval was computed", fixed = TRUE)
  expect_match(det_alt, "No reference rules are shown", fixed = TRUE)
  expect_false(grepl("NA", det_alt, fixed = TRUE))

  interval_fit <- .alt_binary_fit()
  interval_plot <- bm_plot_scale_h(interval_fit, benchmarks = NULL)
  interval_alt <- bm_alt_text(interval_plot)
  expect_match(interval_alt, .bm_num(interval_fit$interval$lower), fixed = TRUE)
  expect_match(interval_alt, .bm_num(interval_fit$interval$upper), fixed = TRUE)
  expect_match(interval_alt, .bm_num(interval_fit$interval$raw_lower),
               fixed = TRUE)
  expect_match(interval_alt, "95 percent interval", fixed = TRUE)

  # Distinct values make a first-row-only alt implementation fail loudly.
  deterministic$scale$H <- -0.123
  interval_fit$empirical$scale$H <- 0.456
  interval_fit$interval$point_estimate <- 0.456
  interval_fit$interval$lower <- 0.401
  interval_fit$interval$upper <- 0.509
  interval_fit$interval$raw_lower <- 0.411
  interval_fit$interval$raw_upper <- 0.499
  compared <- bm_plot_scale_h(
    list(deterministic = deterministic, bootstrap = interval_fit),
    benchmarks = NULL
  )
  compared_alt <- bm_alt_text(compared)
  expect_match(compared_alt, "2 fit rows", fixed = TRUE)
  expect_match(compared_alt, "Row 1 (deterministic): H = -0.123", fixed = TRUE)
  expect_match(compared_alt, "No interval was computed", fixed = TRUE)
  expect_match(compared_alt, "Row 2 (bootstrap): H = 0.456", fixed = TRUE)
  expect_match(compared_alt, "0.401 to 0.509", fixed = TRUE)
  expect_match(compared_alt, "0.411 to 0.499", fixed = TRUE)

  no_raw <- bm_alt_text(bm_plot_scale_h(
    interval_fit, show_raw = FALSE, benchmarks = NULL
  ))
  expect_false(grepl("displayed raw interval", no_raw, fixed = TRUE))
})


test_that("undefined scale values have an honest warning-free fallback", {
  fit <- mokken_h(srs, warn = FALSE)
  fit$scale$H <- NA_real_
  expect_no_warning(p <- bm_plot_scale_h(fit, benchmarks = NULL))
  expect_no_warning(ggplot2::ggplot_build(p))
  alt <- bm_alt_text(p)
  expect_match(alt, "H is undefined", fixed = TRUE)
  expect_match(alt, "No interval was computed", fixed = TRUE)
  expect_false(grepl("H = NA|from NA|to NA", alt))
})


test_that("item alt maps finite extrema to their actual items in both orders", {
  fit <- mokken_h(srs, warn = FALSE)
  fit$item$H_i <- seq(0.05, 0.25, length.out = nrow(fit$item))
  fit$item$H_i[3L] <- -0.321
  fit$item$H_i[8L] <- 0.654
  smallest <- as.character(fit$item$item[3L])
  largest <- as.character(fit$item$item[8L])

  original <- bm_plot_item_h(fit, sort = FALSE)
  original_alt <- bm_alt_text(original)
  expect_match(original_alt, "original data order", fixed = TRUE)
  expect_match(original_alt,
               paste0("-0.321 for ", smallest), fixed = TRUE)
  expect_match(original_alt,
               paste0("0.654 for ", largest), fixed = TRUE)
  expect_identical(as.character(original$data$item),
                   as.character(fit$item$item))

  ascending <- bm_plot_item_h(fit, sort = TRUE)
  ascending_alt <- bm_alt_text(ascending)
  expect_match(ascending_alt, "ascending coefficient order", fixed = TRUE)
  expect_match(ascending_alt,
               paste0("-0.321 for ", smallest), fixed = TRUE)
  expect_match(ascending_alt,
               paste0("0.654 for ", largest), fixed = TRUE)
  expect_true(all(diff(ascending$data$H) >= 0))
})


test_that("raw item intervals use an opaque dark dashed print stroke", {
  p <- bm_plot_item_h(.alt_binary_fit(), show_interval = TRUE)
  ranges <- Filter(
    function(layer) all(c("xmin", "xmax") %in% names(layer)),
    .alt_build(p)
  )
  expect_length(ranges, 1L)
  raw <- ranges[[1L]]
  expect_true(all(raw$linetype == "dashed"))
  expect_true(all(raw$colour == unname(.bm_colours[["ink"]])))
  expect_true(all(is.na(raw$alpha)))
  expect_true(all(raw$linewidth >= 0.55))

  if (requireNamespace("colorspace", quietly = TRUE)) {
    for (transform in list(colorspace::deutan, colorspace::desaturate)) {
      transformed <- transform(unique(raw$colour))
      contrast <- colorspace::contrast_ratio(transformed, "white")
      expect_gte(as.numeric(contrast), 7)
    }
  }
})


test_that("pair alt follows layout, finite range, and value-display mode", {
  fit <- mokken_h(srs, warn = FALSE)
  pair <- bm_pair_h(fit)
  finite <- pair$H[is.finite(pair$H)]

  lower <- bm_plot_pair_h(fit, layout = "lower", show_values = FALSE)
  lower_alt <- bm_alt_text(lower)
  expect_match(lower_alt, "Lower-triangular heatmap", fixed = TRUE)
  expect_match(lower_alt, paste0(nrow(pair), " displayed cells"), fixed = TRUE)
  expect_match(lower_alt, paste0(length(finite), " finite"), fixed = TRUE)
  expect_match(lower_alt, .bm_num(min(finite)), fixed = TRUE)
  expect_match(lower_alt, .bm_num(max(finite)), fixed = TRUE)
  expect_match(lower_alt, "Plus and minus glyphs", fixed = TRUE)

  full <- bm_plot_pair_h(fit, layout = "full", show_values = TRUE)
  full_alt <- bm_alt_text(full)
  expect_match(full_alt, "Full symmetric off-diagonal heatmap", fixed = TRUE)
  expect_match(full_alt, paste0(2L * nrow(pair), " displayed cells"),
               fixed = TRUE)
  expect_match(full_alt, "numeric coefficient values are printed", fixed = TRUE)
  value_layers <- Filter(
    function(layer) "label" %in% names(layer), .alt_build(full)
  )
  expect_length(value_layers, 1L)
  expect_identical(nrow(value_layers[[1L]]), 2L * length(finite))
})


test_that("pair sign glyphs carry sign without colliding with special states", {
  fit <- mokken_h(srs, warn = FALSE)
  fit$pair$H_ij[1L] <- 0
  fit$pair$H_ij[2L] <- NA_real_
  fit$pair$H_ij[3L] <- 0.25
  fit$pair$denominator_status[3L] <- "zero"
  p <- bm_plot_pair_h(fit, show_values = FALSE)

  eligible <- !p$data$boundary & is.finite(p$data$H) & p$data$H != 0
  expected <- ifelse(p$data$H[eligible] > 0, "+", "\u2212")
  expect_identical(p$data$sign_glyph[eligible], expected)
  expect_true(all(is.na(p$data$sign_glyph[!eligible])))
  expect_true(is.na(p$data$sign_glyph[1L]))       # exact zero
  expect_true(is.na(p$data$sign_glyph[2L]))       # undefined
  expect_true(is.na(p$data$sign_glyph[3L]))       # boundary cross

  layers <- .alt_build(p)
  glyph <- Filter(function(layer) "label" %in% names(layer), layers)
  cross <- Filter(
    function(layer) "shape" %in% names(layer) && any(layer$shape == 4L),
    layers
  )
  expect_length(glyph, 1L)
  expect_setequal(unique(glyph[[1L]]$label), c("+", "\u2212"))
  expect_identical(nrow(glyph[[1L]]), sum(eligible))
  expect_length(cross, 1L)
  expect_identical(nrow(cross[[1L]]), sum(p$data$boundary))

  if (requireNamespace("colorspace", quietly = TRUE)) {
    tiles <- layers[[1L]]
    idx <- match(
      paste(glyph[[1L]]$x, glyph[[1L]]$y),
      paste(tiles$x, tiles$y)
    )
    expect_false(anyNA(idx))
    for (transform in list(colorspace::deutan, colorspace::desaturate)) {
      ink <- transform(glyph[[1L]]$colour)
      fill <- transform(tiles$fill[idx])
      contrast <- colorspace::contrast_ratio(ink, fill)
      expect_gte(min(as.numeric(contrast)), 3)
    }
  }
})


test_that("all-undefined pairs use a finite symmetric fallback without warnings", {
  fit <- mokken_h(srs, warn = FALSE)
  fit$pair$H_ij[] <- NA_real_
  expect_no_warning(p <- bm_plot_pair_h(fit, show_values = FALSE))
  expect_no_warning(built <- ggplot2::ggplot_build(p))
  limits <- built$plot$scales$get_scales("fill")$get_limits()
  expect_equal(as.numeric(limits), c(-1, 1), tolerance = 0)
  expect_false(any(vapply(built$data, function(layer) {
    "label" %in% names(layer)
  }, logical(1))))
  alt <- bm_alt_text(p)
  expect_match(alt,
               paste0("All ", nrow(fit$pair), " unique-pair coefficients are undefined"),
               fixed = TRUE)
  expect_match(alt, "neutral fill scale centered at zero", fixed = TRUE)
})


test_that("overview carries explicit numeric alt for all four panels", {
  skip_if_not_installed("patchwork")
  fit <- .alt_binary_fit()
  overview <- bm_plot_overview(fit)
  alt <- bm_alt_text(overview)
  expect_identical(attr(overview, "bm_alt", exact = TRUE), alt)
  expect_match(alt, "Four-panel overview", fixed = TRUE)
  expect_match(alt, "Panel 1, Scale coefficient", fixed = TRUE)
  expect_match(alt, "Panel 2, Bootstrap draws", fixed = TRUE)
  expect_match(alt, "Panel 3, Items", fixed = TRUE)
  expect_match(alt, "Panel 4, Denominator headroom", fixed = TRUE)
  expect_match(alt, paste0("H = ", .bm_num(fit$interval$point_estimate)),
               fixed = TRUE)
  expect_match(alt, paste0(nrow(fit$empirical$item), " item coefficients"),
               fixed = TRUE)

  clustered <- bm_plot_overview(.alt_cluster_fit())
  clustered_alt <- bm_alt_text(clustered)
  expect_match(clustered_alt, "Panel 4, Clusters and targets", fixed = TRUE)
  expect_match(clustered_alt, "30 clusters", fixed = TRUE)
})
