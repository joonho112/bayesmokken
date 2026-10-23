visual_contract <- function() {
  yaml::read_yaml(.bm_test_inst_file(
    "extdata", "spec", "visual-spec.yml"
  ))
}

.visual_binary_fit <- NULL
.visual_cluster_fit <- NULL
.visual_outside_fit <- NULL
.visual_boundary_fit <- NULL

visual_binary_fit <- function() {
  if (is.null(.visual_binary_fit)) {
    .visual_binary_fit <<- bb_mokken_h(
      srs, draws = 120, seed = 4101, warn = FALSE
    )
  }
  .visual_binary_fit
}

visual_cluster_fit <- function() {
  if (is.null(.visual_cluster_fit)) {
    items <- paste0("wb", 1:6)
    .visual_cluster_fit <<- bb_mokken_h(
      classroom_wb[, items],
      item_type = "ordinal",
      supports = bm_supports(classroom_wb, items = items),
      cluster = classroom_wb$class_id,
      cluster_target = "respondent",
      draws = 20,
      seed = 4102,
      warn = FALSE
    )
  }
  .visual_cluster_fit
}

# A complete-record fit with ten clusters: computable, outside the confirmed
# 22-320 range, and therefore marked OUTSIDE_CONFIRMED_CLUSTER_COUNT.
visual_outside_fit <- function() {
  if (is.null(.visual_outside_fit)) {
    block <- data.frame(
      a = c(0, 1, 2),
      b = c(0, 2, 1),
      c = c(1, 0, 2)
    )
    data <- block[rep(seq_len(nrow(block)), times = 10L), , drop = FALSE]
    cluster <- rep(sprintf("g%03d", seq_len(10L)), each = nrow(block))
    .visual_outside_fit <<- bb_mokken_h(
      data,
      item_type = "ordinal",
      supports = list(a = 0:2, b = 0:2, c = 0:2),
      cluster = cluster,
      cluster_target = "respondent",
      draws = 20,
      seed = 4103,
      warn = FALSE
    )
  }
  .visual_outside_fit
}

visual_boundary_fit <- function() {
  if (is.null(.visual_boundary_fit)) {
    data <- srs
    data[[1L]] <- 0L
    .visual_boundary_fit <<- suppressWarnings(
      mokken_h(data, warn = FALSE)
    )
  }
  .visual_boundary_fit
}

visual_cluster_info <- function() {
  bm_cluster_info(
    classroom_wb$class_id,
    data = classroom_wb[, paste0("wb", 1:6)]
  )
}

visual_mode_runners <- function() {
  list(
    bm_plot_scale_h = list(
      single_fit = function() bm_plot_scale_h(visual_binary_fit()),
      fit_comparison = function() bm_plot_scale_h(list(
        first = visual_binary_fit(), second = visual_binary_fit()
      ))
    ),
    bm_plot_draws = list(
      density = function() bm_plot_draws(
        visual_binary_fit(), type = "density"
      ),
      histogram_with_rug = function() bm_plot_draws(
        visual_binary_fit(), type = "histogram", rug = TRUE
      )
    ),
    bm_plot_item_h = list(
      point_only = function() bm_plot_item_h(
        visual_binary_fit(), show_interval = FALSE
      ),
      interval_with_benchmarks = function() bm_plot_item_h(
        visual_binary_fit(), show_interval = TRUE, benchmarks = c(0.3, 0.4)
      )
    ),
    bm_plot_pair_h = list(
      lower_values = function() bm_plot_pair_h(
        visual_binary_fit(), layout = "lower", show_values = TRUE
      ),
      full_no_values = function() bm_plot_pair_h(
        visual_binary_fit(), layout = "full", show_values = FALSE
      ),
      lower_boundary = function() bm_plot_pair_h(
        visual_boundary_fit(), layout = "lower", show_values = TRUE
      )
    ),
    bm_plot_calibration = list(
      binary = function() bm_plot_calibration(mode = "binary"),
      clustered_confirmed = function() bm_plot_calibration(
        visual_cluster_fit()
      ),
      clustered_outside = function() bm_plot_calibration(
        visual_outside_fit()
      ),
      two_stage_historical = function() bm_plot_calibration(
        mode = "clustered_two_stage"
      )
    ),
    bm_plot_mc_stability = list(
      running_endpoints = function() bm_plot_mc_stability(
        visual_binary_fit()
      )
    ),
    bm_plot_clusters = list(
      sizes = function() bm_plot_clusters(
        visual_cluster_info(), show_targets = FALSE
      ),
      implied_weights = function() bm_plot_clusters(
        visual_cluster_info(), show_targets = TRUE
      )
    ),
    bm_plot_margins = list(
      binary_marginal = function() bm_plot_margins(
        visual_binary_fit(), type = "marginal"
      ),
      ordinal_marginal = function() bm_plot_margins(
        visual_cluster_fit(), type = "marginal"
      ),
      headroom = function() bm_plot_margins(
        visual_cluster_fit(), type = "headroom"
      ),
      gap_alias = function() suppressWarnings(bm_plot_margins(
        visual_cluster_fit(), type = "gap"
      ))
    ),
    bm_plot_overview = list(
      unclustered = function() bm_plot_overview(visual_binary_fit()),
      clustered = function() bm_plot_overview(visual_cluster_fit())
    )
  )
}

visual_layer_geom <- function(layer) {
  geom_class <- class(layer$geom)[1L]
  stat_class <- class(layer$stat)[1L]
  if (identical(stat_class, "StatDensity")) return("geom_density")
  if (identical(geom_class, "GeomBar") &&
      identical(stat_class, "StatBin")) return("geom_histogram")
  if (geom_class %in% c("GeomBar", "GeomCol")) return("geom_col")
  switch(
    geom_class,
    GeomVline = "geom_vline",
    GeomHline = "geom_hline",
    GeomLinerange = "geom_linerange",
    GeomPoint = "geom_point",
    GeomRug = "geom_rug",
    GeomTile = "geom_tile",
    GeomText = "geom_text",
    GeomLine = "geom_line",
    GeomSegment = "geom_segment",
    NA_character_
  )
}

test_that("the YAML is an executable review contract for every export", {
  spec <- visual_contract()
  cards <- spec$figures
  exports <- sort(grep(
    "^bm_plot_", getNamespaceExports("bayesmokken"), value = TRUE
  ))
  runners <- visual_mode_runners()
  required <- unlist(
    spec$review_contract$required_figure_fields,
    use.names = FALSE
  )

  expect_identical(
    spec$review_contract$consumed_by,
    "tests/testthat/test-visual-contract.R"
  )
  expect_setequal(names(cards), exports)
  expect_setequal(names(runners), exports)
  for (figure in exports) {
    expect_true(
      all(required %in% names(cards[[figure]])),
      info = paste(figure, "is missing a required contract field")
    )
    expect_setequal(
      names(cards[[figure]]$modes),
      names(runners[[figure]])
    )
  }
})

test_that("every declared mode constructs its documented geometry", {
  spec <- visual_contract()
  runners <- visual_mode_runners()

  for (figure in names(runners)) {
    if (identical(figure, "bm_plot_overview")) {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        expect_error(
          runners[[figure]]$unclustered(),
          class = "bm_input_error"
        )
        next
      }
      for (run in runners[[figure]]) {
        plot <- run()
        expect_s3_class(plot, "patchwork")
      }
      next
    }

    observed <- character()
    for (run in runners[[figure]]) {
      plot <- run()
      expect_s3_class(plot, "ggplot")
      expect_no_error(ggplot2::ggplot_build(plot))
      observed <- c(
        observed,
        vapply(plot$layers, visual_layer_geom, character(1))
      )
    }
    expect_setequal(
      unique(observed[!is.na(observed)]),
      unlist(spec$figures[[figure]]$geoms, use.names = FALSE)
    )
  }
})

test_that("cluster calibration distinguishes confirmation from extrapolation", {
  spec <- visual_contract()$figures$bm_plot_calibration$validation
  curve_lines <- function(plot) {
    built <- ggplot2::ggplot_build(plot)$data
    line_index <- which(vapply(
      plot$layers,
      function(layer) inherits(layer$geom, "GeomLine"),
      logical(1)
    ))
    lines <- built[line_index]
    list(
      solid = Filter(function(data) all(data$linetype == "solid"), lines),
      dashed = Filter(function(data) all(data$linetype == "dashed"), lines)
    )
  }

  # Default complete-record adjustment: solid over 22-320, dashed outside.
  plot <- bm_plot_calibration(mode = "clustered", range = c(5, 400))
  lines <- curve_lines(plot)
  lo <- spec$complete_record$confirmed_cluster_min
  hi <- spec$complete_record$confirmed_cluster_max
  expect_length(lines$solid, 1L)
  expect_equal(range(lines$solid[[1L]]$x), c(lo, hi), tolerance = 0)
  expect_true(all(lines$solid[[1L]]$y > 1))
  expect_length(lines$dashed, 2L)
  expect_true(any(vapply(lines$dashed, function(data) min(data$x) < lo, logical(1))))
  expect_true(any(vapply(lines$dashed, function(data) max(data$x) > hi, logical(1))))
  expect_true(all(vapply(
    lines$dashed,
    function(data) all(data$x <= lo | data$x >= hi),
    logical(1)
  )))
  expect_match(plot$labels$caption, "confirmed\\s+cluster-count\\s+range\\s+G = 22-320")
  expect_match(plot$labels$caption, "extrapolation")
  expect_match(bm_alt_text(plot), "solid over the confirmed G = 22-320")
  expect_match(bm_alt_text(plot), "dashed.*extrapolation")

  # Historical two-stage shrinkage: solid over 22-80, below one.
  plot <- bm_plot_calibration(mode = "clustered_two_stage", range = c(5, 200))
  lines <- curve_lines(plot)
  lo <- spec$two_stage$confirmed_cluster_min
  hi <- spec$two_stage$confirmed_cluster_max
  expect_length(lines$solid, 1L)
  expect_equal(range(lines$solid[[1L]]$x), c(lo, hi), tolerance = 0)
  expect_true(all(lines$solid[[1L]]$y < 1))
  expect_length(lines$dashed, 2L)
  expect_match(plot$labels$caption, "confirmed\\s+cluster-count\\s+range\\s+G = 22-80")
  expect_match(bm_alt_text(plot), "solid over the confirmed G = 22-80")
})

test_that("a fit outside the confirmed range carries its recorded validation metadata into the plot", {
  fit <- visual_outside_fit()
  expect_identical(
    fit$interval$validation_status,
    "OUTSIDE_CONFIRMED_CLUSTER_COUNT"
  )
  expect_identical(fit$interval$validated_cluster_min, 22L)
  expect_identical(fit$interval$validated_cluster_max, 320L)

  plot <- bm_plot_calibration(fit)
  built <- ggplot2::ggplot_build(plot)$data
  point_index <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  ))
  expect_length(point_index, 1L)
  expect_equal(built[[point_index]]$x, 10, tolerance = 0)
  expect_equal(
    built[[point_index]]$y,
    fit$interval$width_multiplier,
    tolerance = 1e-12
  )
  expect_match(plot$labels$caption, "G =\\s*10")
  expect_match(plot$labels$caption, "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
  expect_match(bm_alt_text(plot), "G = 10")
  expect_match(bm_alt_text(plot), "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
})

test_that("MC, cluster, and margin review claims match their data", {
  spec <- visual_contract()$figures
  expect_setequal(
    unlist(spec$bm_plot_mc_stability$geoms, use.names = FALSE),
    c("geom_hline", "geom_line", "geom_point")
  )
  expect_false(grepl(
    "ribbon", paste(unlist(spec$bm_plot_mc_stability), collapse = " "),
    ignore.case = TRUE
  ))

  cluster_plot <- bm_plot_clusters(visual_cluster_info())
  expect_match(cluster_plot$labels$caption, "does not compare target H values")
  expect_match(cluster_plot$labels$caption, "fit both\\s+targets")

  ordinal <- bm_plot_margins(visual_cluster_fit(), type = "marginal")
  expect_equal(
    as.numeric(tapply(ordinal$data$share, ordinal$data$item, sum)),
    rep(1, length(unique(ordinal$data$item))),
    tolerance = 1e-12
  )
  headroom <- bm_plot_margins(visual_cluster_fit(), type = "headroom")
  expect_equal(
    sort(headroom$data$denominator_headroom),
    sort(bm_regularity(visual_cluster_fit())$denominator_headroom),
    tolerance = 0
  )
  expect_warning(
    legacy <- bm_plot_margins(visual_cluster_fit(), type = "gap"),
    "deprecated"
  )
  expect_equal(
    legacy$data$denominator_headroom,
    headroom$data$denominator_headroom,
    tolerance = 0
  )
})
