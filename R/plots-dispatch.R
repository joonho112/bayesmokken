# Re-export the ggplot2 generics users need, so `autoplot(fit)` works after
# `library(bayesmokken)` alone. Without this the method exists but the generic
# does not, which is a confusing failure to hit in an example.

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

# =============================================================================
# Idiomatic entry points
# =============================================================================

#' Plot a Mokken fit
#'
#' `autoplot()` returns a ggplot; `plot()` draws it. Which figure you get
#' depends on `what`, and the default is the one most people want first.
#'
#' @param object,x A `bm_bb_h` or `bm_empirical_h` object.
#' @param what Which figure: `"scale"`, `"draws"`, `"item"`, `"pair"`,
#'   `"margins"`, `"clusters"`, `"calibration"` or `"stability"`. Bootstrap
#'   figures are unavailable for a deterministic fit.
#' @param ... Passed to the underlying `bm_plot_*()` function.
#'
#' @return `autoplot()` returns a ggplot object; `plot()` returns its input
#'   invisibly, after drawing.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' autoplot(fit)
#' autoplot(fit, what = "item")
#'
#' @family bayesmokken-plots
#' @export
autoplot.bm_bb_h <- function(object,
                             what = c("scale", "draws", "item", "pair",
                                      "margins", "clusters", "calibration",
                                      "stability"),
                             ...) {
  what <- match.arg(what)
  switch(
    what,
    scale = bm_plot_scale_h(object, ...),
    draws = bm_plot_draws(object, ...),
    item = bm_plot_item_h(object, ...),
    pair = bm_plot_pair_h(object, ...),
    margins = bm_plot_margins(object, ...),
    clusters = bm_plot_clusters(object, ...),
    calibration = bm_plot_calibration(object, ...),
    stability = bm_plot_mc_stability(object, ...)
  )
}

#' @rdname autoplot.bm_bb_h
#' @export
autoplot.bm_empirical_h <- function(object,
                                    what = c("item", "pair", "margins",
                                             "clusters"),
                                    ...) {
  what <- match.arg(what)
  switch(
    what,
    item = bm_plot_item_h(object, ...),
    pair = bm_plot_pair_h(object, ...),
    margins = bm_plot_margins(object, ...),
    clusters = bm_plot_clusters(object, ...)
  )
}

#' @rdname autoplot.bm_bb_h
#' @export
plot.bm_bb_h <- function(x, what = "scale", ...) {
  print(autoplot(x, what = what, ...))
  invisible(x)
}

#' @rdname autoplot.bm_bb_h
#' @export
plot.bm_empirical_h <- function(x, what = "item", ...) {
  print(autoplot(x, what = what, ...))
  invisible(x)
}


#' A four-panel overview of a fit
#'
#' Assembles the interval, the bootstrap draws, the item coefficients and one
#' diagnostic panel into a single figure. Requires the suggested `patchwork`
#' package.
#'
#' @param x A `bm_bb_h` object.
#' @param ... Ignored.
#'
#' @return A `patchwork` object, which prints like a ggplot.
#'
#' @examplesIf requireNamespace("patchwork", quietly = TRUE)
#' bm_plot_overview(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-plots
#' @seealso [bm_plot_scale_h()], [bm_plot_draws()], [bm_plot_item_h()]
#' @export
bm_plot_overview <- function(x, ...) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    .bm_abort(
      "bm_plot_overview() needs the 'patchwork' package.",
      hint = "install.packages(\"patchwork\"), or draw the four panels separately."
    )
  }
  s <- bm_scale_h(x)
  clustered <- !is.na(s$n_clusters)

  # Panels carry no captions of their own; one caption for the whole figure
  # avoids four repetitions of the same method identifier.
  p1 <- bm_plot_scale_h(x, caption = NA)
  p2 <- bm_plot_draws(x, caption = NA)
  p3 <- bm_plot_item_h(x, caption = NA)
  p4 <- if (clustered) {
    bm_plot_clusters(x, caption = NA)
  } else {
    bm_plot_margins(x, type = "headroom", caption = NA)
  }
  panel_alt <- c(
    bm_alt_text(p1), bm_alt_text(p2), bm_alt_text(p3), bm_alt_text(p4)
  )
  panel_names <- c(
    "Scale coefficient", "Bootstrap draws", "Items",
    if (clustered) "Clusters and targets" else "Denominator headroom"
  )
  p1 <- p1 + labs(title = "Scale coefficient")
  p2 <- p2 + labs(title = "Bootstrap draws")
  p3 <- p3 + labs(title = "Items")
  p4 <- p4 + labs(title = if (clustered) {
    "Clusters and targets"
  } else {
    "Denominator headroom"
  })

  overview <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2L) +
    patchwork::plot_annotation(
      caption = .bm_caption(NULL, .bm_method_caption(x),
                            .bm_fixed_scale_item_note),
      theme = theme_bayesmokken()
    )
  overview_alt <- paste0(
    "Four-panel overview of one Mokken fit. ",
    paste0("Panel ", seq_along(panel_alt), ", ", panel_names, ": ", panel_alt,
           collapse = " ")
  )
  .bm_attach_alt(overview, overview_alt)
}
