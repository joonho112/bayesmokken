# =============================================================================
# The two figures that show what the calibration did
#
# These carry the package's central message. bm_plot_draws() in particular
# shows the whole logic in one picture: bootstrap distribution, quantiles,
# width correction, final interval.
# =============================================================================

#' Plot the scale coefficient and its interval
#'
#' Draws the scale coefficient as a point with its calibrated interval, the
#' uncorrected interval beneath it, and the conventional benchmarks as
#' reference rules. Several fits can be shown together.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object, or a named list of them to
#'   compare.
#' @param benchmarks Numeric vector of reference values, or `NULL` for none.
#' @param show_raw Draw the uncorrected interval beneath the calibrated one.
#'   Keeping this `TRUE` is what makes the correction visible.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_plot_scale_h(fit)
#'
#' \donttest{
#' raw <- bb_mokken_h(srs, draws = 200, seed = 1, interval = "equal_tail")
#' bm_plot_scale_h(list(calibrated = fit, "equal tail" = raw))
#' }
#'
#' @family bayesmokken-plots
#' @seealso [bm_plot_draws()], [bm_benchmark()]
#' @export
bm_plot_scale_h <- function(x,
                            benchmarks = c(0.30, 0.40, 0.50),
                            show_raw = TRUE,
                            caption = NULL) {
  fits <- .bm_fit_list(x)
  rows <- do.call(rbind, lapply(names(fits), function(nm) {
    s <- bm_scale_h(fits[[nm]])
    data.frame(fit = nm, s, stringsAsFactors = FALSE)
  }))
  rows$fit <- factor(rows$fit, levels = rev(names(fits)))

  p <- ggplot(rows, aes(y = .data$fit))
  if (!is.null(benchmarks) && length(benchmarks)) {
    p <- p + geom_vline(xintercept = benchmarks, linetype = "dotted",
                        colour = .bm_colours[["muted"]], linewidth = 0.4)
  }
  if (isTRUE(show_raw) && any(!is.na(rows$raw_lower))) {
    p <- p + geom_linerange(
      aes(xmin = .data$raw_lower, xmax = .data$raw_upper),
      position = position_nudge(y = -0.14),
      colour = .bm_colours[["secondary"]], linewidth = 0.5,
      linetype = "dashed", na.rm = TRUE
    )
  }
  p <- p +
    geom_linerange(aes(xmin = .data$lower, xmax = .data$upper),
                   colour = .bm_colours[["primary"]], linewidth = 1.1,
                   na.rm = TRUE) +
    geom_point(aes(x = .data$H), size = 2.8,
               colour = .bm_colours[["primary"]], na.rm = TRUE) +
    labs(
      x = "Mokken scale coefficient H",
      y = NULL,
      caption = .bm_caption(
        caption,
        .bm_method_caption(fits[[1L]]),
        if (isTRUE(show_raw)) {
          "Thick line: calibrated interval. Dashed line beneath: uncorrected equal-tail interval."
        } else NULL
      )
    ) +
    theme_bayesmokken()
  if (length(fits) == 1L) {
    p <- p + theme(axis.text.y = element_blank())
  }
  .bm_attach_alt(
    p,
    .bm_scale_alt(rows, show_raw = show_raw, benchmarks = benchmarks)
  )
}


#' Plot the bootstrap draws with the interval endpoints
#'
#' Shows the distribution of the Bayesian-bootstrap draws of the scale
#' coefficient, with the uncorrected quantiles and the calibrated endpoints
#' drawn over it. This is the picture of what the method does: resample, take
#' quantiles, apply the frozen width correction.
#'
#' @param x A `bm_bb_h` object.
#' @param type `"density"` or `"histogram"`.
#' @param show_interval Draw the endpoint rules.
#' @param bins Number of histogram bins.
#' @param rug Draw a rug of the individual draws.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_plot_draws(fit)
#'
#' \donttest{
#' bm_plot_draws(fit, type = "histogram", rug = TRUE)
#' }
#'
#' @family bayesmokken-plots
#' @seealso [bm_plot_scale_h()], [bm_mc_diagnostics()]
#' @export
bm_plot_draws <- function(x,
                          type = c("density", "histogram"),
                          show_interval = TRUE,
                          bins = 40,
                          rug = FALSE,
                          caption = NULL) {
  type <- match.arg(type)
  parts <- .bm_parts(x)
  itv <- parts$interval
  if (is.null(itv)) {
    .bm_abort("This object has no bootstrap draws to plot.",
              hint = "Use bb_mokken_h() rather than mokken_h().")
  }
  d <- parts$draws$scale
  finite <- d[is.finite(d)]
  df <- data.frame(value = finite)

  p <- ggplot(df, aes(x = .data$value))
  p <- if (identical(type, "density")) {
    p + geom_density(fill = .bm_colours[["secondary"]], alpha = 0.35,
                     colour = .bm_colours[["primary"]], linewidth = 0.5)
  } else {
    p + geom_histogram(bins = bins, fill = .bm_colours[["secondary"]],
                       colour = "white", linewidth = 0.2)
  }
  if (isTRUE(rug)) {
    p <- p + geom_rug(alpha = 0.15, colour = .bm_colours[["primary"]],
                      length = grid::unit(0.02, "npc"))
  }
  if (isTRUE(show_interval)) {
    p <- p +
      geom_vline(xintercept = c(itv$raw_lower, itv$raw_upper),
                 linetype = "dashed", linewidth = 0.5,
                 colour = .bm_colours[["secondary"]]) +
      geom_vline(xintercept = c(itv$lower, itv$upper),
                 linetype = "solid", linewidth = 1.0,
                 colour = .bm_colours[["primary"]]) +
      geom_vline(xintercept = itv$point_estimate, linetype = "dotdash",
                 linewidth = 0.6, colour = .bm_colours[["accent"]])
  }
  p <- p +
    labs(
      x = "Bayesian-bootstrap draws of H",
      y = if (identical(type, "density")) "density" else "draws",
      subtitle = sprintf("%d of %d draws defined; width multiplier %s",
                         itv$defined_draws, itv$total_draws,
                         .bm_num(itv$width_multiplier, 4L)),
      caption = .bm_caption(
        caption,
        .bm_method_caption(x),
        "Solid rules: calibrated endpoints. Dashed rules: uncorrected equal-tail quantiles. Dot-dash: point estimate."
      )
    ) +
    theme_bayesmokken()

  .bm_attach_alt(p, .bm_alt(
    "Distribution of", itv$defined_draws,
    "Bayesian-bootstrap draws of H, with vertical rules marking the",
    "uncorrected endpoints at", .bm_num(itv$raw_lower), "and",
    .bm_num(itv$raw_upper), "and the calibrated endpoints at",
    .bm_num(itv$lower), "and", .bm_num(itv$upper), "."
  ))
}


# --- shared helpers -----------------------------------------------------------

.bm_scale_alt <- function(rows, show_raw, benchmarks) {
  row_text <- vapply(seq_len(nrow(rows)), function(k) {
    label <- as.character(rows$fit[k])
    point <- if (is.finite(rows$H[k])) {
      paste0("H = ", .bm_num(rows$H[k]), ".")
    } else {
      "H is undefined."
    }
    has_interval <- all(is.finite(c(rows$lower[k], rows$upper[k])))
    interval <- if (has_interval) {
      level <- if (is.finite(rows$level[k])) {
        paste0(format(100 * rows$level[k], trim = TRUE), " percent")
      } else {
        "calibrated"
      }
      paste0(
        "The ", level, " interval runs from ", .bm_num(rows$lower[k]),
        " to ", .bm_num(rows$upper[k]), "."
      )
    } else if (is.na(rows$method_id[k])) {
      "No interval was computed."
    } else {
      "Interval endpoints are undefined."
    }
    raw <- if (isTRUE(show_raw) &&
               all(is.finite(c(rows$raw_lower[k], rows$raw_upper[k])))) {
      paste0(
        " The displayed raw interval runs from ",
        .bm_num(rows$raw_lower[k]), " to ", .bm_num(rows$raw_upper[k]), "."
      )
    } else {
      ""
    }
    paste0("Row ", k, " (", label, "): ", point, " ", interval, raw)
  }, character(1))

  references <- if (!is.null(benchmarks) && length(benchmarks)) {
    paste0(
      " Reference rules are shown at ",
      paste(.bm_num(benchmarks), collapse = ", "), "."
    )
  } else {
    " No reference rules are shown."
  }
  paste0(
    "Scale coefficient plot with ", nrow(rows),
    if (nrow(rows) == 1L) " fit row. " else " fit rows. ",
    paste(row_text, collapse = " "), references
  )
}

# Accept a single fit or a named list, so every figure can compare without a
# separate entry point.
.bm_fit_list <- function(x) {
  if (inherits(x, c("bm_bb_h", "bm_empirical_h"))) {
    return(stats::setNames(list(x), "fit"))
  }
  if (!is.list(x) || !length(x)) {
    .bm_abort("Expected a fit, or a named list of fits.")
  }
  nms <- names(x)
  if (is.null(nms) || any(!nzchar(nms))) {
    .bm_abort("Every fit in the list must be named.",
              hint = "For example list(calibrated = f1, `equal tail` = f2).")
  }
  for (f in x) {
    if (!inherits(f, c("bm_bb_h", "bm_empirical_h"))) {
      .bm_abort("Every element must be a fit from bb_mokken_h() or mokken_h().")
    }
  }
  x
}

# Alt text rides along on the object so vignettes and pkgdown can pick it up
# without the author retyping a description of the figure.
.bm_attach_alt <- function(p, alt) {
  attr(p, "bm_alt") <- alt
  p
}

#' Alternative text for a bayesmokken figure
#'
#' Returns the description the package generated for a figure, for use as
#' `fig.alt` in a report or `alt` on a web page.
#'
#' @param p A ggplot produced by one of the `bm_plot_*()` functions.
#'
#' @return A single string, or `NA` if the plot did not come from this package.
#'
#' @examples
#' bm_alt_text(bm_plot_scale_h(bb_mokken_h(srs, draws = 200, seed = 1)))
#'
#' @family bayesmokken-plots
#' @export
bm_alt_text <- function(p) {
  alt <- attr(p, "bm_alt")
  if (is.null(alt)) NA_character_ else alt
}
