# =============================================================================
# Figures about the method itself
#
# These two are as much teaching aids as diagnostics. bm_plot_calibration()
# runs with no fit at all, so it can be used to explain the correction before
# anyone has data in front of them.
# =============================================================================

#' Plot the width correction
#'
#' Draws the frozen width multiplier against sample size, for the binary
#' correction, or against the number of clusters, for the clustered one, and
#' marks where a given analysis sits on that curve.
#'
#' The two corrections run in opposite directions, and this figure is the
#' quickest way to see why. The binary interval is widened by
#' \eqn{1 + 0.25/\sqrt{n}}, because the uncorrected bootstrap interval was
#' measured to be slightly too narrow. The clustered interval is narrowed by
#' \eqn{1 - 1.5/\sqrt{G}}, because the uncorrected hierarchical interval was
#' measured to be substantially too wide.
#'
#' @section Cluster-count validation:
#' The clustered correction has a confirmed cluster-count range of
#' \eqn{22 \le G \le 80}. That part of the curve is solid. The same frozen
#' formula is drawn with a dashed line outside the confirmed range to identify
#' it as extrapolation, not as additional validation. Public calibrated fits
#' below 22 clusters are unsupported; fits above 80 clusters retain the
#' correction but carry `EXTRAPOLATED_CLUSTER_COUNT` validation metadata. When
#' `x` is supplied, the point uses its recorded width multiplier and its
#' validation status is repeated in the caption and alternative text.
#'
#' @param x Optional fit. When supplied, its sample size or cluster count is
#'   marked on the curve and `mode` is taken from it.
#' @param mode `"binary"` or `"clustered"`. Ignored when `x` is supplied.
#' @param range Numeric length-2 range for the horizontal axis.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' # As a teaching figure, with no data at all
#' bm_plot_calibration(mode = "clustered")
#'
#' # With an analysis marked on it
#' bm_plot_calibration(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-plots
#' @seealso [bm_plot_scale_h()], `vignette("m2-bayesian-bootstrap-calibration", package = "bayesmokken")`
#' @export
bm_plot_calibration <- function(x = NULL,
                                mode = c("binary", "clustered"),
                                range = NULL,
                                caption = NULL) {
  here <- NULL
  here_multiplier <- NULL
  validation_status <- NULL
  validated_cluster_min <- .bm_validated_cluster_min
  validated_cluster_max <- .bm_validated_cluster_max
  if (!is.null(x)) {
    s <- bm_scale_h(x)
    mode <- if (is.na(s$n_clusters)) "binary" else "clustered"
    here <- if (identical(mode, "binary")) s$n else s$n_clusters
    if (is.finite(s$width_multiplier)) {
      here_multiplier <- s$width_multiplier
    }
    if (identical(mode, "clustered")) {
      itv <- .bm_parts(x)$interval
      if (!is.null(itv)) {
        validation_status <- itv$validation_status
        if (!is.null(itv$validated_cluster_min)) {
          validated_cluster_min <- itv$validated_cluster_min
        }
        if (!is.null(itv$validated_cluster_max)) {
          validated_cluster_max <- itv$validated_cluster_max
        }
      }
    }
  } else {
    mode <- match.arg(mode)
  }
  if (is.null(range)) {
    range <- if (identical(mode, "binary")) c(30, 2000) else c(5, 200)
  }
  grid <- seq(range[1L], range[2L], length.out = 400L)
  if (identical(mode, "clustered")) {
    boundaries <- c(validated_cluster_min, validated_cluster_max)
    boundaries <- boundaries[
      boundaries >= min(range) & boundaries <= max(range)
    ]
    grid <- sort(unique(c(grid, boundaries)))
  }
  mult <- if (identical(mode, "binary")) {
    1 + 0.25 / sqrt(grid)
  } else {
    1 - 1.5 / sqrt(grid)
  }
  df <- data.frame(x = grid, multiplier = mult)

  p <- ggplot(df, aes(x = .data$x, y = .data$multiplier)) +
    geom_hline(yintercept = 1, linetype = "dotted",
               colour = .bm_colours[["muted"]], linewidth = 0.4)

  if (identical(mode, "binary")) {
    p <- p + geom_line(
      colour = .bm_colours[["primary"]], linewidth = 0.9
    )
  } else {
    confirmed <- df[
      df$x >= validated_cluster_min & df$x <= validated_cluster_max,
      , drop = FALSE
    ]
    below <- df[df$x <= validated_cluster_min, , drop = FALSE]
    above <- df[df$x >= validated_cluster_max, , drop = FALSE]
    if (nrow(below) >= 2L) {
      p <- p + geom_line(
        data = below, linetype = "dashed",
        colour = .bm_colours[["secondary"]], linewidth = 0.7
      )
    }
    if (nrow(above) >= 2L) {
      p <- p + geom_line(
        data = above, linetype = "dashed",
        colour = .bm_colours[["secondary"]], linewidth = 0.7
      )
    }
    if (nrow(confirmed) >= 2L) {
      p <- p + geom_line(
        data = confirmed, linetype = "solid",
        colour = .bm_colours[["primary"]], linewidth = 0.9
      )
    }
  }

  if (!is.null(here) && here >= range[1L] && here <= range[2L]) {
    hm <- here_multiplier
    if (is.null(hm)) {
      hm <- if (identical(mode, "binary")) {
        1 + 0.25 / sqrt(here)
      } else {
        1 - 1.5 / sqrt(here)
      }
    }
    point_label <- sprintf(
      "%s = %d, x %s",
      if (identical(mode, "binary")) "n" else "G",
      as.integer(here), .bm_num(hm, 4L)
    )
    if (identical(mode, "clustered") && !is.null(validation_status)) {
      point_label <- paste(point_label, validation_status, sep = "\n")
    }
    p <- p +
      geom_point(data = data.frame(x = here, multiplier = hm),
                 size = 3, colour = .bm_colours[["accent"]]) +
      geom_text(data = data.frame(x = here, multiplier = hm),
                aes(label = point_label),
                vjust = -1.1, hjust = 0.5, size = 3.1,
                colour = .bm_colours[["accent"]])
  }

  p <- p +
    labs(
      x = if (identical(mode, "binary")) "complete-scale respondents (n)" else "clusters (G)",
      y = "interval width multiplier",
      subtitle = if (identical(mode, "binary")) {
        "Binary: 1 + 0.25/sqrt(n)  -- the correction widens"
      } else {
        "Clustered: 1 - 1.5/sqrt(G)  -- the correction narrows"
      },
      caption = .bm_caption(
        caption,
        "The constant was fixed by a frozen train-validate-lock procedure and is not a user argument.",
        if (identical(mode, "clustered")) {
          sprintf(
            paste(
              "Solid curve: confirmed cluster-count range G = %d-%d.",
              "Dashed curve: extrapolation outside that range."
            ),
            validated_cluster_min, validated_cluster_max
          )
        } else NULL,
        if (identical(mode, "clustered") &&
            !is.null(validation_status)) {
          sprintf(
            "Current fit metadata: G = %d; validation status %s.",
            as.integer(here), validation_status
          )
        } else NULL
      )
    ) +
    theme_bayesmokken()

  .bm_attach_alt(p, .bm_alt(
    "Curve of the interval width multiplier against",
    if (identical(mode, "binary")) "sample size, rising above 1 and decreasing toward 1."
    else sprintf(
      paste(
        "the number of clusters, below 1 and increasing toward 1;",
        "the curve is solid over the confirmed G = %d-%d range and dashed",
        "outside it as extrapolation."
      ),
      validated_cluster_min, validated_cluster_max
    ),
    if (identical(mode, "clustered") && !is.null(validation_status)) {
      sprintf(
        "The current fit is G = %d with validation status %s.",
        as.integer(here), validation_status
      )
    } else NULL
  ))
}


#' Plot Monte Carlo settling of the interval endpoints
#'
#' Shows how the interval endpoints move as more bootstrap draws are used. A
#' series that has flattened well before the right edge means the draw count is
#' generous; one still moving means more draws are worth taking.
#'
#' @param x A `bm_bb_h` object.
#' @param points Number of prefix lengths to evaluate.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' bm_plot_mc_stability(bb_mokken_h(srs, draws = 500, seed = 1))
#'
#' @family bayesmokken-plots
#' @seealso [bm_mc_diagnostics()], [bm_plot_draws()]
#' @export
bm_plot_mc_stability <- function(x, points = 40, caption = NULL) {
  parts <- .bm_parts(x)
  itv <- parts$interval
  if (is.null(itv)) {
    .bm_abort("This object has no bootstrap draws.",
              hint = "Use bb_mokken_h() rather than mokken_h().")
  }
  d <- parts$draws$scale
  finite <- d[is.finite(d)]
  n <- length(finite)
  if (n < 20L) .bm_abort("Too few defined draws to show settling.")

  # The running endpoints must be computed the way the interval was: type-8
  # quantiles, then the same width multiplier. Anything else would show a
  # curve that does not end where the reported interval is.
  ks <- unique(round(seq(20L, n, length.out = min(points, n))))
  mult <- itv$width_multiplier
  rows <- do.call(rbind, lapply(ks, function(k) {
    q <- stats::quantile(finite[seq_len(k)], c(0.025, 0.975), names = FALSE,
                         type = 8)
    mid <- mean(q)
    half <- diff(q) / 2
    data.frame(draws = k,
               lower = mid - mult * half,
               upper = mid + mult * half)
  }))
  long <- data.frame(
    draws = rep(rows$draws, 2L),
    endpoint = rep(c("lower", "upper"), each = nrow(rows)),
    value = c(rows$lower, rows$upper)
  )

  p <- ggplot(long, aes(x = .data$draws, y = .data$value,
                        group = .data$endpoint)) +
    geom_hline(yintercept = c(itv$lower, itv$upper), linetype = "dashed",
               colour = .bm_colours[["muted"]], linewidth = 0.4) +
    geom_line(colour = .bm_colours[["primary"]], linewidth = 0.7) +
    geom_point(data = long[long$draws == max(long$draws), , drop = FALSE],
               size = 2.2, colour = .bm_colours[["accent"]]) +
    labs(
      x = "draws used", y = "interval endpoint",
      subtitle = sprintf("endpoint Monte Carlo standard error %s / %s",
                         .bm_num(parts$mc$lower_endpoint_mcse, 4L),
                         .bm_num(parts$mc$upper_endpoint_mcse, 4L)),
      caption = .bm_caption(
        caption,
        "Endpoint Monte Carlo diagnostics only; not evidence about the calibration.",
        "Dashed rules are the reported endpoints."
      )
    ) +
    theme_bayesmokken()

  .bm_attach_alt(p, .bm_alt(
    "Running interval endpoints against the number of draws used, converging to",
    .bm_num(itv$lower), "and", .bm_num(itv$upper), "at", n, "draws."
  ))
}
