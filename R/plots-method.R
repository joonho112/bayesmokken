# =============================================================================
# Figures about the method itself
#
# These two are as much teaching aids as diagnostics. bm_plot_calibration()
# runs with no fit at all, so it can be used to explain the construction
# before anyone has data in front of them.
# =============================================================================

#' Plot the interval multiplier
#'
#' Draws the frozen multiplier against sample size, for the binary width
#' correction, or against the number of clusters, for the clustered
#' adjustment, and marks where a given analysis sits on that curve.
#'
#' Three curves exist. The binary interval is widened about its midpoint by
#' \eqn{1 + 0.25/\sqrt{n}}, because the uncorrected bootstrap interval was
#' measured to be slightly too narrow. The default clustered interval reflects
#' the percentile quantiles about the point estimate and expands the reflected
#' errors by \eqn{k_G = \sqrt{(G+1)/(G-1)}\, t_{G-1,.975}/z_{.975}}, a
#' prespecified function of the cluster count that is above one and tends to
#' one as \eqn{G} grows. The historical two-stage interval of version 0.9.0 was
#' narrowed about its midpoint by \eqn{1 - 1.5/\sqrt{G}}, because the
#' uncorrected two-stage interval was measured to be too wide; that curve is
#' kept under `mode = "clustered_two_stage"` for reproducing earlier results.
#'
#' @section Cluster-count validation:
#' The part of a clustered curve inside the confirmed cluster-count range is
#' solid, and the same formula is drawn dashed outside it to identify the
#' region as extrapolation of the evidence, not as additional validation. For
#' the default complete-record adjustment the confirmed range is
#' \eqn{22 \le G \le 320}; the interval remains computable for \eqn{G \ge 2}
#' and reports `OUTSIDE_CONFIRMED_CLUSTER_COUNT` outside that range. For the
#' historical two-stage correction the range is \eqn{22 \le G \le 80}; fits
#' below 22 clusters are refused on that path and fits above 80 carry
#' `EXTRAPOLATED_CLUSTER_COUNT`. When `x` is supplied, the point uses its
#' recorded width multiplier and its validation status is repeated in the
#' caption and alternative text.
#'
#' @param x Optional fit. When supplied, its sample size or cluster count is
#'   marked on the curve and `mode` is taken from it.
#' @param mode `"binary"`, `"clustered"` (the default complete-record
#'   adjustment) or `"clustered_two_stage"` (the historical two-stage
#'   shrinkage). Ignored when `x` is supplied.
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
#' @seealso [bm_plot_scale_h()], `vignette("m2-bayesian-bootstrap-calibration", package = "bayesmokken")`,
#'   `vignette("m4-hierarchical-bb-targets", package = "bayesmokken")`
#' @export
bm_plot_calibration <- function(x = NULL,
                                mode = c("binary", "clustered",
                                         "clustered_two_stage"),
                                range = NULL,
                                caption = NULL) {
  here <- NULL
  here_multiplier <- NULL
  validation_status <- NULL
  validated_cluster_min <- NULL
  validated_cluster_max <- NULL
  if (!is.null(x)) {
    s <- bm_scale_h(x)
    itv <- .bm_parts(x)$interval
    mode <- if (is.na(s$n_clusters)) {
      "binary"
    } else if (!is.null(itv) &&
               identical(itv$validation_scope,
                         "POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB")) {
      "clustered_two_stage"
    } else {
      "clustered"
    }
    here <- if (identical(mode, "binary")) s$n else s$n_clusters
    if (is.finite(s$width_multiplier)) {
      here_multiplier <- s$width_multiplier
    }
    if (!identical(mode, "binary") && !is.null(itv)) {
      validation_status <- itv$validation_status
      validated_cluster_min <- itv$validated_cluster_min
      validated_cluster_max <- itv$validated_cluster_max
    }
  } else {
    mode <- match.arg(mode)
  }

  spec <- switch(
    mode,
    binary = list(
      formula = function(g) 1 + 0.25 / sqrt(g),
      range = c(30, 2000),
      confirmed = c(NA_integer_, NA_integer_),
      xlab = "complete-scale respondents (n)",
      subtitle = "Binary: 1 + 0.25/sqrt(n)  -- the correction widens",
      constant_note = paste(
        "The constant was fixed by a frozen train-validate-lock procedure",
        "and is not a user argument."
      ),
      alt_shape = "sample size, rising above 1 and decreasing toward 1."
    ),
    clustered = list(
      formula = .bm_cluster_k_multiplier,
      range = c(5, 400),
      confirmed = c(.bm_crbb_confirmed_cluster_min,
                    .bm_crbb_confirmed_cluster_max),
      xlab = "clusters (G)",
      subtitle = paste(
        "Clustered (complete-record): sqrt((G+1)/(G-1)) t[G-1]/z",
        " -- expands the reflected errors"
      ),
      constant_note = paste(
        "The multiplier is a prespecified function of G, not a tuned",
        "constant and not a user argument."
      ),
      alt_shape = paste(
        "the number of clusters, above 1 and decreasing toward 1;"
      )
    ),
    clustered_two_stage = list(
      formula = function(g) 1 - 1.5 / sqrt(g),
      range = c(5, 200),
      confirmed = c(.bm_validated_cluster_min, .bm_validated_cluster_max),
      xlab = "clusters (G)",
      subtitle = paste(
        "Historical two-stage (0.9.0): 1 - 1.5/sqrt(G)",
        " -- the shrinkage narrows"
      ),
      constant_note = paste(
        "The constant was fixed by a frozen train-validate-lock procedure",
        "and is not a user argument."
      ),
      alt_shape = paste(
        "the number of clusters, below 1 and increasing toward 1;"
      )
    )
  )
  clustered <- !identical(mode, "binary")
  if (clustered) {
    if (is.null(validated_cluster_min)) {
      validated_cluster_min <- spec$confirmed[[1L]]
    }
    if (is.null(validated_cluster_max)) {
      validated_cluster_max <- spec$confirmed[[2L]]
    }
  }
  if (is.null(range)) range <- spec$range
  grid <- seq(range[1L], range[2L], length.out = 400L)
  if (clustered) {
    boundaries <- c(validated_cluster_min, validated_cluster_max)
    boundaries <- boundaries[
      boundaries >= min(range) & boundaries <= max(range)
    ]
    grid <- sort(unique(c(grid, boundaries)))
  }
  df <- data.frame(x = grid, multiplier = spec$formula(grid))

  p <- ggplot(df, aes(x = .data$x, y = .data$multiplier)) +
    geom_hline(yintercept = 1, linetype = "dotted",
               colour = .bm_colours[["muted"]], linewidth = 0.4)

  if (!clustered) {
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
    if (is.null(hm)) hm <- spec$formula(here)
    point_label <- sprintf(
      "%s = %d, x %s",
      if (clustered) "G" else "n",
      as.integer(here), .bm_num(hm, 4L)
    )
    if (clustered && !is.null(validation_status)) {
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
      x = spec$xlab,
      y = "interval width multiplier",
      subtitle = spec$subtitle,
      caption = .bm_caption(
        caption,
        spec$constant_note,
        if (clustered) {
          sprintf(
            paste(
              "Solid curve: confirmed cluster-count range G = %d-%d.",
              "Dashed curve: extrapolation outside that range."
            ),
            validated_cluster_min, validated_cluster_max
          )
        } else NULL,
        if (clustered && !is.null(validation_status)) {
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
    spec$alt_shape,
    if (clustered) {
      sprintf(
        paste(
          "the curve is solid over the confirmed G = %d-%d range and dashed",
          "outside it as extrapolation."
        ),
        validated_cluster_min, validated_cluster_max
      )
    } else NULL,
    if (clustered && !is.null(validation_status)) {
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
  # quantiles, then the same construction. Anything else would show a curve
  # that does not end where the reported interval is. For the complete-record
  # construction the running endpoints are the reflected, expanded values
  # before the intersection with (-Inf, 1].
  ks <- unique(round(seq(20L, n, length.out = min(points, n))))
  mult <- itv$width_multiplier
  reflected <- identical(
    itv$construction, "reflected_error_expansion_about_point"
  )
  point <- itv$point_estimate
  rows <- do.call(rbind, lapply(ks, function(k) {
    q <- stats::quantile(finite[seq_len(k)], c(0.025, 0.975), names = FALSE,
                         type = 8)
    if (reflected) {
      return(data.frame(draws = k,
                        lower = point - mult * (q[2L] - point),
                        upper = point - mult * (q[1L] - point)))
    }
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
        "Dashed rules are the reported endpoints.",
        if (reflected) {
          "Running endpoints are the reflected, expanded values before the intersection with (-Inf, 1]."
        } else NULL
      )
    ) +
    theme_bayesmokken()

  .bm_attach_alt(p, .bm_alt(
    "Running interval endpoints against the number of draws used, converging to",
    .bm_num(itv$lower), "and", .bm_num(itv$upper), "at", n, "draws."
  ))
}
