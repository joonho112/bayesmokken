# =============================================================================
# Figures of the scale's internal structure
# =============================================================================

#' Plot the item coefficients
#'
#' Draws one row per item. By default, rows follow ascending item-coefficient
#' order, with the smallest coefficient at the bottom of the panel.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param sort Order items by coefficient. Set `FALSE` to keep the item order
#'   of the data.
#' @param show_interval Draw raw item-level bootstrap intervals when the object
#'   has draws. The default is `FALSE` because these intervals are uncalibrated.
#' @param benchmarks Reference values, or `NULL` (the default) for none.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @section These intervals are not calibrated:
#' The width correction was validated for the scale coefficient. The intervals
#' drawn here are raw equal-tail quantiles of the item draws, so they are drawn
#' thin and dashed and the caption says so. Three separate signals, because a
#' reader may lose any one of them to grayscale printing or color vision
#' deficiency.
#'
#' @section Fixed-scale item inspection:
#' The ordering and optional item intervals describe the declared fixed scale.
#' Removing items and refitting defines a new analysis; scale-level interval
#' claims from the original fit do not carry over.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_plot_item_h(fit)
#'
#' @family bayesmokken-plots
#' @seealso [bm_item_h()], [bm_plot_pair_h()]
#' @export
bm_plot_item_h <- function(x,
                           sort = TRUE,
                           show_interval = FALSE,
                           benchmarks = NULL,
                           caption = NULL) {
  it <- bm_item_h(x, include_interval = isTRUE(show_interval))
  if (!isTRUE(sort)) {
    original <- .bm_parts(x)$empirical$item$item
    it <- it[match(original, it$item), , drop = FALSE]
  }
  it$item <- factor(it$item, levels = it$item)
  it$status <- .bm_status_factor(it$denominator_status)
  has_ci <- isTRUE(show_interval) && all(c("bb_lower", "bb_upper") %in% names(it))

  p <- ggplot(it, aes(x = .data$H, y = .data$item))
  if (!is.null(benchmarks) && length(benchmarks)) {
    p <- p + geom_vline(xintercept = benchmarks, linetype = "dotted",
                        colour = .bm_colours[["muted"]], linewidth = 0.4)
  }
  if (has_ci) {
    p <- p + geom_linerange(
      aes(xmin = .data$bb_lower, xmax = .data$bb_upper),
      linetype = "dashed", linewidth = 0.55,
      colour = .bm_colours[["ink"]], na.rm = TRUE
    )
  }
  p <- p +
    geom_point(aes(colour = .data$status, shape = .data$status),
               size = 2.4, fill = "white", stroke = 0.7) +
    scale_colour_manual(values = .bm_status_colour, drop = FALSE,
                        name = "denominator") +
    scale_shape_manual(values = .bm_status_shape, drop = FALSE,
                       name = "denominator") +
    labs(
      x = expression(paste("Item coefficient ", H[i])),
      y = NULL,
      caption = .bm_caption(caption, .bm_method_caption(x),
                            .bm_fixed_scale_item_note,
                            uncalibrated = has_ci)
    ) +
    theme_bayesmokken()
  if (length(unique(it$status)) == 1L) {
    p <- p + guides(colour = "none", shape = "none")
  }
  .bm_attach_alt(p, .bm_item_alt(it, sort = sort, has_ci = has_ci))
}


#' Plot the pair coefficients
#'
#' A lower-triangular heatmap of the pair coefficients, using the same item
#' order as [bm_plot_item_h()] so the two figures can be read together.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param layout `"lower"` for the triangle, `"full"` for the whole square.
#' @param show_values Print the coefficient in each cell. `NULL` decides by
#'   the number of items.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_plot_pair_h(fit)
#'
#' @family bayesmokken-plots
#' @seealso [bm_pair_h()], [bm_plot_item_h()]
#' @export
bm_plot_pair_h <- function(x,
                           layout = c("lower", "full"),
                           show_values = NULL,
                           caption = NULL) {
  layout <- match.arg(layout)
  pr <- bm_pair_h(x)
  order_items <- bm_item_h(
    x, include_interval = FALSE
  )$item   # same order as the item plot
  n_items <- length(order_items)
  if (is.null(show_values)) show_values <- n_items <= 12L

  df <- pr
  if (identical(layout, "full")) {
    mirror <- df
    mirror$item_i <- pr$item_j
    mirror$item_j <- pr$item_i
    df <- rbind(df, mirror)
  }
  # Pairs are stored with i < j in the ORIGINAL item order, but the axes here
  # are sorted by coefficient so the figure lines up with bm_plot_item_h().
  # Orienting each pair by its position in the DISPLAY order is what makes the
  # triangle come out solid instead of ragged.
  pos_i <- match(df$item_i, order_items)
  pos_j <- match(df$item_j, order_items)
  if (identical(layout, "lower")) {
    hi <- ifelse(pos_i >= pos_j, df$item_i, df$item_j)
    lo <- ifelse(pos_i >= pos_j, df$item_j, df$item_i)
    df$row <- factor(hi, levels = order_items)
    df$col <- factor(lo, levels = order_items)
  } else {
    df$row <- factor(df$item_i, levels = order_items)
    df$col <- factor(df$item_j, levels = order_items)
  }
  df$boundary <- !is.na(df$denominator_status) &
    df$denominator_status != "positive"
  finite_h <- df$H[is.finite(df$H)]
  limit <- if (length(finite_h)) max(abs(finite_h)) else 1
  if (!is.finite(limit) || limit <= 0) limit <- 1
  # Dark ink on a dark tile is unreadable, so flip to white past the point
  # where the diverging ramp goes dark. Computed here, before ggplot() takes
  # its copy of the data.
  df$label_colour <- ifelse(
    is.finite(df$H) & abs(df$H) > 0.62 * limit,
    "white", unname(.bm_colours[["ink"]])
  )
  df$value_label <- ifelse(is.finite(df$H), .bm_num(df$H, 2L), NA_character_)
  # Numeric labels already carry a sign. When they are suppressed, retain the
  # sign with a glyph that survives grayscale/CVD conversion. Exact zero has
  # no sign; undefined and boundary cells are reserved for their own states.
  df$sign_glyph <- ifelse(
    !df$boundary & is.finite(df$H) & df$H > 0,
    "+",
    ifelse(
      !df$boundary & is.finite(df$H) & df$H < 0,
      "\u2212", NA_character_
    )
  )

  p <- ggplot(df, aes(x = .data$col, y = .data$row)) +
    geom_tile(aes(fill = .data$H), colour = "white", linewidth = 0.5) +
    scale_fill_gradientn(
      colours = bm_palette(type = "diverging"),
      limits = c(-limit, limit),
      name = expression(H[ij])
  )
  if (isTRUE(show_values)) {
    p <- p + geom_text(
      aes(label = .data$value_label, colour = I(.data$label_colour)),
      size = 2.7, na.rm = TRUE
    )
  } else if (any(!is.na(df$sign_glyph))) {
    p <- p + geom_text(
      data = df[!is.na(df$sign_glyph), , drop = FALSE],
      aes(label = .data$sign_glyph, colour = I(.data$label_colour)),
      size = 3.2, fontface = "bold", na.rm = TRUE
    )
  }
  if (any(df$boundary)) {
    p <- p + geom_point(
      data = df[df$boundary, , drop = FALSE],
      shape = 4, size = 3, stroke = 0.9, colour = .bm_colours[["ink"]]
    )
  }
  p <- p +
    scale_y_discrete(limits = rev(levels(df$row))) +
    coord_fixed() +
    labs(
      x = NULL, y = NULL,
      caption = .bm_caption(
        caption, .bm_method_caption(x),
        if (any(df$boundary)) {
          "Crosses mark pairs whose maximum covariance is zero or near zero."
        } else NULL
      )
    ) +
    theme_bayesmokken() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

  .bm_attach_alt(
    p,
    .bm_pair_alt(pr, df, layout = layout, show_values = isTRUE(show_values))
  )
}


.bm_item_alt <- function(items, sort, has_ci) {
  finite <- which(is.finite(items$H))
  range_text <- if (length(finite)) {
    lo <- finite[which.min(items$H[finite])]
    hi <- finite[which.max(items$H[finite])]
    paste0(
      "The smallest finite coefficient is ", .bm_num(items$H[lo]),
      " for ", as.character(items$item[lo]),
      "; the largest is ", .bm_num(items$H[hi]),
      " for ", as.character(items$item[hi]), "."
    )
  } else {
    "All item coefficients are undefined."
  }
  order_text <- if (isTRUE(sort)) {
    "Items are displayed in ascending coefficient order."
  } else {
    "Items are displayed in the original data order."
  }
  interval_text <- if (isTRUE(has_ci)) {
    defined <- sum(is.finite(items$bb_lower) & is.finite(items$bb_upper))
    paste0(
      "Raw uncalibrated bootstrap intervals are displayed for ", defined,
      " of ", nrow(items), " items."
    )
  } else {
    "No item intervals are displayed."
  }
  paste(
    "Dot-and-line plot of", nrow(items), "item coefficients.",
    order_text, range_text, interval_text
  )
}


.bm_pair_alt <- function(pairs, displayed, layout, show_values) {
  layout_text <- if (identical(layout, "full")) {
    "Full symmetric off-diagonal heatmap"
  } else {
    "Lower-triangular heatmap"
  }
  finite <- pairs$H[is.finite(pairs$H)]
  range_text <- if (length(finite)) {
    paste0(
      length(finite), " finite unique-pair coefficients range from ",
      .bm_num(min(finite)), " to ", .bm_num(max(finite)), "; ",
      sum(!is.finite(pairs$H)), " are undefined."
    )
  } else {
    paste0(
      "All ", nrow(pairs),
      " unique-pair coefficients are undefined; a neutral fill scale centered",
      " at zero is used."
    )
  }
  value_text <- if (isTRUE(show_values)) {
    "Finite numeric coefficient values are printed in their cells."
  } else {
    paste(
      "Plus and minus glyphs mark positive and negative finite coefficients.",
      "Zero, undefined, and boundary cells have no sign glyph."
    )
  }
  boundary_text <- if (any(displayed$boundary)) {
    paste0(
      " Crosses mark ", sum(displayed$boundary),
      " displayed boundary-denominator cells."
    )
  } else {
    " No boundary-denominator cells are displayed."
  }
  paste0(
    layout_text, " with ", nrow(displayed), " displayed cells representing ",
    nrow(pairs), " unique item pairs. ", range_text, " ", value_text,
    boundary_text
  )
}
