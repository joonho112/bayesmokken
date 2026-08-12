# =============================================================================
# Diagnostic figures
# =============================================================================

#' Plot cluster imbalance and implied target weights
#'
#' Shows how unequal the clusters are or the weight each of the two population
#' targets gives them. The optional size--mean-score statistic is descriptive
#' only: this figure does not establish whether the two target H values agree.
#' Fit the same scale under both targets and pass those fits to
#' [bm_plot_scale_h()] for a direct target-sensitivity comparison.
#'
#' @param x A clustered fit, a `bm_cluster_info` object, or a vector of cluster
#'   labels.
#' @param show_targets Draw the implied target weights panel.
#' @param max_clusters Show at most this many clusters, largest first.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' bm_plot_clusters(bm_cluster_info(classroom_wb$class_id,
#'                                  data = classroom_wb[, paste0("wb", 1:6)]))
#'
#' @family bayesmokken-plots
#' @seealso [bm_cluster_info()], `vignette("m4-hierarchical-bb-targets", package = "bayesmokken")`
#' @export
bm_plot_clusters <- function(x,
                             show_targets = TRUE,
                             max_clusters = 60L,
                             caption = NULL) {
  info <- .bm_as_cluster_info(x)
  rho <- if (!is.null(info$size_mean_score_spearman)) {
    info$size_mean_score_spearman
  } else {
    info$informative_index
  }
  tab <- info$table
  from_fit <- isTRUE(attr(info, "bm_derived_from_fit"))
  if (nrow(tab) > max_clusters) {
    tab <- tab[seq_len(max_clusters), , drop = FALSE]
  }
  tab$cluster <- factor(tab$cluster, levels = tab$cluster)

  if (!isTRUE(show_targets)) {
    p <- ggplot(tab, aes(x = .data$cluster, y = .data$size)) +
      geom_col(fill = .bm_colours[["primary"]], width = 0.75) +
      labs(x = NULL, y = "cluster size")
  } else {
    long <- data.frame(
      cluster = rep(tab$cluster, 2L),
      target = rep(c("respondent-weighted (n_g / N)", "equal-cluster (1 / G)"),
                   each = nrow(tab)),
      weight = c(tab$weight_respondent, tab$weight_equal)
    )
    long$target <- factor(long$target,
                          levels = c("respondent-weighted (n_g / N)",
                                     "equal-cluster (1 / G)"))
    p <- ggplot(long, aes(x = .data$cluster, y = .data$weight,
                          colour = .data$target, shape = .data$target,
                          group = .data$target)) +
      geom_line(aes(linetype = .data$target), linewidth = 0.6) +
      geom_point(size = 1.9) +
      scale_colour_manual(values = unname(.bm_colours[c("primary", "accent")]),
                          name = NULL) +
      scale_shape_manual(values = c(16L, 17L), name = NULL) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      labs(x = NULL, y = "weight given to the cluster")
  }

  p <- p +
    labs(
      subtitle = paste0(
        sprintf("G = %d, sizes %d-%d", info$G, min(info$sizes),
                max(info$sizes)),
        # Only claim the index when it was actually computed. A fitted object
        # does not retain the item data, so deriving cluster info from it
        # leaves the index undefined; printing "NA" there would advertise a
        # quantity the figure does not have.
        if (!is.na(rho)) {
          sprintf(", size/mean-score Spearman %s",
                  .bm_num(rho, 2L))
        } else ""),
      caption = .bm_caption(
        caption,
        if (from_fit) {
          paste("A fitted object does not keep the cluster labels; clusters",
                "are shown by rank. For labels and the size--mean-score",
                "statistic, pass bm_cluster_info(cluster, data) instead of the fit.")
        } else NULL,
        "The respondent-weighted target weights a cluster by n_g/N; the equal-cluster target gives every cluster 1/G.",
        if (nrow(tab) < info$G) {
          sprintf("Showing the %d largest of %d clusters.", nrow(tab), info$G)
        } else NULL,
        paste("This display does not compare target H values; fit both targets",
              "and compare their deterministic coefficients directly.")
      )
    ) +
    theme_bayesmokken() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = rel(0.7)))

  .bm_attach_alt(p, .bm_alt(
    if (isTRUE(show_targets)) {
      paste("Implied respondent-weighted and equal-cluster weights for",
            info$G, "clusters with sizes ranging from", min(info$sizes),
            "to", max(info$sizes), "respondents.")
    } else {
      paste("Cluster sizes in descending order for", info$G,
            "clusters ranging from", min(info$sizes), "to",
            max(info$sizes), "respondents.")
    }
  ))
}


#' Plot item margins and denominator headroom
#'
#' Shows binary score-one shares or stacked ordinal category shares. The
#' headroom view instead shows the smallest pair maximum covariance involving
#' each item, the quantity that approaches zero at a denominator boundary.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param type `"marginal"` for item response distributions or `"headroom"`
#'   for the smallest pair maximum covariance involving each item. `"gap"` is
#'   a deprecated alias for `"headroom"`.
#' @param caption Caption text; `NULL` builds the default, `NA` removes it.
#'
#' @return A ggplot object.
#'
#' @examples
#' fit <- mokken_h(srs)
#' bm_plot_margins(fit)
#' bm_plot_margins(fit, type = "headroom")
#'
#' @family bayesmokken-plots
#' @seealso [bm_regularity()], [bm_diagnostics()]
#' @export
bm_plot_margins <- function(x,
                            type = c("marginal", "headroom", "gap"),
                            caption = NULL) {
  type <- match.arg(type)
  if (identical(type, "gap")) {
    .Deprecated(
      new = 'bm_plot_margins(type = "headroom")',
      package = "bayesmokken",
      msg = paste(
        '`type = "gap"` is deprecated; use `type = "headroom"`.',
        "The plotted quantity is minimum pair denominator headroom,",
        "not a gap between ordered margins."
      )
    )
    type <- "headroom"
  }
  reg <- bm_regularity(x)
  order_items <- bm_item_h(x, include_interval = FALSE)$item
  emp <- .bm_parts(x)$empirical

  if (identical(type, "headroom")) {
    reg$item <- factor(reg$item, levels = rev(reg$item))
    p <- ggplot(reg, aes(x = .data$denominator_headroom, y = .data$item)) +
      geom_segment(aes(x = 0, xend = .data$denominator_headroom,
                       yend = .data$item),
                   colour = .bm_colours[["secondary"]], linewidth = 0.5) +
      geom_point(size = 2.4, colour = .bm_colours[["primary"]]) +
      labs(x = "minimum pair maximum covariance (denominator headroom)",
           y = NULL,
           subtitle = sprintf("least headroom: %s paired with %s",
                              reg$item[1L], reg$worst_pair[1L]))
    alt <- .bm_alt(
      "Minimum pair denominator headroom per item ranges from",
      .bm_num(min(reg$denominator_headroom)), "for",
      as.character(reg$item[1L]), "upward; values near zero mark a",
      "denominator boundary."
    )
    default_caption <- paste(
      "Headroom is the smallest pair maximum covariance involving each item;",
      "values near zero mark a denominator boundary."
    )
  } else {
    if (identical(emp$estimand$item_type, "ordinal")) {
      margins <- .bm_ordinal_category_share_data(emp, order_items)
      category_levels <- as.character(sort(unique(margins$score)))
      margins$item <- factor(margins$item, levels = order_items)
      margins$category <- factor(
        margins$category, levels = category_levels
      )
      fill <- stats::setNames(
        bm_palette(length(category_levels), type = "sequential"),
        category_levels
      )
      p <- ggplot(
        margins,
        aes(x = .data$share, y = .data$item, fill = .data$category)
      ) +
        geom_col(width = 0.72) +
        scale_x_continuous(limits = c(0, 1)) +
        scale_fill_manual(values = fill, name = "score") +
        labs(
          x = "category share (each item sums to 1)",
          y = NULL,
          subtitle = "target-weighted shares over declared score supports"
        )
      alt <- .bm_alt(
        "Stacked category-share bars for", length(order_items),
        "ordinal items; every item sums to one over declared scores from",
        .bm_num(min(margins$score)), "to", .bm_num(max(margins$score)), "."
      )
      default_caption <- paste(
        "Each bar partitions the complete-scale target mass over the item's",
        "declared score support; its category shares sum to one."
      )
    } else {
      reg$item <- factor(reg$item, levels = order_items)
      p <- ggplot(reg, aes(x = .data$mean_score, y = .data$item)) +
        geom_segment(aes(x = 0, xend = .data$mean_score,
                         yend = .data$item),
                     colour = .bm_colours[["secondary"]], linewidth = 0.5) +
        geom_point(size = 2.4, colour = .bm_colours[["primary"]]) +
        scale_x_continuous(limits = c(0, 1)) +
        labs(x = "share scored 1, P(Y = 1)", y = NULL,
             subtitle = sprintf("score-one shares span %s to %s",
                                .bm_num(min(reg$mean_score)),
                                .bm_num(max(reg$mean_score))))
      alt <- .bm_alt(
        "Binary item score-one shares range from",
        .bm_num(min(reg$mean_score)), "to",
        .bm_num(max(reg$mean_score)), "."
      )
      default_caption <- paste(
        "Each point is the complete-scale target share scored 1 for that item."
      )
    }
  }

  p <- p +
    labs(caption = .bm_caption(
      caption,
      default_caption
    )) +
    theme_bayesmokken()
  .bm_attach_alt(p, alt)
}

.bm_ordinal_category_share_data <- function(emp, item_order) {
  if (is.null(emp$category_mass)) {
    .bm_abort(
      "This ordinal object does not carry item category masses.",
      hint = "Refit it with the current package version to plot category shares."
    )
  }
  rows <- lapply(item_order, function(item) {
    support <- emp$provenance$supports[[item]]
    mass <- .bm_category_mass_for_support(emp, item)
    data.frame(
      item = item,
      score = as.numeric(support),
      category = as.character(support),
      share = unname(mass),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


# Accept a fit, a cluster-info object, or bare labels.
.bm_as_cluster_info <- function(x) {
  if (inherits(x, "bm_cluster_info")) return(x)
  if (inherits(x, c("bm_bb_h", "bm_empirical_h"))) {
    emp <- .bm_parts(x)$empirical
    sizes <- emp$provenance$cluster_sizes
    if (is.null(sizes)) {
      .bm_abort("This fit has no cluster structure.",
                hint = "Pass `cluster` to bb_mokken_h(), or supply labels here.")
    }
    # The fit records cluster SIZES but not cluster LABELS, so anything we
    # invent here would be a fiction dressed as data. Number them by rank and
    # flag that the caller should pass the real labels for more.
    nm <- names(sizes)
    if (is.null(nm)) {
      nm <- sprintf("#%02d", order(order(-as.integer(sizes))))
    }
    labels <- rep(nm, times = as.integer(sizes))
    info <- bm_cluster_info(labels)
    attr(info, "bm_derived_from_fit") <- is.null(names(sizes))
    return(info)
  }
  bm_cluster_info(x)
}
