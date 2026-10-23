# =============================================================================
# Console output for the remaining classes
# =============================================================================

#' Print a typed interval
#'
#' @param x A `bm_h_interval` object, as carried by `fit$interval`.
#' @param digits Number of digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bb_mokken_h(srs, draws = 200, seed = 1)$interval
#'
#' @family bayesmokken-console
#' @export
print.bm_h_interval <- function(x, digits = 3L, ...) {
  semantics <- .bm_interval_semantic_lines(x)
  .bm_header("Interval")
  .bm_field("method", x$method_id,
            "      status  ", semantics$numerical_status)
  .bm_field("level", paste0(format(100 * x$level), "%"),
            "                     draws   ",
            sprintf("%d of %d defined", x$defined_draws, x$total_draws))
  if (!is.null(semantics$validation_status)) {
    .bm_field("validation", semantics$validation_status)
    .bm_field("scope", semantics$validation_scope)
    .bm_field("range", semantics$validation_range)
  }
  .bm_field("point", .bm_num(x$point_estimate, digits))
  construction <- x$construction
  if (identical(construction, "reflected_error_expansion_about_point")) {
    .bm_field("adjusted", .bm_ci(x$lower, x$upper, digits),
              "        width x ", .bm_num(x$width_multiplier, 4L),
              "  (k_G = ", .bm_num(x$root_multiplier, 4L),
              ", reflected about the point estimate)")
    if (isTRUE(x$set_empty)) {
      .bm_field("", sprintf(
        "! empty confidence set: reflected endpoints %s lie above 1",
        .bm_ci(x$reflected_lower, x$reflected_upper, digits)))
    } else if (isTRUE(x$upper_intersection)) {
      .bm_field("", sprintf(
        "upper endpoint capped at 1 (reflected upper endpoint %s)",
        .bm_num(x$reflected_upper, digits)))
    }
  } else if (identical(construction, "percentile")) {
    .bm_field("percentile", .bm_ci(x$lower, x$upper, digits),
              "        width x ", .bm_num(x$width_multiplier, 4L),
              "  (no adjustment)")
  } else {
    .bm_field("calibrated", .bm_ci(x$lower, x$upper, digits),
              "        width x ", .bm_num(x$width_multiplier, 4L),
              "  (gamma = ", .bm_num(x$gamma, 2L), ")")
  }
  .bm_field("raw", .bm_ci(x$raw_lower, x$raw_upper, digits),
            "        quantile type ", x$quantile_type,
            ", clipping ", tolower(x$endpoint_clipping))
  .bm_blank()
  .bm_para(paste("Interpretation:", semantics$interpretation))
  .bm_see(semantics$navigation, blank = FALSE)
  invisible(x)
}

#' @rdname print.bm_h_interval
#' @param digits Number of digits.
#' @export
format.bm_h_interval <- function(x, digits = 3L, ...) {
  paste0(.bm_num(x$point_estimate, digits), " ",
         .bm_ci(x$lower, x$upper, digits))
}


#' Print a data screen
#'
#' @param x A `bm_data_check` object from [bm_check_data()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bm_check_data(srs)
#'
#' @family bayesmokken-console
#' @export
print.bm_data_check <- function(x, ...) {
  .bm_header("Data check")
  .bm_field("rows / items", sprintf("%d x %d", x$rows$n_total, nrow(x$items)))
  vals <- unique(unlist(strsplit(x$items$observed, " ")))
  .bm_field("item type", sprintf("%s (%s)   values %s", x$item_type,
                                 x$item_type_source,
                                 paste(utils::head(sort(as.numeric(vals)), 8),
                                       collapse = " ")))
  .bm_field("complete-scale", sprintf("%d of %d (%d excluded)",
                                      x$rows$n_complete, x$rows$n_total,
                                      x$rows$n_excluded))
  const <- x$items$item[x$items$n_values <= 1L]
  .bm_field("constant items", if (length(const)) paste(const, collapse = ", ") else "none")
  rev <- x$flags$item[x$flags$level == "warning" & !is.na(x$flags$item)]
  .bm_field("reverse flags", if (length(rev)) paste(rev, collapse = ", ") else "none")
  if (!is.null(x$clusters)) {
    ci <- x$clusters
    .bm_field("clusters", sprintf(
      "%d   sizes %d-%d (median %.0f)   size/mean rho %s",
      ci$G, min(ci$sizes), max(ci$sizes), stats::median(ci$sizes),
      .bm_num(if (!is.null(ci$size_mean_score_spearman)) {
        ci$size_mean_score_spearman
      } else {
        ci$informative_index
      }, 2L)))
  }
  notes <- x$flags[is.na(x$flags$item) |
                     x$flags$level %in% c("error", "warning"), , drop = FALSE]
  if (nrow(notes)) {
    .bm_blank()
    for (k in seq_len(nrow(notes))) {
      .bm_para(paste0("! ", if (!is.na(notes$item[k])) paste0(notes$item[k], ": ") else "",
                      notes$message[k]))
    }
  }
  .bm_blank()
  cat("  Next: ", x$next_call, "\n", sep = "")
  invisible(x)
}


#' Print a cluster summary
#'
#' @param x A `bm_cluster_info` object from [bm_cluster_info()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bm_cluster_info(classroom_wb$class_id,
#'                 data = classroom_wb[, paste0("wb", 1:6)])
#'
#' @family bayesmokken-console
#' @export
print.bm_cluster_info <- function(x, ...) {
  rho <- if (!is.null(x$size_mean_score_spearman)) {
    x$size_mean_score_spearman
  } else {
    x$informative_index
  }
  .bm_header("Cluster structure")
  .bm_field("clusters", sprintf("G = %d   N = %d", x$G, x$N))
  .bm_field("sizes", sprintf("%d-%d (median %.0f, mean %.1f)",
                             min(x$sizes), max(x$sizes),
                             stats::median(x$sizes), mean(x$sizes)))
  .bm_field("size spread", .bm_sparkline(x$table$size))
  .bm_field("target weight", sprintf(
    "respondent %s-%s   equal-cluster %s (all clusters)",
    .bm_num(min(x$table$weight_respondent), 4L),
    .bm_num(max(x$table$weight_respondent), 4L),
    .bm_num(x$table$weight_equal[1L], 4L)))
  .bm_field("size/mean rho", .bm_num(rho, 2L))
  if (length(x$singletons)) {
    .bm_field("singletons", paste(x$singletons, collapse = ", "))
  }
  .bm_blank()
  .bm_para(if (is.na(rho)) {
    paste("The size--mean-score association was not computed. This statistic",
          "does not measure agreement between the two target H values; fit",
          "both targets and compare their deterministic coefficients directly.")
  } else {
    paste("This Spearman correlation describes cluster size versus mean total",
          "score only. It does not measure agreement between the two target H",
          "values; fit both targets and compare their deterministic",
          "coefficients directly.")
  })
  .bm_see("bm_plot_clusters()", "?bayesmokken-estimands")
  invisible(x)
}


#' Print a diagnostic report
#'
#' @param x A `bm_diagnostic_report` object from [bm_diagnostics()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bm_diagnostics(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
print.bm_diagnostic_report <- function(x, ...) {
  .bm_header("Diagnostics")
  d <- x$denominator
  .bm_field("denominators", paste(sprintf("%s %d positive", d$level, d$positive),
                                  collapse = .bm_dot_separator()))
  bad <- sum(d$near_zero) + sum(d$zero)
  if (bad) {
    .bm_field("", sprintf("! %d at or near zero", bad))
  }
  reg <- x$regularity
  if (nrow(reg)) {
    .bm_field("regularity", sprintf(
      "smallest max-covariance %s (%s-%s)",
      .bm_num(reg$min_pair_denominator[1L], 4L), reg$item[1L],
      reg$worst_pair[1L]))
  }
  .bm_field("transport", if (is.null(x$knots)) {
    "not applicable (binary path)"
  } else {
    sprintf("%d knot record(s)", nrow(x$knots))
  })
  if (!is.null(x$mc)) {
    .bm_field("Monte Carlo", sprintf(
      "endpoint MCSE %s / %s   prefix-half shift %s",
      .bm_num(x$mc$lower_endpoint_mcse, 4L),
      .bm_num(x$mc$upper_endpoint_mcse, 4L),
      .bm_num(x$mc$prefix_half_endpoint_max_difference, 4L)))
  }
  .bm_blank()
  .bm_para(paste("Overall:", x$overall))
  .bm_para(paste("This describes the arithmetic, not the quality of the scale.",
                 "No verdict on the scale is offered."))
  .bm_see("bm_regularity()", "bm_mc_diagnostics()", "bm_guardrails()")
  invisible(x)
}


#' Print a results table
#'
#' @param x A `bm_report` object from [bm_report()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bm_report(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
print.bm_report <- function(x, ...) {
  if (!is.null(x$rendered)) {
    cat(x$rendered, sep = "\n")
    return(invisible(x))
  }
  .bm_header(.bm_dot_join("Results", paste(x$what, "level")))
  tab <- x$table
  for (j in seq_along(tab)) {
    if (is.double(tab[[j]])) tab[[j]] <- .bm_num(tab[[j]], x$digits)
  }
  print(tab, row.names = FALSE, right = FALSE)
  .bm_blank()
  for (n in x$notes) .bm_para(paste("Note:", n))
  invisible(x)
}


#' Print a benchmark comparison
#'
#' @param x A `bm_benchmark` object from [bm_benchmark()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bm_benchmark(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
print.bm_benchmark <- function(x, ...) {
  print(as.data.frame(x), row.names = FALSE, right = FALSE)
  .bm_blank()
  .bm_para(.bm_benchmark_note)
  invisible(x)
}


# A tiny ASCII sparkline, so cluster-size inequality is visible without a plot.
.bm_sparkline <- function(v, width = 24L) {
  if (!length(v)) return("")
  blocks <- c(".", ":", "-", "=", "+", "*", "#")
  v <- sort(v, decreasing = TRUE)
  if (length(v) > width) {
    idx <- round(seq(1, length(v), length.out = width))
    v <- v[idx]
  }
  rng <- range(v)
  k <- if (diff(rng) == 0) rep(4L, length(v)) else {
    1L + as.integer(round((length(blocks) - 1L) * (v - rng[1]) / diff(rng)))
  }
  paste0(paste(blocks[k], collapse = ""),
         sprintf("  (%d-%d)", rng[1], rng[2]))
}
