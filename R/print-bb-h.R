# =============================================================================
# Console output for bootstrap fits
#
# The message this screen has to carry is what the calibration did. The raw
# interval and the corrected one are shown together, with the multiplier, so
# the reader can see that the binary correction widens and the clustered one
# narrows. Hiding the raw interval would make the correction invisible.
# =============================================================================

#' Print a Bayesian-bootstrap Mokken fit
#'
#' Shows the estimand, the frozen method identifier, the sample, and the scale
#' coefficient with both its raw and calibrated intervals.
#'
#' @param x A `bm_bb_h` object from [bb_mokken_h()].
#' @param digits Number of digits for coefficients.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' bb_mokken_h(srs, draws = 200, seed = 1)
#'
#' @family bayesmokken-console
#' @seealso [summary.bm_bb_h()], [bm_scale_h()], [bm_plot_draws()]
#' @export
print.bm_bb_h <- function(x, digits = 3L, ...) {
  clustered <- identical(
    x$bb_type, "two_stage_hierarchical_cluster_bayesian_bootstrap"
  )
  itv <- x$interval
  semantics <- .bm_fit_semantic_lines(x)

  .bm_header(if (clustered) {
    .bm_dot_join(
      "Mokken scalability H", "two-stage hierarchical Bayesian bootstrap"
    )
  } else {
    .bm_dot_join("Mokken scalability H", "Bayesian-bootstrap interval")
  })

  .bm_field("estimand", .bm_estimand_line(x$empirical))
  if (clustered) {
    .bm_field("target", x$sampling$target_id)
  }
  if (!is.null(itv)) {
    .bm_field("method", itv$method_id,
              "  (raw base: ", itv$base_method_id, ")")
  }
  .bm_field("sample", .bm_sample_line(x, clustered))
  if (!is.null(semantics$validation_status)) {
    .bm_field("validation", semantics$validation_status)
    .bm_field("scope", semantics$validation_scope)
    .bm_field("range", semantics$validation_range)
  }
  .bm_blank()

  if (!is.null(itv)) {
    .bm_field("scale H", .bm_num(itv$point_estimate, digits),
              "   ", format(100 * itv$level), "% CI ",
              .bm_ci(itv$lower, itv$upper, digits),
              strrep(" ", 6L), "status: ", semantics$numerical_status)
    same <- identical(.bm_ci(itv$raw_lower, itv$raw_upper, digits),
                      .bm_ci(itv$lower, itv$upper, digits))
    .bm_field("", "raw     ", .bm_ci(itv$raw_lower, itv$raw_upper, digits),
              "  ->  width x ", .bm_num(itv$width_multiplier, 4L))
    if (same) {
      # The two intervals round to the same string at this precision. Say so,
      # rather than let the display suggest the correction did nothing.
      .bm_field("", sprintf(
        "(the correction changes the length by %s, below %d-digit display)",
        .bm_num((itv$upper - itv$lower) - (itv$raw_upper - itv$raw_lower), 5L),
        digits))
    }
    if (itv$defined_draws < itv$total_draws) {
      .bm_field("", sprintf("! %d of %d draws undefined",
                            itv$total_draws - itv$defined_draws,
                            itv$total_draws))
    }
  } else {
    .bm_field("scale H", .bm_num(x$empirical$scale$H, digits),
              "   (no interval computed)")
  }

  if (!is.null(semantics$interpretation)) {
    .bm_blank()
    .bm_para(paste("Interpretation:", semantics$interpretation))
  }

  .bm_see(semantics$navigation)
  invisible(x)
}


#' Summarize a Bayesian-bootstrap Mokken fit
#'
#' Adds draw summaries for the scale and the items, and the Monte Carlo
#' diagnostics, to what `print()` shows.
#'
#' @param object A `bm_bb_h` object.
#' @param ... Ignored.
#'
#' @return An object of class `summary_bm_bb_h`.
#'
#' @examples
#' summary(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
summary.bm_bb_h <- function(object, ...) {
  # The first five elements are the v0 contract and are reproduced exactly:
  # tests written against 0.0.0.9001 index sm$scale_draws by name and check its
  # nine columns. v1 adds to this list; it does not reshape it (INV-2).
  base <- if (!is.null(object$summaries)) {
    object$summaries
  } else {
    bm_bb_summary(object)
  }
  structure(
    c(unclass(base),
      list(
        scale = bm_scale_h(object),
        item = bm_item_h(object, include_interval = FALSE),
        mc = object$mc_diagnostics,
        estimand = .bm_estimand_line(object$empirical),
        clustered = identical(
          object$bb_type,
          "two_stage_hierarchical_cluster_bayesian_bootstrap")
      )),
    class = "summary_bm_bb_h"
  )
}

#' @rdname summary.bm_bb_h
#' @param x A `summary_bm_bb_h` object.
#' @param digits Number of digits for coefficients.
#' @export
print.summary_bm_bb_h <- function(x, digits = 3L, ...) {
  .bm_header(.bm_dot_join("Mokken scalability H", "summary"))
  .bm_field("estimand", x$estimand)
  itv <- x$interval
  semantics <- .bm_interval_semantic_lines(
    itv,
    navigation = c("bm_item_h()", "bm_mc_diagnostics()", "bm_report()")
  )
  if (!is.null(itv)) {
    .bm_field("method", itv$method_id)
    if (!is.null(semantics$validation_status)) {
      .bm_field("validation", semantics$validation_status)
      .bm_field("scope", semantics$validation_scope)
      .bm_field("range", semantics$validation_range)
    }
    .bm_field("scale H", .bm_num(itv$point_estimate, digits), "   ",
              .bm_ci(itv$lower, itv$upper, digits))
    .bm_field("draws", sprintf("%d defined of %d", itv$defined_draws,
                               itv$total_draws))
  }
  .bm_blank()
  cat("  items (ascending coefficient order):\n")
  it <- x$item
  has_ci <- all(c("bb_lower", "bb_upper") %in% names(it))
  for (col in c("H", "bb_mean", "bb_lower", "bb_upper")) {
    if (col %in% names(it)) it[[col]] <- .bm_num(it[[col]], digits)
  }
  keep <- intersect(c("item", "H", "bb_mean", "bb_lower", "bb_upper",
                      "denominator_status"), names(it))
  print(it[, keep, drop = FALSE], row.names = FALSE, right = FALSE)
  .bm_blank()
  .bm_para(.bm_fixed_scale_item_note)
  if (has_ci) {
    .bm_blank()
    .bm_para(.bm_uncalibrated_note)
  }

  if (!is.null(x$mc) && is.finite(x$mc$lower_endpoint_mcse)) {
    .bm_blank()
    .bm_field("MC endpoint", sprintf(
      "MCSE %s / %s   batch max deviation %s",
      .bm_num(x$mc$lower_endpoint_mcse, 4L),
      .bm_num(x$mc$upper_endpoint_mcse, 4L),
      .bm_num(x$mc$interval_batch_max_deviation, 4L)))
  }
  .bm_see(semantics$navigation)
  invisible(x)
}


.bm_sample_line <- function(x, clustered) {
  n <- x$grouping$n_respondents
  b <- length(x$draws$scale)
  if (clustered) {
    sizes <- x$grouping$counts
    .bm_dot_join(
      sprintf("n = %d", n),
      sprintf("clusters = %d (sizes %d-%d)",
              x$grouping$n_clusters, min(sizes), max(sizes)),
      sprintf("draws = %d", b)
    )
  } else {
    .bm_dot_join(
      sprintf("n = %d complete", n),
      sprintf("patterns = %d", x$grouping$n_patterns),
      sprintf("draws = %d", b)
    )
  }
}
