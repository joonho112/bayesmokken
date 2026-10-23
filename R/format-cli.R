# =============================================================================
# Opt-in cli formatters
#
# The default print methods use base R only, so the package is fully usable
# with nothing installed beyond its Imports. These three functions add colour,
# alignment and rules for people who have `cli`, and they are the ONLY place
# the package touches it.
#
# Colour is used for status, never as the sole carrier of meaning: a reader on
# a monochrome terminal loses nothing but the emphasis.
# =============================================================================

.bm_need_cli <- function() {
  if (!requireNamespace("cli", quietly = TRUE)) {
    .bm_abort(
      "The cli-decorated formatters need the 'cli' package.",
      hint = "install.packages(\"cli\"), or just print the object as usual."
    )
  }
  invisible(TRUE)
}

.bm_cli_see <- function(calls) {
  cli::cli_text("")
  cli::cli_text(paste0(
    "See: ", paste(calls, collapse = .bm_dot_separator())
  ))
}

#' Rich console output for a fit
#'
#' A `cli`-decorated alternative to `print()`. Same content, more typography.
#' Requires the suggested `cli` package; the ordinary print methods do not.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examplesIf requireNamespace("cli", quietly = TRUE)
#' format_bm_fit_cli(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @seealso [print.bm_bb_h()]
#' @export
format_bm_fit_cli <- function(x, ...) {
  .bm_need_cli()
  parts <- .bm_parts(x)
  itv <- parts$interval
  semantics <- .bm_fit_semantic_lines(x)
  s <- bm_scale_h(x)

  cli::cli_h1("Mokken scalability H")
  details <- c(
    "estimand" = .bm_estimand_line(parts$empirical),
    "method" = if (is.na(s$method_id)) "deterministic (no interval)" else s$method_id,
    "sample" = sprintf("n = %d, %d items%s", s$n, s$n_items,
                       if (is.na(s$n_clusters)) "" else
                         sprintf(", %d clusters", s$n_clusters))
  )
  if (!is.null(semantics$validation_status)) {
    details <- c(
      details,
      "validation" = semantics$validation_status,
      "scope" = semantics$validation_scope,
      "range" = semantics$validation_range
    )
  }
  cli::cli_dl(details)
  if (!is.null(itv)) {
    cli::cli_text("")
    cli::cli_alert_info(sprintf(
      "H = {.strong %s}, 95%% CI [%s, %s]; status %s",
      .bm_num(s$H), .bm_num(s$lower), .bm_num(s$upper),
      semantics$numerical_status))
    cli::cli_alert(sprintf(
      "raw [%s, %s], width multiplier %s",
      .bm_num(s$raw_lower), .bm_num(s$raw_upper),
      .bm_num(s$width_multiplier, 4L)))
    if (itv$defined_draws < itv$total_draws) {
      cli::cli_alert_warning(sprintf(
        "%d of %d draws undefined",
        itv$total_draws - itv$defined_draws, itv$total_draws))
    }
  }
  if (!is.null(semantics$interpretation)) {
    cli::cli_text("")
    interpretation <- semantics$interpretation
    cli::cli_text("{.emph {interpretation}}")
  }
  .bm_cli_see(semantics$navigation)
  invisible(x)
}


#' Rich console output for an interval
#'
#' @param x A `bm_bb_h` object or a `bm_h_interval`.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examplesIf requireNamespace("cli", quietly = TRUE)
#' format_bm_interval_cli(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
format_bm_interval_cli <- function(x, ...) {
  .bm_need_cli()
  itv <- if (inherits(x, "bm_h_interval")) x else .bm_parts(x)$interval
  if (is.null(itv)) {
    .bm_abort("This object has no interval.",
              hint = "Use bb_mokken_h() rather than mokken_h().")
  }
  semantics <- .bm_interval_semantic_lines(itv)
  cli::cli_h2("Interval")
  reported_label <- if (identical(
    itv$construction, "reflected_error_expansion_about_point"
  )) {
    "adjusted"
  } else if (identical(itv$construction, "percentile")) {
    "percentile"
  } else {
    "calibrated"
  }
  details <- c(
    "method" = itv$method_id,
    "status" = semantics$numerical_status,
    "point" = .bm_num(itv$point_estimate)
  )
  details[reported_label] <- .bm_ci(itv$lower, itv$upper)
  details <- c(
    details,
    "raw" = .bm_ci(itv$raw_lower, itv$raw_upper),
    "width x" = .bm_num(itv$width_multiplier, 4L),
    "draws" = sprintf("%d of %d defined", itv$defined_draws, itv$total_draws)
  )
  if (!is.null(semantics$validation_status)) {
    details <- c(
      details,
      "validation" = semantics$validation_status,
      "scope" = semantics$validation_scope,
      "range" = semantics$validation_range
    )
  }
  cli::cli_dl(details)
  cli::cli_text("")
  interpretation <- semantics$interpretation
  cli::cli_text("{.emph {interpretation}}")
  .bm_cli_see(semantics$navigation)
  invisible(x)
}


#' Rich console output for the item table
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param include_interval If `TRUE`, include raw, uncalibrated item intervals
#'   when bootstrap draws are available. The default is `FALSE`.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examplesIf requireNamespace("cli", quietly = TRUE)
#' format_bm_items_cli(bb_mokken_h(srs, draws = 200, seed = 1))
#'
#' @family bayesmokken-console
#' @export
format_bm_items_cli <- function(x, include_interval = FALSE, ...) {
  .bm_need_cli()
  it <- bm_item_h(x, include_interval = include_interval)
  has_ci <- "bb_lower" %in% names(it)
  cli::cli_h2("Items (ascending coefficient order)")
  lines <- vapply(seq_len(nrow(it)), function(k) {
    base <- sprintf("%-10s %s", it$item[k], .bm_num(it$H[k]))
    if (has_ci) {
      base <- paste0(base, "  ", .bm_ci(it$bb_lower[k], it$bb_upper[k]))
    }
    if (it$denominator_status[k] != "positive") {
      base <- paste0(base, "  [", it$denominator_status[k], " denominator]")
    }
    base
  }, character(1))
  cli::cli_verbatim(lines)
  if (has_ci) {
    cli::cli_text("")
    note <- .bm_uncalibrated_note
    cli::cli_alert_warning("{note}")
  }
  cli::cli_text("")
  fixed_scale_note <- .bm_fixed_scale_item_note
  cli::cli_alert_info("{fixed_scale_note}")
  .bm_cli_see(c("bm_item_h()", "bm_plot_item_h()"))
  invisible(x)
}
