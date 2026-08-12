# =============================================================================
# Console output for deterministic fits
# =============================================================================

#' Print a deterministic Mokken fit
#'
#' Shows what was estimated, how many respondents contributed, the scale
#' coefficient, and the extreme items. The full item table is in `summary()`.
#'
#' @param x A `bm_empirical_h` object from [mokken_h()].
#' @param digits Number of digits for coefficients.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' mokken_h(srs)
#'
#' @family bayesmokken-console
#' @seealso [summary.bm_empirical_h()], [bm_item_h()], [bb_mokken_h()]
#' @export
print.bm_empirical_h <- function(x, digits = 3L, ...) {
  semantics <- .bm_fit_semantic_lines(x)
  .bm_header(.bm_dot_join("Mokken scalability H", "deterministic"))
  .bm_field("estimand", .bm_estimand_line(x))
  .bm_field("rows", sprintf(
    "%d of %d complete (%d excluded, %.1f%%)",
    x$provenance$n_complete, x$provenance$n_total, x$provenance$n_excluded,
    100 * x$provenance$exclusion_fraction))
  .bm_field("items", nrow(x$item))
  if (!is.null(x$provenance$cluster_count)) {
    .bm_field("clusters", sprintf(
      "%d (sizes %d-%d)", x$provenance$cluster_count,
      min(x$provenance$cluster_sizes), max(x$provenance$cluster_sizes)))
  }
  .bm_blank()
  .bm_field("scale H", .bm_num(x$scale$H, digits),
            strrep(" ", 22L), "denominator: ", x$scale$denominator_status)

  it <- bm_item_h(x, include_interval = FALSE)
  if (nrow(it) >= 2L) {
    lo <- it[1L, ]
    hi <- it[nrow(it), ]
    .bm_field("items", sprintf(
      "lowest  %s  %s      highest  %s  %s",
      lo$item, .bm_num(lo$H, digits), hi$item, .bm_num(hi$H, digits)))
  }

  msg <- .bm_status_line(c(x$pair$denominator_status,
                           x$item$denominator_status,
                           x$scale$denominator_status))
  if (!is.null(msg)) {
    .bm_blank()
    .bm_para(paste("!", msg, "- see bm_diagnostics() and bm_regularity()."))
  }

  .bm_see(semantics$navigation)
  invisible(x)
}


#' Summarize a deterministic Mokken fit
#'
#' Adds the full item table, the pair denominator counts and the preprocessing
#' hashes to what `print()` shows.
#'
#' @param object A `bm_empirical_h` object.
#' @param ... Ignored.
#'
#' @return An object of class `summary_bm_empirical_h`.
#'
#' @examples
#' summary(mokken_h(srs))
#'
#' @family bayesmokken-console
#' @export
summary.bm_empirical_h <- function(object, ...) {
  # v0 contract: scale, item, n_total, n_complete, n_excluded, pair_status,
  # item_status, estimand. Kept, then extended.
  structure(
    list(
      n_total = object$provenance$n_total,
      n_complete = object$provenance$n_complete,
      n_excluded = object$provenance$n_excluded,
      item_status = table(object$item$denominator_status),
      scale = bm_scale_h(object),
      item = bm_item_h(object, include_interval = FALSE),
      pair_status = table(object$pair$denominator_status),
      provenance = bm_provenance(object),
      estimand = .bm_estimand_line(object),
      rows = object$provenance
    ),
    class = "summary_bm_empirical_h"
  )
}

#' @rdname summary.bm_empirical_h
#' @param x A `summary_bm_empirical_h` object.
#' @param digits Number of digits for coefficients.
#' @export
print.summary_bm_empirical_h <- function(x, digits = 3L, ...) {
  .bm_header(.bm_dot_join("Mokken scalability H", "summary"))
  .bm_field("estimand", x$estimand)
  .bm_field("rows", sprintf("%d of %d complete", x$rows$n_complete,
                            x$rows$n_total))
  .bm_field("scale H", .bm_num(x$scale$H, digits))
  .bm_blank()
  cat("  items (ascending coefficient order):\n")
  it <- x$item
  it$H <- .bm_num(it$H, digits)
  it$numerator <- .bm_num(it$numerator, digits)
  it$denominator <- .bm_num(it$denominator, digits)
  print(it, row.names = FALSE, right = FALSE)
  .bm_blank()
  .bm_para(.bm_fixed_scale_item_note)
  .bm_blank()
  .bm_field("pair denom", paste(sprintf("%s %d", names(x$pair_status),
                                        as.integer(x$pair_status)),
                                collapse = ", "))
  .bm_field("row hash", substr(x$provenance$row_hash, 1L, 12L))
  .bm_see("bm_item_h()", "bm_pair_h()", "bm_diagnostics()")
  invisible(x)
}


# One sentence describing what was estimated, shared by both printers.
.bm_estimand_line <- function(x) {
  type <- x$estimand$item_type
  n_items <- nrow(x$item)
  base <- sprintf("fixed %d-item %s scale, complete-scale respondents",
                  n_items, type)
  tgt <- x$estimand$cluster_target
  if (is.null(tgt)) return(base)
  sprintf("fixed %d-item %s scale, %s target", n_items, type,
          if (identical(tgt, "respondent")) "respondent-weighted" else "equal-cluster")
}
