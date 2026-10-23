#' Mokken scalability coefficients for a fixed item set
#'
#' Computes the pair, item and scale coefficients as functionals of one
#' complete-scale response distribution. Use this when you want the
#' coefficients alone; use [bb_mokken_h()] when you also want an interval.
#'
#' @details
#' Item and scale coefficients are **ratios of sums of covariances**, not
#' averages of pair coefficients. Polytomous denominators are the finite
#' comonotone maximum covariance at the weighted margins, computed exactly
#' rather than approximated. See `?bayesmokken-estimands`.
#'
#' Respondents with any missing response on the fixed item set are excluded,
#' and the counts are reported on the result. Run [bm_check_data()] first to
#' see how many that will be.
#'
#' @param data A matrix or data frame with at least two respondent rows and two
#'   uniquely named item columns.
#' @param items Character vector naming the fixed item set. Defaults to every
#'   column, which is what you want only if `data` contains items and nothing
#'   else.
#' @param score_map Named list mapping raw values to scores, one element per
#'   item that needs one. Required for character, factor and logical columns,
#'   and for reverse keying. Build it with [bm_score_map()].
#' @param missing_codes Named list of raw values to treat as missing before
#'   scoring, for example `list(item1 = c(-9, 99))`. Build it with
#'   [bm_missing_codes()].
#' @param weights Optional nonnegative respondent weights, aligned with the
#'   ORIGINAL input rows. They are normalized after complete-scale filtering,
#'   so you do not need to renormalize them yourself.
#' @param row_ids Optional unique row identifiers, used to report which
#'   respondents were excluded. Defaults to row names, then to input position.
#' @param missing Missing-data target. Only `"complete_scale"` is supported;
#'   the argument exists so the choice is visible in the call rather than
#'   implicit.
#' @param zero_tol Numerical tolerance below which a denominator counts as
#'   zero. The default is 64 machine epsilons. Change it only with a reason.
#' @param eta Margin below which a denominator counts as near zero rather than
#'   positive. Governs when the status field warns rather than the value being
#'   wrong.
#' @param warn If `TRUE`, emit one structured warning when any reported
#'   denominator is zero or near zero. The statuses are on the result either
#'   way; this only controls whether you are told out loud.
#' @param item_type `"auto"`, `"binary"` or `"ordinal"`. `"auto"` resolves to
#'   `"ordinal"` whenever `supports` or `cluster` is supplied, and to
#'   `"binary"` otherwise. See the decision table in [bb_mokken_h()].
#' @param supports Required for every ordinal call: a named list giving each
#'   selected item's finite increasing score support. Every observed value must
#'   appear in it, and unobserved categories the instrument allows should appear
#'   too, because the maximum covariance is computed at the declared support.
#'   Build it with [bm_supports()].
#' @param cluster Cluster labels aligned with the original input rows, for
#'   respondents nested in classrooms, clinics and the like. Filtered only
#'   after complete-scale rows are identified, so it must have one entry per
#'   input row, not per retained row.
#' @param cluster_target `"respondent"` or `"equal_cluster"`. These are
#'   different population quantities whenever cluster size is associated with
#'   any aspect of the response distribution, not merely its mean level. This
#'   argument must therefore be declared whenever `cluster` is supplied. The
#'   choice is yours to make and to report. See
#'   [bm_cluster_info()].
#' @param weight_law Weight-law declaration: `"auto"`, `"iid_respondent"`,
#'   `"complete_record"` or `"hierarchical_cluster"`. With `cluster`, `"auto"`
#'   resolves to `"complete_record"`, the complete-record Bayesian bootstrap
#'   of the clustered article's current version; `"hierarchical_cluster"` is
#'   the two-stage law of version 0.9.0, kept only for reproducing earlier
#'   results and warned about by [bb_mokken_h()]. The deterministic
#'   coefficient does not depend on the law; a clustered `mokken_h()` call
#'   records the resolved law as provenance.
#'
#' @return An object of class `bm_empirical_h`, with `pair`, `item` and `scale`
#'   tables, an `estimand` block recording exactly what was estimated, and a
#'   `provenance` block with the inclusion counts, a response-value-sensitive
#'   `analysis_input_hash`, and diagnostic preprocessing hashes.
#'   Use [bm_scale_h()], [bm_item_h()] and [bm_pair_h()] to get at it.
#'
#' @examples
#' # Binary
#' mokken_h(srs)
#'
#' # Polytomous, with the support declared
#' items <- paste0("wb", 1:6)
#' mokken_h(classroom_wb[, items], item_type = "ordinal",
#'          supports = bm_supports(classroom_wb, items = items))
#'
#' \donttest{
#' # Clustered, respondent-weighted target
#' mokken_h(classroom_wb[, items], item_type = "ordinal",
#'          supports = bm_supports(classroom_wb, items = items),
#'          cluster = classroom_wb$class_id,
#'          cluster_target = "respondent",
#'          weight_law = "complete_record")
#' }
#'
#' @family bayesmokken-estimation
#' @seealso [bb_mokken_h()] to add an interval, [bm_check_data()] to screen
#'   first, `?bayesmokken-estimands` for what is being estimated.
#' @export
mokken_h <- function(data,
                     items = colnames(data),
                     score_map = NULL,
                     missing_codes = NULL,
                     weights = NULL,
                     row_ids = NULL,
                     missing = "complete_scale",
                     zero_tol = 64 * .Machine$double.eps,
                     eta = sqrt(.Machine$double.eps),
                     warn = TRUE,
                     item_type = c("auto", "binary", "ordinal"),
                     supports = NULL,
                     cluster = NULL,
                     cluster_target = c("respondent", "equal_cluster"),
                     weight_law = c(
                       "auto", "iid_respondent", "complete_record",
                       "hierarchical_cluster"
                     )) {
  .bm_require_cluster_target(cluster, missing(cluster_target))
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn)) {
    .bm_abort("`warn` must be one nonmissing logical value.")
  }
  if (is.null(items) || !is.character(items) || length(items) < 2L ||
      anyNA(items) || any(!nzchar(items)) || anyDuplicated(items)) {
    .bm_abort("`items` must contain at least two unique, nonempty item names.")
  }
  source_names <- colnames(data)
  if (is.null(source_names) || any(!items %in% source_names)) {
    .bm_abort("Every requested item must be a named column in `data`.")
  }
  selected <- data[, items, drop = FALSE]
  selected_maps <- if (is.null(score_map)) NULL else score_map[intersect(names(score_map), items)]
  selected_missing <- if (is.null(missing_codes)) NULL else
    missing_codes[intersect(names(missing_codes), items)]
  selected_supports <- if (is.null(supports)) NULL else
    supports[intersect(names(supports), items)]
  item_type <- .bm_resolve_item_type(
    item_type,
    score_map = selected_maps,
    supports = selected_supports,
    cluster = cluster
  )
  mode <- .bm_validate_mode_contract(
    item_type = item_type,
    supports = selected_supports,
    cluster = cluster,
    cluster_target = cluster_target,
    weight_law = weight_law,
    random_weight = FALSE
  )
  # After the mode contract, so that an explicit `item_type = "binary"` keeps
  # its own more specific message. This catches the route that contract cannot
  # see: `item_type = "auto"` with a cluster resolves to "ordinal" before the
  # binary rules are ever consulted, so dichotomous data plus a cluster vector
  # -- the default-argument call -- silently entered the clustered path.
  .bm_guard_path_crossing(
    selected = data[, items, drop = FALSE],
    supports = selected_supports,
    cluster = cluster
  )
  if (identical(item_type, "ordinal")) {
    return(.bm_mokken_h_ordinal(
      selected = selected,
      score_map = selected_maps,
      missing_codes = selected_missing,
      supports = selected_supports,
      weights = weights,
      row_ids = row_ids,
      missing = missing,
      cluster = cluster,
      cluster_target = mode$cluster_target,
      zero_tol = zero_tol,
      eta = eta,
      warn = warn,
      weight_law = mode$weight_law
    ))
  }

  validated <- bm_validate_binary_data(
    selected,
    score_map = selected_maps,
    missing_codes = selected_missing
  )
  complete <- bm_complete_scale(
    validated,
    row_ids = row_ids,
    missing = missing
  )

  if (!is.null(weights)) {
    if (!is.numeric(weights) || length(weights) != complete$n_total ||
        anyNA(weights) || any(!is.finite(weights)) || any(weights < 0)) {
      .bm_abort("`weights` must be finite, nonnegative, and match original input rows.")
    }
    weights <- weights[complete$included]
  }
  masses <- bm_weighted_masses(complete$data, weights)
  complete$analysis_input_hash <- .bm_analysis_input_hash(
    complete, normalized_weights = weights
  )
  pair_result <- bm_h_pair(masses, zero_tol = zero_tol, eta = eta)
  aggregate <- bm_h_aggregate(
    pair_result$pair,
    complete$item_names,
    zero_tol = zero_tol,
    eta = eta
  )
  out <- new_bm_empirical_h(
    pair = pair_result$pair,
    item = aggregate$item,
    scale = aggregate$scale,
    complete = complete,
    masses = masses,
    zero_tol = zero_tol,
    eta = eta
  )

  affected <- c(
    out$pair$denominator_status,
    out$item$denominator_status,
    out$scale$denominator_status
  )
  if (isTRUE(warn) && any(affected != "positive")) {
    warning(structure(
      list(
        message = paste0(
          "One or more scalability denominators are zero or near zero; ",
          "inspect structured status fields."
        ),
        call = NULL,
        statuses = table(affected)
      ),
      class = c("bm_boundary_warning", "warning", "condition")
    ))
  }
  out
}
