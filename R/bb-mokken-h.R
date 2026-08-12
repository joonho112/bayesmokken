#' Bayesian-bootstrap confidence interval for a fixed-scale Mokken coefficient
#'
#' Computes the scalability coefficient for a fixed item set and attaches a
#' confidence interval whose coverage has been checked in prespecified
#' simulations. Binary items take the respondent Bayesian bootstrap; polytomous
#' items answered by respondents nested in clusters take the two-stage
#' hierarchical Bayesian bootstrap. Both calibrations are frozen.
#'
#' @details
#' # Which path your call takes
#'
#' Public mode rules are applied in this order:
#'
#' 1. Supplying `cluster` requires an explicit `cluster_target`; the package
#'    never chooses between the respondent-weighted and equal-cluster targets.
#' 2. `item_type = "auto"` resolves to ordinal when `supports` or `cluster` is
#'    supplied, and to binary otherwise.
#' 3. Every resolved ordinal call requires a named, increasing support for
#'    every selected item. Observed values are not substituted for a support.
#' 4. The binary path rejects `supports`, `cluster` and
#'    `weight_law = "hierarchical_cluster"`.
#' 5. Ordinal [mokken_h()] accepts the iid respondent law without clusters and
#'    the hierarchical law with clusters. Ordinal `bb_mokken_h()` additionally
#'    requires clusters and the hierarchical law.
#'
#' The supported lanes are summarized below. `required` means the argument
#' must occur in the call, even though the published formal default is retained
#' for call compatibility.
#'
#' | resolved `item_type` | `supports` | `cluster` | `cluster_target` | `weight_law` | result |
#' |----|----|----|----|----|----|
#' | binary | absent | absent | not used | `"auto"` or `"iid_respondent"` | point H; respondent BB interval |
#' | ordinal | required | absent | not used | `"auto"` or `"iid_respondent"` | point H only; interval call errors |
#' | ordinal | required | present | required | `"auto"` or `"hierarchical_cluster"` | clustered point H; hierarchical BB interval |
#'
#' For `bb_mokken_h()`, `interval = "calibrated"` selects the validated
#' width-corrected confidence interval and `interval = "equal_tail"` selects
#' the uncorrected comparator. Both interval choices use the sampling law fixed
#' by the lane above. Supplying `cluster` also makes `"auto"` resolve to
#' ordinal for 0/1 data, but it does not create an observed two-point support:
#' that support must still be declared explicitly. A calibrated clustered
#' interval additionally requires at least one selected item with more than two
#' declared support points and at least 22 complete-scale clusters. Counts above
#' 80 are returned with a typed extrapolation warning and explicit validation
#' metadata. The equal-tail comparator is not a validated substitute outside
#' this scope. See [bm_guardrails()].
#'
#' # The five arguments most people need
#'
#' `data`, `draws`, `seed` for a binary scale; add `supports`, `cluster`,
#' `cluster_target` and `weight_law` for clustered polytomous data. Everything
#' else has a frozen default that should be left alone unless you have a
#' specific reason.
#'
#' @section Frozen calibration:
#' The width constants and the confidence level are not arguments. \eqn{\gamma}
#' = 0.25 for the binary correction and \eqn{\kappa} = 1.5 for the clustered
#' one were fixed on training data, validated on disjoint held-out conditions,
#' and locked. `level` must be exactly 0.95. Coverage at any other setting has
#' not been examined, so the package will not produce one. The clustered
#' confirmation covered polytomous designs at cluster counts 22, 35, 55 and 80;
#' it is not a universal theorem for other supports or cluster counts. See
#' `?bayesmokken-intervals`.
#'
#' @section What this interval is not:
#' It is a repeated-sampling confidence interval, not a posterior credible
#' interval. The package returns no threshold probabilities, performs no item
#' selection, and propagates no scale-selection uncertainty. If your item set
#' was chosen by inspecting these data, this interval understates your
#' uncertainty.
#'
#' @section Only the scale coefficient is calibrated:
#' `fit$draws$item` and `fit$draws$pair` are available and [bm_item_h()] will
#' summarize them, but the width correction was validated for the scale
#' coefficient alone. Item- and pair-level intervals are reported as raw
#' quantiles and labeled `uncalibrated`.
#'
#' @inheritParams mokken_h
#' @param draws Number of Bayesian-bootstrap draws. The frozen default is
#'   2,000. Raising it reduces Monte Carlo error in the endpoints and costs
#'   time linearly; [bm_plot_mc_stability()] shows whether it is worth it. A
#'   minimum fraction must come out finite -- `max(100, ceiling(0.90 * draws))`
#'   on the binary path, `max(20, ceiling(0.99 * draws))` on the clustered one
#'   -- and a shortfall produces a typed interval failure rather than an
#'   interval computed from whatever survived.
#' @param seed Nonnegative integer seed for the L'Ecuyer-CMRG draw streams.
#'   Fractional values are rejected rather than truncated, because `1.1` and
#'   `1L` would otherwise silently give the same answer. Always set it.
#' @param batch_size Draws evaluated per processing batch. Affects memory and
#'   nothing else: changing it does not change which draws are taken.
#' @param workers Requested worker count. Recorded for provenance; the
#'   reference engine keeps the worker-invariant seed map and currently runs
#'   serially, so results do not depend on this.
#' @param interval `"calibrated"` for the validated method, `"equal_tail"` for
#'   the uncorrected comparator. The comparator exists so you can see what the
#'   correction did; it is not an alternative recommendation.
#' @param level Confidence level. Must be exactly `0.95`.
#'
#' @return An object of class `bm_bb_h`:
#'
#' \describe{
#'   \item{`empirical`}{The deterministic fit, as returned by [mokken_h()].}
#'   \item{`draws`}{`$scale` (vector), `$item` and `$pair` (matrices, one
#'     column per item or pair). Undefined draws are kept as `NA`.}
#'   \item{`interval`}{A `bm_h_interval`: method identifier, status, point
#'     estimate, raw and calibrated endpoints, the width multiplier, draw
#'     counts, and the policy fields recording that endpoints are never
#'     clipped and no method cascade is permitted. Clustered intervals also
#'     separate respondent `n_complete` from cluster-based calibration
#'     provenance and record the validation scope and status.}
#'   \item{`summaries`, `mc_diagnostics`}{Draw summaries and endpoint Monte
#'     Carlo diagnostics.}
#'   \item{`grouping`, `rng`, `sampling`, `computation`}{Provenance: pattern or
#'     cluster structure, seed and generator, weight law and target
#'     identifiers.}
#' }
#'
#' Reach for [bm_scale_h()], [bm_item_h()] and [bm_interval()] rather than
#' indexing this structure by hand.
#'
#' @references
#' Lee, J. (2026). Calibrated Bayesian bootstrap confidence intervals for the
#' fixed binary Mokken scalability coefficient. Manuscript under review.
#'
#' Lee, J. (2026). Cluster-aware confidence intervals for the weighted
#' polytomous Mokken scalability coefficient. Manuscript under review.
#'
#' @examples
#' # Binary: the published application
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' fit
#' bm_scale_h(fit)
#'
#' \donttest{
#' # Clustered polytomous, with the target declared
#' items <- paste0("wb", 1:6)
#' cfit <- bb_mokken_h(classroom_wb[, items],
#'                     item_type = "ordinal",
#'                     supports = bm_supports(classroom_wb, items = items),
#'                     cluster = classroom_wb$class_id,
#'                     cluster_target = "respondent",
#'                     weight_law = "hierarchical_cluster",
#'                     draws = 500, seed = 1)
#' bm_scale_h(cfit)
#'
#' # See what the correction did
#' bm_plot_draws(fit)
#' }
#'
#' # Guard rails are typed errors, not silent fallbacks
#' try(bb_mokken_h(srs, draws = 200, seed = 1, level = 0.90))
#'
#' @family bayesmokken-estimation
#' @seealso [mokken_h()] for the coefficient without an interval,
#'   [bm_check_data()] to screen data first, [bm_scale_h()] to extract the
#'   result, [bm_plot_draws()] to see the correction, `?bayesmokken-intervals`
#'   for what the interval means.
#' @export
bb_mokken_h <- function(data,
                        items = colnames(data),
                        score_map = NULL,
                        missing_codes = NULL,
                        row_ids = NULL,
                        missing = "complete_scale",
                        draws = 2000L,
                        seed = 1L,
                        batch_size = 250L,
                        workers = 1L,
                        interval = c("calibrated", "equal_tail"),
                        level = 0.95,
                        zero_tol = 64 * .Machine$double.eps,
                        eta = sqrt(.Machine$double.eps),
                        warn = TRUE,
                        item_type = c("auto", "binary", "ordinal"),
                        supports = NULL,
                        cluster = NULL,
                        cluster_target = c("respondent", "equal_cluster"),
                        weight_law = c(
                          "auto", "iid_respondent", "hierarchical_cluster"
                        )) {
  .bm_require_cluster_target(cluster, missing(cluster_target))
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn)) {
    .bm_abort("`warn` must be one nonmissing logical value.")
  }
  interval <- .bm_match_interval(interval)
  level <- .bm_validate_interval_level(level)
  selected_maps <- if (is.null(score_map)) NULL else
    score_map[intersect(names(score_map), items)]
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
    random_weight = TRUE
  )
  # See the note at the same point in `mokken_h()`: this runs after the mode
  # contract so an explicit `item_type = "binary"` keeps its own message, and
  # it exists because `item_type = "auto"` plus a cluster resolves to "ordinal"
  # without ever consulting the binary rules.
  .bm_guard_path_crossing(
    selected = data[, items, drop = FALSE],
    supports = selected_supports,
    cluster = cluster
  )
  if (identical(item_type, "ordinal")) {
    return(.bm_bb_mokken_h_ordinal(
      data = data,
      items = items,
      score_map = score_map,
      missing_codes = missing_codes,
      row_ids = row_ids,
      missing = missing,
      draws = draws,
      seed = seed,
      batch_size = batch_size,
      workers = workers,
      interval = interval,
      level = level,
      zero_tol = zero_tol,
      eta = eta,
      warn = warn,
      supports = supports,
      cluster = cluster,
      cluster_target = mode$cluster_target
    ))
  }
  empirical <- mokken_h(
    data = data,
    items = items,
    score_map = score_map,
    missing_codes = missing_codes,
    row_ids = row_ids,
    missing = missing,
    zero_tol = zero_tol,
    eta = eta,
    warn = warn
  )

  selected <- data[, items, drop = FALSE]
  selected_maps <- if (is.null(score_map)) NULL else
    score_map[intersect(names(score_map), items)]
  selected_missing <- if (is.null(missing_codes)) NULL else
    missing_codes[intersect(names(missing_codes), items)]
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
  complete$analysis_input_hash <- .bm_analysis_input_hash(complete)
  if (!identical(
        complete$analysis_input_hash,
        empirical$provenance$analysis_input_hash
      ) ||
      !identical(complete$row_hash, empirical$provenance$row_hash) ||
      !identical(complete$score_map_hash, empirical$provenance$score_map_hash)) {
    .bm_abort("Deterministic and BB preprocessing hashes disagree.",
              "bm_invariant_error")
  }

  out <- bm_bb_respondent(
    complete = complete,
    empirical = empirical,
    draws = draws,
    seed = seed,
    batch_size = batch_size,
    workers = workers,
    zero_tol = zero_tol,
    eta = eta
  )
  out$interval <- .bm_interval_from_draws(
    bb_draws = out$draws$scale,
    point_estimate = empirical$scale$H,
    n_complete = empirical$provenance$n_complete,
    interval = interval,
    level = level
  )
  out$summaries <- bm_bb_summary(out)
  out$mc_diagnostics <- bm_mc_diagnostics(out, level = level)

  if (isTRUE(warn) &&
      out$interval$defined_draws < out$interval$total_draws) {
    warning(structure(
      list(
        message = paste0(
          "Some Bayesian-bootstrap draws have undefined scale H; ",
          "inspect the typed interval status and defined-draw counts."
        ),
        call = NULL,
        defined_fraction =
          out$interval$defined_draws / out$interval$total_draws
      ),
      class = c("bm_bb_boundary_warning", "warning", "condition")
    ))
  }
  out
}

#' Deprecated posterior-oriented alias
#'
#' `bayes_mokken_h()` is retained temporarily for call compatibility. It
#' returns the same `bm_bb_h` object as `bb_mokken_h()` and does not compute
#' threshold probabilities. The threshold arguments are ignored.
#'
#' @inheritParams bb_mokken_h
#' @param threshold Deprecated and ignored.
#' @param item_threshold Deprecated and ignored.
#' @param pair_threshold Deprecated and ignored.
#'
#' @return A `bm_bb_h` object.
#'
#' @family bayesmokken-estimation
#' @seealso [bb_mokken_h()], which this forwards to.
#' @export
bayes_mokken_h <- function(data,
                           items = colnames(data),
                           score_map = NULL,
                           missing_codes = NULL,
                           row_ids = NULL,
                           missing = "complete_scale",
                           draws = 2000L,
                           seed = 1L,
                           batch_size = 250L,
                           workers = 1L,
                           threshold = 0.30,
                           item_threshold = 0.30,
                           pair_threshold = 0,
                           interval = c("calibrated", "equal_tail"),
                           level = 0.95,
                           zero_tol = 64 * .Machine$double.eps,
                           eta = sqrt(.Machine$double.eps),
                           warn = TRUE,
                           item_type = c("auto", "binary", "ordinal"),
                           supports = NULL,
                           cluster = NULL,
                           cluster_target = c("respondent", "equal_cluster"),
                           weight_law = c(
                             "auto", "iid_respondent",
                             "hierarchical_cluster"
                           )) {
  .bm_require_cluster_target(cluster, missing(cluster_target))
  .Deprecated(
    new = "bb_mokken_h",
    package = "bayesmokken",
    msg = paste(
      "`bayes_mokken_h()` is deprecated; use `bb_mokken_h()`.",
      "Legacy threshold arguments are ignored because the public method",
      "does not report posterior probabilities."
    )
  )
  bb_mokken_h(
    data = data,
    items = items,
    score_map = score_map,
    missing_codes = missing_codes,
    row_ids = row_ids,
    missing = missing,
    draws = draws,
    seed = seed,
    batch_size = batch_size,
    workers = workers,
    interval = interval,
    level = level,
    zero_tol = zero_tol,
    eta = eta,
    warn = warn,
    item_type = item_type,
    supports = supports,
    cluster = cluster,
    cluster_target = cluster_target,
    weight_law = weight_law
  )
}
