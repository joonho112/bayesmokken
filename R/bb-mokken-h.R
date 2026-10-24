#' Bayesian-bootstrap confidence interval for a fixed-scale Mokken coefficient
#'
#' Computes the scalability coefficient for a fixed item set and attaches a
#' confidence interval whose coverage has been checked in prespecified
#' simulations. Binary items take the respondent Bayesian bootstrap with a
#' frozen width correction; polytomous items answered by respondents nested
#' in clusters take the complete-record Bayesian bootstrap with the adjusted
#' interval of the clustered article's current version. Both constructions
#' are frozen.
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
#' 4. The binary path rejects `supports`, `cluster` and the cluster weight
#'    laws.
#' 5. Ordinal [mokken_h()] accepts the iid respondent law without clusters and
#'    a cluster-record law with clusters. Ordinal `bb_mokken_h()` additionally
#'    requires clusters, and `weight_law = "auto"` then resolves to
#'    `"complete_record"`.
#'
#' The supported lanes are summarized below. `required` means the argument
#' must occur in the call, even though the published formal default is retained
#' for call compatibility.
#'
#' | resolved `item_type` | `supports` | `cluster` | `cluster_target` | `weight_law` | result |
#' |----|----|----|----|----|----|
#' | binary | absent | absent | not used | `"auto"` or `"iid_respondent"` | point H; respondent BB interval |
#' | ordinal | required | absent | not used | `"auto"` or `"iid_respondent"` | point H only; interval call errors |
#' | ordinal | required | present | required | `"auto"` or `"complete_record"` | clustered point H; adjusted complete-record BB interval |
#' | ordinal | required | present | required | `"hierarchical_cluster"` | clustered point H; historical two-stage BB interval, with a typed warning |
#'
#' For `bb_mokken_h()`, `interval = "calibrated"` selects the validated
#' construction of the lane and `interval = "equal_tail"` selects its
#' uncorrected percentile comparator. On the default clustered lane
#' `"calibrated"` is the adjusted interval described below; the name is kept
#' for call compatibility and does not mean that a constant was tuned on data.
#' Supplying `cluster` also makes `"auto"` resolve to ordinal for 0/1 data,
#' but it does not create an observed two-point support: that support must
#' still be declared explicitly, and dichotomous items with a `cluster`
#' argument are refused on every route. See [bm_guardrails()].
#'
#' # The adjusted complete-record interval (default clustered lane)
#'
#' With the clusters as the independent sampling units, one Dirichlet vector
#' \eqn{A \sim \mathrm{Dirichlet}_G(1, \ldots, 1)} is drawn per draw over the
#' \eqn{G} complete cluster records and nothing is reweighted inside a record:
#' respondent \eqn{i} of cluster \eqn{g} receives weight \eqn{A_g/m_g} under the
#' equal-cluster target and \eqn{A_g/\sum_h m_h A_h} under the respondent
#' target, with the random denominator recomputed in every draw. The scale
#' coefficient is evaluated exactly for every draw; a draw whose scale
#' denominator or any pair denominator is at most \eqn{10^{-12}} is typed
#' undefined, and at least \eqn{\max\{20, \lceil 0.99 B \rceil\}} draws must be
#' defined. The type-8 quantiles \eqn{q_L, q_U} at .025 and .975 of the defined
#' draws are reflected about the point estimate \eqn{\hat H} and expanded by
#' \deqn{k_G = \sqrt{(G+1)/(G-1)}\; t_{G-1,.975} / z_{.975},}
#' giving \eqn{[\hat H - k_G (q_U - \hat H),\; \hat H - k_G (q_L - \hat H)]},
#' which is then intersected with \eqn{(-\infty, 1]}. The reflection is about
#' the point estimate, not the midpoint of the percentile interval, so the
#' reported interval need not contain \eqn{\hat H}. An empty intersection is
#' reported as an empty set with status `EMPTY_PARAMETER_INTERSECTION`, never
#' as \eqn{[1, 1]}; negative values are retained; a zero-width interval is
#' usable and flagged. The procedure is computable for \eqn{G \ge 2}, which is
#' a computational requirement and not a recommendation; its confirmation
#' covered cluster counts 22, 30, 35, 55, 80, 160 and 320, and the interval
#' records `CONFIRMED_CLUSTER_COUNT` inside 22--320 and
#' `OUTSIDE_CONFIRMED_CLUSTER_COUNT` elsewhere, without refusing or warning.
#' The draw stream reproduces the clustered article's archive: the same seed
#' gives the same draws, independently of `batch_size` and `workers`.
#'
#' # The historical two-stage lane
#'
#' `weight_law = "hierarchical_cluster"` runs the clustered procedure of
#' bayesmokken 0.9.0: a second Dirichlet reweighting within every cluster and
#' a midpoint-preserving shrinkage of the percentile interval by
#' \eqn{1 - 1.5/\sqrt{G}}, confirmed on 22--80 clusters, refused below 22 and
#' flagged above 80. The within-cluster stage adds first-order variance that no
#' multiplier tending to one can remove, which is why the lane was replaced.
#' It is retained so that results of the earlier version can be reproduced;
#' every random-weight call under it emits a warning of class
#' `bm_historical_method_warning`, independently of `warn`.
#'
#' # The five arguments most people need
#'
#' `data`, `draws`, `seed` for a binary scale; add `supports`, `cluster` and
#' `cluster_target` for clustered polytomous data. Everything else has a
#' frozen default that should be left alone unless you have a specific reason.
#'
#' @section Frozen constants:
#' The multipliers and the confidence level are not arguments. \eqn{\gamma}
#' = 0.25 for the binary correction was fixed on training data, validated on
#' disjoint held-out conditions, and locked; \eqn{k_G} for the clustered
#' adjustment is a prespecified function of the cluster count, selected
#' between two prespecified candidates before any confirmation outcome
#' existed. `level` must be exactly 0.95. Coverage at any other setting has
#' not been examined, so the package will not produce one. See
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
#' summarize them, but the interval constructions were validated for the scale
#' coefficient alone. Item- and pair-level intervals are reported as raw
#' quantiles and labeled `uncalibrated`.
#'
#' @inheritParams mokken_h
#' @param draws Number of Bayesian-bootstrap draws. The frozen default is
#'   2,000; the clustered article's application and confirmation used 9,999.
#'   Raising it reduces Monte Carlo error in the endpoints and costs time
#'   linearly; [bm_plot_mc_stability()] shows whether it is worth it. A
#'   minimum fraction must come out finite -- `max(100, ceiling(0.90 * draws))`
#'   on the binary path, `max(20, ceiling(0.99 * draws))` on the clustered one
#'   -- and a shortfall produces a typed interval failure rather than an
#'   interval computed from whatever survived.
#' @param seed Nonnegative integer seed for the L'Ecuyer-CMRG draw streams.
#'   Fractional values are rejected rather than truncated, because `1.1` and
#'   `1L` would otherwise silently give the same answer. Always set it. On the
#'   default clustered lane, the caller must use a built-in RNG and must not
#'   use Box-Muller normals, whose hidden cache cannot be restored. Select
#'   Inversion normals before calling. On this lane the stream is the clustered
#'   article's draw-major
#'   mapping, so the archive's seeds reproduce its draws.
#' @param batch_size Draws evaluated per processing batch. Affects memory and
#'   nothing else: changing it does not change which draws are taken.
#' @param workers Requested worker count. Recorded for provenance; the
#'   reference engine keeps the worker-invariant seed map and currently runs
#'   serially, so results do not depend on this.
#' @param interval `"calibrated"` for the validated construction of the lane,
#'   `"equal_tail"` for the uncorrected percentile comparator. The comparator
#'   exists so you can see what the construction did; it is not an alternative
#'   recommendation.
#' @param level Confidence level. Must be exactly `0.95`.
#'
#' @return An object of class `bm_bb_h`:
#'
#' \describe{
#'   \item{`empirical`}{The deterministic fit, as returned by [mokken_h()].}
#'   \item{`draws`}{`$scale` (vector), `$item` and `$pair` (matrices, one
#'     column per item or pair). Undefined draws are kept as `NA`. Clustered
#'     fits add `$scale_denominator` and `$minimum_pair_denominator`.}
#'   \item{`interval`}{A `bm_h_interval`: method identifier, status, point
#'     estimate, raw percentile and reported endpoints, the width multiplier,
#'     draw counts, and the policy fields recording that no method cascade is
#'     permitted. Clustered intervals also separate respondent `n_complete`
#'     from the cluster count and record the validation scope and status; on
#'     the default lane they carry the reflected endpoints before the
#'     intersection with \eqn{(-\infty, 1]}, the intersection and empty-set
#'     flags, the boundary counts, and the archive method identifier.}
#'   \item{`summaries`, `mc_diagnostics`}{Draw summaries and endpoint Monte
#'     Carlo diagnostics.}
#'   \item{`grouping`, `rng`, `sampling`, `computation`}{Provenance: pattern or
#'     cluster structure, seed, generator and stream mapping, weight law and
#'     target identifiers.}
#' }
#'
#' Reach for [bm_scale_h()], [bm_item_h()] and [bm_interval()] rather than
#' indexing this structure by hand.
#'
#' @references
#' Lee, J., & Wind, S. A. (2026). A calibrated Bayesian bootstrap confidence
#' interval for Mokken's scale-level scalability coefficient. Preprint.
#'
#' Lee, J. (2026). Cluster-aware uncertainty for polytomous Mokken
#' scalability: A cluster-level Bayesian bootstrap. Preprint.
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
#'                     weight_law = "complete_record",
#'                     draws = 500, seed = 1)
#' bm_scale_h(cfit)
#'
#' # See what the construction did
#' bm_plot_draws(fit)
#' }
#'
#' # Guard rails are typed errors, not silent fallbacks
#' try(bb_mokken_h(srs, draws = 200, seed = 1, level = 0.90))
#'
#' @family bayesmokken-estimation
#' @seealso [mokken_h()] for the coefficient without an interval,
#'   [bm_check_data()] to screen data first, [bm_scale_h()] to extract the
#'   result, [bm_plot_draws()] to see the construction, `?bayesmokken-intervals`
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
                          "auto", "iid_respondent", "complete_record",
                          "hierarchical_cluster"
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
      cluster_target = mode$cluster_target,
      weight_law = mode$weight_law
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
                             "auto", "iid_respondent", "complete_record",
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
