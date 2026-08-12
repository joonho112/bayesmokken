.bm_mokken_h_ordinal <- function(
    selected, score_map, missing_codes, supports,
    weights, row_ids, missing, cluster, cluster_target,
    zero_tol, eta, warn) {
  validated <- bm_validate_ordinal_data(
    selected,
    score_map = score_map,
    missing_codes = missing_codes,
    supports = supports
  )
  complete <- bm_complete_scale(
    validated,
    row_ids = row_ids,
    missing = missing
  )

  cluster_info <- NULL
  if (!is.null(cluster)) {
    if (!is.null(weights)) {
      .bm_abort("`weights` and `cluster` cannot be supplied together.")
    }
    cluster_info <- bm_complete_cluster(cluster, complete)
    point_weights <- bm_cluster_point_weights(cluster_info, cluster_target)
  } else {
    if (!is.null(weights)) {
      if (!is.numeric(weights) || length(weights) != complete$n_total ||
          anyNA(weights) || any(!is.finite(weights)) || any(weights < 0)) {
        .bm_abort(
          "`weights` must be finite, nonnegative, and match original input rows."
        )
      }
      weights <- weights[complete$included]
    }
    point_weights <- weights
  }

  moments <- bm_ordinal_moments(
    complete$data,
    weights = point_weights,
    supports = complete$supports
  )
  complete$analysis_input_hash <- .bm_analysis_input_hash(
    complete,
    normalized_weights = point_weights,
    cluster_info = cluster_info,
    cluster_target = if (is.null(cluster_info)) NULL else cluster_target
  )
  pair <- bm_ordinal_pair_state(
    moments,
    supports = complete$supports,
    zero_tol = zero_tol,
    eta = eta
  )
  aggregate <- bm_h_aggregate(
    pair,
    complete$item_names,
    zero_tol = zero_tol,
    eta = eta
  )
  knot <- bm_ordinal_knot_diagnostics(
    complete$data,
    weights = moments$weights,
    supports = complete$supports
  )
  out <- new_bm_ordinal_empirical_h(
    pair = pair,
    item = aggregate$item,
    scale = aggregate$scale,
    complete = complete,
    moments = moments,
    zero_tol = zero_tol,
    eta = eta,
    cluster_info = cluster_info,
    cluster_target = cluster_target,
    knot_diagnostics = knot
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
