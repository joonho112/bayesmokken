.bm_validate_cluster_calibration_scope <- function(supports, cluster_count) {
  if (all(lengths(supports) == 2L)) {
    stop(structure(
      list(
        message = paste0(
          "The calibrated hierarchical interval is not validated when every ",
          "selected item has a two-category support."
        ),
        call = NULL,
        validation_scope = "POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB"
      ),
      class = c(
        "bm_cluster_calibration_scope_error",
        "bm_validation_scope_error",
        "bm_input_error",
        "error",
        "condition"
      )
    ))
  }
  if (cluster_count < .bm_validated_cluster_min) {
    stop(structure(
      list(
        message = sprintf(
          paste0(
            "The calibrated hierarchical interval requires at least %d ",
            "complete-scale clusters; found %d."
          ),
          .bm_validated_cluster_min,
          cluster_count
        ),
        call = NULL,
        cluster_count = as.integer(cluster_count),
        validated_cluster_min = .bm_validated_cluster_min,
        validated_cluster_max = .bm_validated_cluster_max
      ),
      class = c(
        "bm_cluster_count_below_validation_range_error",
        "bm_validation_range_error",
        "bm_input_error",
        "error",
        "condition"
      )
    ))
  }
  invisible(NULL)
}

.bm_warn_cluster_count_extrapolation <- function(cluster_count) {
  warning(structure(
    list(
      message = sprintf(
        paste0(
          "The calibrated hierarchical interval extrapolates beyond the ",
          "confirmed cluster-count range %d-%d (found %d clusters)."
        ),
        .bm_validated_cluster_min,
        .bm_validated_cluster_max,
        cluster_count
      ),
      call = NULL,
      cluster_count = as.integer(cluster_count),
      validation_status = "EXTRAPOLATED_CLUSTER_COUNT",
      validated_cluster_min = .bm_validated_cluster_min,
      validated_cluster_max = .bm_validated_cluster_max
    ),
    class = c(
      "bm_cluster_count_extrapolation_warning",
      "bm_validation_range_warning",
      "warning",
      "condition"
    )
  ))
}

.bm_bb_mokken_h_ordinal <- function(
    data, items, score_map, missing_codes, row_ids, missing,
    draws, seed, batch_size, workers, interval, level,
    zero_tol, eta, warn, supports, cluster, cluster_target) {
  empirical <- mokken_h(
    data = data,
    items = items,
    score_map = score_map,
    missing_codes = missing_codes,
    weights = NULL,
    row_ids = row_ids,
    missing = missing,
    zero_tol = zero_tol,
    eta = eta,
    warn = warn,
    item_type = "ordinal",
    supports = supports,
    cluster = cluster,
    cluster_target = cluster_target,
    weight_law = "hierarchical_cluster"
  )

  selected <- data[, items, drop = FALSE]
  selected_maps <- if (is.null(score_map)) NULL else
    score_map[intersect(names(score_map), items)]
  selected_missing <- if (is.null(missing_codes)) NULL else
    missing_codes[intersect(names(missing_codes), items)]
  selected_supports <- if (is.null(supports)) NULL else
    supports[intersect(names(supports), items)]
  validated <- bm_validate_ordinal_data(
    selected,
    score_map = selected_maps,
    missing_codes = selected_missing,
    supports = selected_supports
  )
  complete <- bm_complete_scale(
    validated,
    row_ids = row_ids,
    missing = missing
  )
  cluster_info <- bm_complete_cluster(cluster, complete)
  complete$analysis_input_hash <- .bm_analysis_input_hash(
    complete,
    normalized_weights = bm_cluster_point_weights(cluster_info, cluster_target),
    cluster_info = cluster_info,
    cluster_target = cluster_target
  )
  if (identical(interval, "calibrated")) {
    .bm_validate_cluster_calibration_scope(
      validated$supports, cluster_info$G
    )
  }
  if (!identical(
        complete$analysis_input_hash,
        empirical$provenance$analysis_input_hash
      ) ||
      !identical(complete$row_hash, empirical$provenance$row_hash) ||
      !identical(
        complete$score_map_hash,
        empirical$provenance$score_map_hash
      ) ||
      !identical(complete$support_hash, empirical$provenance$support_hash) ||
      !identical(
        bm_cluster_hash(cluster_info),
        empirical$provenance$cluster_id_hash
      )) {
    .bm_abort(
      "Deterministic and cluster BB preprocessing hashes disagree.",
      "bm_invariant_error"
    )
  }

  out <- bm_bb_cluster_ordinal(
    complete = complete,
    cluster_info = cluster_info,
    empirical = empirical,
    draws = draws,
    seed = seed,
    batch_size = batch_size,
    workers = workers,
    zero_tol = zero_tol,
    eta = eta
  )
  out$interval <- .bm_cluster_interval_from_draws(
    bb_draws = out$draws$scale,
    point_estimate = empirical$scale$H,
    n_complete = complete$n_complete,
    cluster_count = cluster_info$G,
    cluster_target = cluster_target,
    interval = interval,
    level = level
  )
  out$summaries <- bm_bb_summary(out)
  out$mc_diagnostics <- bm_mc_diagnostics(out, level = level)

  if (isTRUE(warn) &&
      identical(interval, "calibrated") &&
      cluster_info$G > .bm_validated_cluster_max) {
    .bm_warn_cluster_count_extrapolation(cluster_info$G)
  }

  if (isTRUE(warn) &&
      out$interval$defined_draws < out$interval$total_draws) {
    warning(structure(
      list(
        message = paste0(
          "Some hierarchical Bayesian-bootstrap draws have undefined ",
          "scale H; inspect the typed interval status and defined-draw counts."
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
