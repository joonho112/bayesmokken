# Locked Phase 15/16 cluster-width transform. Gamma is not a public argument
# and no candidate-search or per-dataset retuning path is present.

.bm_validated_cluster_min <- 22L
.bm_validated_cluster_max <- 80L

.bm_cluster_validation_status <- function(interval, cluster_count) {
  if (!identical(interval, "calibrated")) {
    return("NOT_APPLICABLE_EQUAL_TAIL")
  }
  if (cluster_count < .bm_validated_cluster_min) {
    return("BELOW_VALIDATED_CLUSTER_COUNT")
  }
  if (cluster_count > .bm_validated_cluster_max) {
    return("EXTRAPOLATED_CLUSTER_COUNT")
  }
  "CONFIRMED_CLUSTER_COUNT"
}

.bm_add_cluster_interval_metadata <- function(
    out, cluster_count, cluster_target, interval) {
  out$cluster_count <- as.integer(cluster_count)
  out$cluster_target <- cluster_target
  out$calibration_n <- as.integer(cluster_count)
  out$calibration_unit <- "clusters"
  out$validation_status <- .bm_cluster_validation_status(
    interval, cluster_count
  )
  out$validation_scope <- "POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB"
  out$validated_cluster_min <- .bm_validated_cluster_min
  out$validated_cluster_max <- .bm_validated_cluster_max
  out$calibration_rate <- "1 + gamma_cluster / sqrt(G)"
  out$minimum_defined_fraction <- 0.99
  out
}

.bm_cluster_interval_ids <- function(interval, cluster_target) {
  suffix <- if (identical(cluster_target, "respondent")) {
    "RESP"
  } else {
    "CLUSTER"
  }
  base_method_id <- if (identical(cluster_target, "respondent")) {
    "V4-PRI-TWO-STAGE-HBB-RESP-v1"
  } else {
    "V4-SENS-TWO-STAGE-HBB-CLUSTER-v1"
  }
  if (identical(interval, "calibrated")) {
    list(
      method_id = paste0("V4-P15-HBB-", suffix, "-GN150-v1"),
      base_method_id = base_method_id,
      gamma = -1.5
    )
  } else {
    list(
      method_id = paste0("V4-CMP-RAW-HBB-EQUAL-TAIL-", suffix, "-v1"),
      base_method_id = base_method_id,
      gamma = 0
    )
  }
}

.bm_cluster_interval_from_draws <- function(
    bb_draws, point_estimate, n_complete, cluster_count, cluster_target,
    interval = "calibrated", level = 0.95) {
  interval <- .bm_match_interval(interval)
  level <- .bm_validate_interval_level(level)
  ids <- .bm_cluster_interval_ids(interval, cluster_target)
  if (!is.numeric(bb_draws) || is.matrix(bb_draws) ||
      is.data.frame(bb_draws)) {
    .bm_abort("`bb_draws` must be one numeric vector.")
  }
  if (!is.numeric(cluster_count) || length(cluster_count) != 1L ||
      !is.finite(cluster_count) || cluster_count < 2 ||
      cluster_count != as.integer(cluster_count)) {
    .bm_abort("`cluster_count` must be an integer of at least 2.")
  }
  if (!is.numeric(n_complete) || length(n_complete) != 1L ||
      !is.finite(n_complete) || n_complete < 2 ||
      n_complete != as.integer(n_complete)) {
    .bm_abort("`n_complete` must be an integer of at least 2.")
  }

  total_draws <- length(bb_draws)
  defined <- is.finite(bb_draws)
  defined_draws <- sum(defined)
  minimum_defined_draws <- max(20L, ceiling(0.99 * total_draws))
  multiplier <- if (identical(interval, "calibrated")) {
    1 + ids$gamma / sqrt(cluster_count)
  } else {
    1
  }
  if (!is.finite(multiplier) || multiplier <= 0) {
    .bm_abort(
      "The locked cluster width multiplier is nonpositive for this cluster count."
    )
  }

  failure <- function(status) {
    out <- .bm_new_interval(
      ids = ids,
      status = status,
      level = level,
      point_estimate = suppressWarnings(as.numeric(point_estimate)[1L]),
      lower = NA_real_,
      upper = NA_real_,
      raw_lower = NA_real_,
      raw_upper = NA_real_,
      midpoint = NA_real_,
      width_multiplier = multiplier,
      n_complete = n_complete,
      defined_draws = defined_draws,
      total_draws = total_draws,
      minimum_defined_draws = minimum_defined_draws
    )
    .bm_add_cluster_interval_metadata(
      out, cluster_count, cluster_target, interval
    )
  }
  if (!is.numeric(point_estimate) || length(point_estimate) != 1L ||
      !is.finite(point_estimate)) {
    return(failure("UNDEFINED_POINT_ESTIMATE"))
  }
  if (defined_draws < minimum_defined_draws) {
    return(failure("INSUFFICIENT_DEFINED_BB_DRAWS"))
  }

  probability <- c((1 - level) / 2, (1 + level) / 2)
  endpoint <- stats::quantile(
    bb_draws[defined],
    probs = probability,
    names = FALSE,
    type = 8L,
    na.rm = FALSE
  )
  raw_lower <- endpoint[[1L]]
  raw_upper <- endpoint[[2L]]
  if (!is.finite(raw_lower) || !is.finite(raw_upper) ||
      raw_lower > raw_upper) {
    return(failure("NONFINITE_OR_REVERSED_BASE_INTERVAL"))
  }

  midpoint <- (raw_lower + raw_upper) / 2
  raw_halfwidth <- (raw_upper - raw_lower) / 2
  out <- .bm_new_interval(
    ids = ids,
    status = if (defined_draws == total_draws) {
      "OK"
    } else {
      "OK_WITH_UNDEFINED_DRAWS"
    },
    level = level,
    point_estimate = point_estimate,
    lower = midpoint - multiplier * raw_halfwidth,
    upper = midpoint + multiplier * raw_halfwidth,
    raw_lower = raw_lower,
    raw_upper = raw_upper,
    midpoint = midpoint,
    width_multiplier = multiplier,
    n_complete = n_complete,
    defined_draws = defined_draws,
    total_draws = total_draws,
    minimum_defined_draws = minimum_defined_draws
  )
  .bm_add_cluster_interval_metadata(
    out, cluster_count, cluster_target, interval
  )
}
