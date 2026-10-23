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
  out$construction <- "midpoint_halfwidth_scaling"
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


# =============================================================================
# Adjusted complete-record interval (clustered article, current version;
# protocol C2, archive method `one_stage_basic_G_adjusted`).
#
# Given the scale draws H* of the complete-record law and the point estimate
# H, take the type-8 quantiles q_L and q_U of the defined draws, reflect them
# about H, expand the reflected errors by
#
#     k_G = sqrt((G + 1) / (G - 1)) * t_{G-1, .975} / z_{.975},
#
# and intersect the result with (-Inf, 1]:
#
#     reflected_lower = H - k_G (q_U - H),   reflected_upper = H - k_G (q_L - H),
#     [lower, upper] = [reflected_lower, reflected_upper] intersect (-Inf, 1].
#
# The center of the reflection is the point estimate, not the midpoint of the
# percentile interval, so the reported interval need not contain H. The width
# before the intersection is exactly k_G times the percentile width, which is
# why `width_multiplier` carries k_G. Nothing is retuned per dataset: k_G is a
# prespecified function of the cluster count, the level is fixed at 95%, and
# no alternative construction is substituted when this one is unavailable.
# =============================================================================

.bm_crbb_confirmed_cluster_min <- 22L
.bm_crbb_confirmed_cluster_max <- 320L
.bm_crbb_zero_tolerance <- 1e-12
.bm_crbb_multiplier_formula <-
  "sqrt((G + 1) / (G - 1)) * qt(0.975, G - 1) / qnorm(0.975)"
.bm_crbb_undefined_draw_rule <-
  "scale denominator or any pair maximum covariance <= 1e-12"

.bm_cluster_k_multiplier <- function(cluster_count) {
  sqrt((cluster_count + 1) / (cluster_count - 1)) *
    stats::qt(0.975, df = cluster_count - 1) / stats::qnorm(0.975)
}

.bm_crbb_validation_status <- function(interval, cluster_count) {
  if (!identical(interval, "calibrated")) {
    return("NOT_APPLICABLE_EQUAL_TAIL")
  }
  if (cluster_count < .bm_crbb_confirmed_cluster_min ||
      cluster_count > .bm_crbb_confirmed_cluster_max) {
    return("OUTSIDE_CONFIRMED_CLUSTER_COUNT")
  }
  "CONFIRMED_CLUSTER_COUNT"
}

.bm_crbb_interval_ids <- function(interval, cluster_target) {
  suffix <- if (identical(cluster_target, "respondent")) {
    "RESP"
  } else {
    "CLUSTER"
  }
  base_method_id <- paste0("V5-CMP-CRBB-PERCENTILE-", suffix, "-v1")
  if (identical(interval, "calibrated")) {
    list(
      method_id = paste0("V5-C2-CRBB-ADJUSTED-", suffix, "-v1"),
      base_method_id = base_method_id,
      gamma = NA_real_,
      archive_method_id = "one_stage_basic_G_adjusted"
    )
  } else {
    list(
      method_id = base_method_id,
      base_method_id = base_method_id,
      gamma = NA_real_,
      archive_method_id = "one_stage_bb"
    )
  }
}

# The point estimate of the deterministic fit is undefined for the
# complete-record procedure under the same 1e-12 rule as its draws.
.bm_crbb_point_defined <- function(empirical) {
  isTRUE(is.finite(empirical$scale$H)) &&
    isTRUE(empirical$scale$cov_max_sum > .bm_crbb_zero_tolerance) &&
    all(empirical$pair$cov_max > .bm_crbb_zero_tolerance)
}

.bm_crbb_interval_from_draws <- function(
    bb_draws, point_estimate, n_complete, cluster_count, cluster_target,
    interval = "calibrated", level = 0.95, point_defined = TRUE) {
  interval <- .bm_match_interval(interval)
  level <- .bm_validate_interval_level(level)
  calibrated <- identical(interval, "calibrated")
  ids <- .bm_crbb_interval_ids(interval, cluster_target)
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
  values <- bb_draws[defined]
  minimum_defined_draws <- max(20L, ceiling(0.99 * total_draws))
  multiplier <- if (calibrated) .bm_cluster_k_multiplier(cluster_count) else 1
  point <- suppressWarnings(as.numeric(point_estimate)[1L])
  point_usable <- is.numeric(point_estimate) && length(point_estimate) == 1L &&
    is.finite(point) && isTRUE(point_defined)
  flags <- list(
    point_boundary = point_usable && point == 1,
    point_bound_violation = point_usable && point > 1,
    boundary_draws = as.integer(sum(values == 1)),
    bound_violation_draws = as.integer(sum(values > 1))
  )

  build <- function(status, lower = NA_real_, upper = NA_real_,
                    raw_lower = NA_real_, raw_upper = NA_real_,
                    reflected_lower = NA_real_, reflected_upper = NA_real_,
                    upper_intersection = NA, set_empty = NA,
                    zero_width = NA) {
    out <- .bm_new_interval(
      ids = ids,
      status = status,
      level = level,
      point_estimate = point,
      lower = lower,
      upper = upper,
      raw_lower = raw_lower,
      raw_upper = raw_upper,
      midpoint = if (is.finite(raw_lower) && is.finite(raw_upper)) {
        (raw_lower + raw_upper) / 2
      } else {
        NA_real_
      },
      width_multiplier = multiplier,
      n_complete = n_complete,
      defined_draws = defined_draws,
      total_draws = total_draws,
      minimum_defined_draws = minimum_defined_draws
    )
    if (calibrated) {
      out$endpoint_clipping <- "UPPER_PARAMETER_BOUND_ONE"
    }
    .bm_add_crbb_interval_metadata(
      out,
      cluster_count = cluster_count,
      cluster_target = cluster_target,
      interval = interval,
      ids = ids,
      multiplier = multiplier,
      reflected_lower = reflected_lower,
      reflected_upper = reflected_upper,
      upper_intersection = upper_intersection,
      set_empty = set_empty,
      zero_width = zero_width,
      flags = flags
    )
  }

  if (!point_usable) {
    return(build("UNDEFINED_POINT_ESTIMATE"))
  }
  if (calibrated && point > 1) {
    return(build("POINT_UPPER_BOUND_VIOLATION"))
  }
  if (defined_draws < minimum_defined_draws) {
    return(build("INSUFFICIENT_DEFINED_BB_DRAWS"))
  }
  if (calibrated && flags$bound_violation_draws > 0L) {
    return(build("DRAW_UPPER_BOUND_VIOLATION"))
  }
  if (!is.finite(multiplier) || multiplier <= 0) {
    return(build("INVALID_ROOT_MULTIPLIER"))
  }

  # The level is fixed at 95%, and the protocol's quantile probabilities are
  # the literal constants .025 and .975. Deriving them as (1 - level) / 2
  # differs from the literal in the last bit and moves a type-8 quantile by
  # about 1e-16, which is enough to break bit-for-bit agreement with the
  # archive's arithmetic for the adjusted interval. The same literals are used
  # for the percentile comparator, so that its endpoints are exactly the raw
  # quantiles of the adjusted interval; the archive's own comparator derives
  # its probabilities from the level and can differ from this one by that
  # last bit.
  probability <- c(0.025, 0.975)
  endpoint <- stats::quantile(
    values,
    probs = probability,
    names = FALSE,
    type = 8L,
    na.rm = FALSE
  )
  raw_lower <- endpoint[[1L]]
  raw_upper <- endpoint[[2L]]
  if (!is.finite(raw_lower) || !is.finite(raw_upper) ||
      raw_lower > raw_upper) {
    return(build("NONFINITE_OR_REVERSED_BASE_INTERVAL",
                 raw_lower = raw_lower, raw_upper = raw_upper))
  }
  ok_status <- if (defined_draws == total_draws) {
    "OK"
  } else {
    "OK_WITH_UNDEFINED_DRAWS"
  }

  if (!calibrated) {
    return(build(
      ok_status,
      lower = raw_lower, upper = raw_upper,
      raw_lower = raw_lower, raw_upper = raw_upper,
      upper_intersection = FALSE, set_empty = FALSE,
      zero_width = (raw_upper - raw_lower) == 0
    ))
  }

  # Reflection of the error quantiles about the point estimate, then the
  # expansion. This is not midpoint-and-half-width scaling.
  reflected_lower <- point - multiplier * (raw_upper - point)
  reflected_upper <- point - multiplier * (raw_lower - point)
  if (!is.finite(reflected_lower) || !is.finite(reflected_upper)) {
    return(build("NONFINITE_REFLECTED_ENDPOINT",
                 raw_lower = raw_lower, raw_upper = raw_upper,
                 reflected_lower = reflected_lower,
                 reflected_upper = reflected_upper))
  }
  if (reflected_lower > reflected_upper) {
    return(build("REVERSED_REFLECTED_ENDPOINTS",
                 raw_lower = raw_lower, raw_upper = raw_upper,
                 reflected_lower = reflected_lower,
                 reflected_upper = reflected_upper))
  }
  upper_intersection <- reflected_upper > 1
  if (reflected_lower > 1) {
    # A computed empty confidence set: a noncoverage outcome, recorded as
    # such and never projected to [1, 1].
    return(build("EMPTY_PARAMETER_INTERSECTION",
                 raw_lower = raw_lower, raw_upper = raw_upper,
                 reflected_lower = reflected_lower,
                 reflected_upper = reflected_upper,
                 upper_intersection = upper_intersection,
                 set_empty = TRUE, zero_width = FALSE))
  }
  lower <- reflected_lower
  upper <- min(reflected_upper, 1)
  if (!is.finite(upper - lower)) {
    return(build("NONFINITE_INTERVAL_LENGTH",
                 raw_lower = raw_lower, raw_upper = raw_upper,
                 reflected_lower = reflected_lower,
                 reflected_upper = reflected_upper,
                 upper_intersection = upper_intersection,
                 set_empty = FALSE))
  }
  build(
    ok_status,
    lower = lower, upper = upper,
    raw_lower = raw_lower, raw_upper = raw_upper,
    reflected_lower = reflected_lower, reflected_upper = reflected_upper,
    upper_intersection = upper_intersection, set_empty = FALSE,
    zero_width = (upper - lower) == 0
  )
}

.bm_add_crbb_interval_metadata <- function(
    out, cluster_count, cluster_target, interval, ids, multiplier,
    reflected_lower, reflected_upper, upper_intersection, set_empty,
    zero_width, flags) {
  calibrated <- identical(interval, "calibrated")
  out$cluster_count <- as.integer(cluster_count)
  out$cluster_target <- cluster_target
  out$calibration_n <- as.integer(cluster_count)
  out$calibration_unit <- "clusters"
  out$validation_status <- .bm_crbb_validation_status(interval, cluster_count)
  out$validation_scope <- "POLYTOMOUS_CLUSTERED_COMPLETE_RECORD_BB"
  out$validated_cluster_min <- .bm_crbb_confirmed_cluster_min
  out$validated_cluster_max <- .bm_crbb_confirmed_cluster_max
  out$calibration_rate <- if (calibrated) {
    .bm_crbb_multiplier_formula
  } else {
    "1 (percentile comparator)"
  }
  out$minimum_defined_fraction <- 0.99
  out$construction <- if (calibrated) {
    "reflected_error_expansion_about_point"
  } else {
    "percentile"
  }
  out$root_multiplier <- multiplier
  out$multiplier_formula <- if (calibrated) {
    .bm_crbb_multiplier_formula
  } else {
    "1"
  }
  out$reflected_lower <- reflected_lower
  out$reflected_upper <- reflected_upper
  out$parameter_space <- if (calibrated) "(-Inf, 1]" else "unrestricted"
  out$upper_intersection <- upper_intersection
  out$set_empty <- set_empty
  out$zero_width <- zero_width
  out$point_boundary <- flags$point_boundary
  out$boundary_draws <- flags$boundary_draws
  out$point_bound_violation <- flags$point_bound_violation
  out$bound_violation_draws <- flags$bound_violation_draws
  out$archive_method_id <- ids$archive_method_id
  out$protocol_id <- if (calibrated) "C2" else "C2-comparator"
  out$zero_tolerance <- .bm_crbb_zero_tolerance
  out$undefined_draw_rule <- .bm_crbb_undefined_draw_rule
  out
}
