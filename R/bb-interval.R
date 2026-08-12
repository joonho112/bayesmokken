.bm_interval_methods <- function() {
  c("calibrated", "equal_tail")
}

.bm_match_interval <- function(interval) {
  if (!is.character(interval) || !length(interval) || anyNA(interval)) {
    .bm_abort(
      "`interval` must be one of \"calibrated\" or \"equal_tail\"."
    )
  }
  match.arg(interval, .bm_interval_methods())
}

.bm_validate_interval_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      !isTRUE(all.equal(as.numeric(level), 0.95, tolerance = 0))) {
    .bm_abort(
      "`level` must be exactly 0.95 for the frozen Paper A interval.",
      hint = .bm_hints$level_fixed
    )
  }
  0.95
}

.bm_interval_ids <- function(interval) {
  if (identical(interval, "calibrated")) {
    return(list(
      method_id = "P7-FS-WIDTH-RAW-G025-v1",
      base_method_id = "CMP-RAW-BB-EQUAL-TAIL-v1",
      gamma = 0.25
    ))
  }
  list(
    method_id = "CMP-RAW-BB-EQUAL-TAIL-v1",
    base_method_id = "CMP-RAW-BB-EQUAL-TAIL-v1",
    gamma = 0
  )
}

.bm_new_interval <- function(ids, status, level, point_estimate,
                             lower, upper, raw_lower, raw_upper,
                             midpoint, width_multiplier, n_complete,
                             defined_draws, total_draws,
                             minimum_defined_draws) {
  interpretation <- paste(
    "Bayesian-bootstrap-based confidence interval for repeated-sampling",
    "uncertainty; not a posterior credible interval or posterior probability."
  )
  structure(
    list(
      method_id = ids$method_id,
      base_method_id = ids$base_method_id,
      status = status,
      level = level,
      point_estimate = point_estimate,
      lower = lower,
      upper = upper,
      raw_lower = raw_lower,
      raw_upper = raw_upper,
      midpoint = midpoint,
      gamma = ids$gamma,
      width_multiplier = width_multiplier,
      n_complete = as.integer(n_complete),
      defined_draws = as.integer(defined_draws),
      total_draws = as.integer(total_draws),
      minimum_defined_draws = as.integer(minimum_defined_draws),
      quantile_type = 8L,
      endpoint_clipping = "NONE",
      per_replication_method_cascade = "FORBIDDEN",
      draw_filter_policy =
        "FINITE_ONLY_WITH_EXPLICIT_DEFINED_COUNTS_AND_STATUS",
      interpretation = interpretation
    ),
    class = "bm_h_interval"
  )
}

.bm_interval_failure <- function(ids, status, level, point_estimate,
                                 n_complete, defined_draws, total_draws,
                                 minimum_defined_draws) {
  .bm_new_interval(
    ids = ids,
    status = status,
    level = level,
    point_estimate = point_estimate,
    lower = NA_real_,
    upper = NA_real_,
    raw_lower = NA_real_,
    raw_upper = NA_real_,
    midpoint = NA_real_,
    width_multiplier = if (identical(ids$gamma, 0)) {
      1
    } else {
      1 + ids$gamma / sqrt(n_complete)
    },
    n_complete = n_complete,
    defined_draws = defined_draws,
    total_draws = total_draws,
    minimum_defined_draws = minimum_defined_draws
  )
}

.bm_interval_from_draws <- function(bb_draws, point_estimate, n_complete,
                                    interval = "calibrated",
                                    level = 0.95) {
  interval <- .bm_match_interval(interval)
  level <- .bm_validate_interval_level(level)
  ids <- .bm_interval_ids(interval)

  if (!is.numeric(bb_draws) || is.matrix(bb_draws) ||
      is.data.frame(bb_draws)) {
    .bm_abort("`bb_draws` must be one numeric vector.")
  }
  if (!is.numeric(n_complete) || length(n_complete) != 1L ||
      !is.finite(n_complete) || n_complete < 2 ||
      n_complete != as.integer(n_complete)) {
    .bm_abort("`n_complete` must be an integer of at least 2.")
  }

  total_draws <- length(bb_draws)
  defined <- is.finite(bb_draws)
  defined_draws <- sum(defined)
  minimum_defined_draws <- max(100L, ceiling(0.90 * total_draws))

  if (!is.numeric(point_estimate) || length(point_estimate) != 1L ||
      !is.finite(point_estimate)) {
    return(.bm_interval_failure(
      ids, "UNDEFINED_POINT_ESTIMATE", level,
      suppressWarnings(as.numeric(point_estimate)[1L]),
      n_complete, defined_draws, total_draws, minimum_defined_draws
    ))
  }
  if (defined_draws < minimum_defined_draws) {
    return(.bm_interval_failure(
      ids, "INSUFFICIENT_DEFINED_BB_DRAWS", level, point_estimate,
      n_complete, defined_draws, total_draws, minimum_defined_draws
    ))
  }

  probabilities <- c((1 - level) / 2, (1 + level) / 2)
  endpoint <- stats::quantile(
    bb_draws[defined],
    probs = probabilities,
    names = FALSE,
    type = 8L,
    na.rm = FALSE
  )
  raw_lower <- endpoint[[1L]]
  raw_upper <- endpoint[[2L]]
  if (!is.finite(raw_lower) || !is.finite(raw_upper) ||
      raw_lower > raw_upper) {
    return(.bm_interval_failure(
      ids, "NONFINITE_OR_REVERSED_BASE_INTERVAL", level, point_estimate,
      n_complete, defined_draws, total_draws, minimum_defined_draws
    ))
  }

  midpoint <- (raw_lower + raw_upper) / 2
  raw_halfwidth <- (raw_upper - raw_lower) / 2
  width_multiplier <- if (identical(interval, "calibrated")) {
    1 + ids$gamma / sqrt(n_complete)
  } else {
    1
  }
  lower <- midpoint - width_multiplier * raw_halfwidth
  upper <- midpoint + width_multiplier * raw_halfwidth
  status <- if (defined_draws == total_draws) {
    "OK"
  } else {
    "OK_WITH_UNDEFINED_DRAWS"
  }

  .bm_new_interval(
    ids = ids,
    status = status,
    level = level,
    point_estimate = point_estimate,
    lower = lower,
    upper = upper,
    raw_lower = raw_lower,
    raw_upper = raw_upper,
    midpoint = midpoint,
    width_multiplier = width_multiplier,
    n_complete = n_complete,
    defined_draws = defined_draws,
    total_draws = total_draws,
    minimum_defined_draws = minimum_defined_draws
  )
}

#' @export
print.bm_h_interval <- function(
    x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("<bm_h_interval>\n")
  cat("  method: ", x$method_id, "\n", sep = "")
  cat("  status: ", x$status, "\n", sep = "")
  cat(
    "  H [", format(100 * x$level, digits = digits), "% CI]: ",
    format(x$point_estimate, digits = digits), " [",
    format(x$lower, digits = digits), ", ",
    format(x$upper, digits = digits), "]\n",
    sep = ""
  )
  cat(
    "  defined BB draws: ", x$defined_draws, "/",
    x$total_draws, "\n",
    sep = ""
  )
  if (!is.null(x$cluster_count)) {
    cat(
      "  clusters/target: ", x$cluster_count, "/",
      x$cluster_target, "\n",
      sep = ""
    )
  }
  invisible(x)
}
