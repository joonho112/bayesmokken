.bm_validate_engine_controls <- function(draws, batch_size, workers) {
  integer_scalar <- function(x, lower) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
      x >= lower && x == as.integer(x)
  }
  if (!integer_scalar(draws, 2L)) {
    .bm_abort("`draws` must be an integer of at least 2.")
  }
  if (!integer_scalar(batch_size, 1L)) {
    .bm_abort("`batch_size` must be a positive integer.")
  }
  if (!integer_scalar(workers, 1L)) {
    .bm_abort("`workers` must be a positive integer.")
  }
  list(
    draws = as.integer(draws),
    batch_size = as.integer(batch_size),
    workers = as.integer(workers)
  )
}

.bm_state_matrix <- function(denominator, zero_tol, eta) {
  if (any(!is.finite(denominator))) {
    .bm_abort("Bayesian-bootstrap denominator matrix is nonfinite.",
              "bm_invariant_error")
  }
  if (any(denominator < -zero_tol)) {
    .bm_abort("Bayesian-bootstrap maximum-covariance denominator is negative.",
              "bm_invariant_error")
  }
  value <- denominator
  value[value < 0] <- 0
  status <- matrix(
    "positive", nrow(value), ncol(value),
    dimnames = dimnames(value)
  )
  status[value <= eta] <- "near_zero"
  status[value <= zero_tol] <- "zero"
  list(value = value, status = status)
}

.bm_evaluate_grouped_draw_batch <- function(patterns, weights, pair_index,
                                            incidence, zero_tol, eta) {
  marginal <- weights %*% patterns
  pair_pattern <- patterns[, pair_index[1L, ], drop = FALSE] *
    patterns[, pair_index[2L, ], drop = FALSE]
  expected_product <- weights %*% pair_pattern
  marginal_product <- marginal[, pair_index[1L, ], drop = FALSE] *
    marginal[, pair_index[2L, ], drop = FALSE]
  covariance <- expected_product - marginal_product
  denominator_raw <- pmin(
    marginal[, pair_index[1L, ], drop = FALSE],
    marginal[, pair_index[2L, ], drop = FALSE]
  ) - marginal_product
  pair_state <- .bm_state_matrix(denominator_raw, zero_tol, eta)
  pair_h <- covariance / pair_state$value
  pair_h[pair_state$status == "zero"] <- NA_real_

  item_covariance <- covariance %*% incidence
  item_denominator_raw <- pair_state$value %*% incidence
  item_state <- .bm_state_matrix(item_denominator_raw, zero_tol, eta)
  item_h <- item_covariance / item_state$value
  item_h[item_state$status == "zero"] <- NA_real_

  scale_covariance <- rowSums(covariance)
  scale_denominator_raw <- rowSums(pair_state$value)
  scale_state <- .bm_state_matrix(
    matrix(scale_denominator_raw, ncol = 1L),
    zero_tol, eta
  )
  scale_h <- scale_covariance / scale_state$value[, 1L]
  scale_h[scale_state$status[, 1L] == "zero"] <- NA_real_

  list(
    pair = pair_h,
    item = item_h,
    scale = scale_h,
    pair_status = pair_state$status,
    item_status = item_state$status,
    scale_status = scale_state$status[, 1L]
  )
}

bm_bb_respondent <- function(complete, empirical, draws, seed,
                             batch_size = 250L, workers = 1L,
                             zero_tol = 64 * .Machine$double.eps,
                             eta = sqrt(.Machine$double.eps)) {
  if (!inherits(complete, "bm_complete_scale") ||
      !inherits(empirical, "bm_empirical_h")) {
    .bm_abort("Internal BB engine received invalid deterministic objects.",
              "bm_internal_error")
  }
  controls <- .bm_validate_engine_controls(draws, batch_size, workers)
  grouping <- bm_group_patterns(complete$data)
  grouped_draws <- bm_grouped_bb_weights(
    grouping,
    draws = controls$draws,
    seed = seed
  )

  pair_labels <- .bm_pair_key(
    empirical$pair$item_i,
    empirical$pair$item_j
  )
  pair_h <- matrix(
    NA_real_, controls$draws, length(pair_labels),
    dimnames = list(NULL, pair_labels)
  )
  item_h <- matrix(
    NA_real_, controls$draws, length(complete$item_names),
    dimnames = list(NULL, complete$item_names)
  )
  scale_h <- rep(NA_real_, controls$draws)
  pair_status <- matrix(
    NA_character_, controls$draws, length(pair_labels),
    dimnames = list(NULL, pair_labels)
  )
  item_status <- matrix(
    NA_character_, controls$draws, length(complete$item_names),
    dimnames = list(NULL, complete$item_names)
  )
  scale_status <- rep(NA_character_, controls$draws)

  pair_index <- utils::combn(seq_len(ncol(grouping$patterns)), 2L)
  incidence <- matrix(
    0, nrow = ncol(pair_index), ncol = ncol(grouping$patterns)
  )
  for (k in seq_len(ncol(pair_index))) {
    incidence[k, pair_index[, k]] <- 1
  }
  starts <- seq.int(1L, controls$draws, by = controls$batch_size)
  for (start in starts) {
    finish <- min(start + controls$batch_size - 1L, controls$draws)
    evaluated <- .bm_evaluate_grouped_draw_batch(
      grouping$patterns,
      grouped_draws$weights[start:finish, , drop = FALSE],
      pair_index,
      incidence,
      zero_tol,
      eta
    )
    pair_h[start:finish, ] <- evaluated$pair
    item_h[start:finish, ] <- evaluated$item
    scale_h[start:finish] <- evaluated$scale
    pair_status[start:finish, ] <- evaluated$pair_status
    item_status[start:finish, ] <- evaluated$item_status
    scale_status[start:finish] <- evaluated$scale_status
  }

  new_bm_bb_h(
    empirical = empirical,
    draws = list(
      pair = pair_h,
      item = item_h,
      scale = scale_h,
      pair_status = pair_status,
      item_status = item_status,
      scale_status = scale_status,
      draw_id = seq_len(controls$draws)
    ),
    grouping = grouping,
    rng = list(
      seed = grouped_draws$seed,
      rng_kind = grouped_draws$rng_kind,
      generator = grouped_draws$generator,
      stream_map_hash = grouped_draws$stream_map_hash
    ),
    computation = list(
      batch_size = controls$batch_size,
      workers_requested = controls$workers,
      execution = "deterministic_serial_reference",
      kernel = "vectorized_grouped_binary_moments",
      dense_pattern_space = FALSE,
      replicated_response_matrices = FALSE
    )
  )
}
