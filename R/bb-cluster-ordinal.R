bm_bb_cluster_ordinal <- function(
    complete, cluster_info, empirical, draws, seed,
    batch_size = 250L, workers = 1L,
    zero_tol = 64 * .Machine$double.eps,
    eta = sqrt(.Machine$double.eps),
    weight_law = c("complete_record", "hierarchical_cluster")) {
  weight_law <- match.arg(weight_law)
  if (!inherits(complete, "bm_complete_scale") ||
      !identical(complete$item_type, "ordinal") ||
      !inherits(empirical, "bm_empirical_h")) {
    .bm_abort(
      "Internal cluster BB engine received invalid ordinal objects.",
      "bm_internal_error"
    )
  }
  controls <- .bm_validate_engine_controls(draws, batch_size, workers)
  cluster_target <- empirical$estimand$cluster_target
  weight_draws <- if (identical(weight_law, "complete_record")) {
    bm_cluster_crbb_weights(
      cluster_info,
      draws = controls$draws,
      seed = seed,
      target = cluster_target
    )
  } else {
    bm_cluster_hbb_weights(
      cluster_info,
      draws = controls$draws,
      seed = seed,
      target = cluster_target
    )
  }

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
  scale_denominator <- rep(NA_real_, controls$draws)
  minimum_pair_denominator <- rep(NA_real_, controls$draws)

  pair_index <- utils::combn(seq_len(ncol(complete$data)), 2L)
  incidence <- matrix(
    0, nrow = ncol(pair_index), ncol = ncol(complete$data)
  )
  for (k in seq_len(ncol(pair_index))) {
    incidence[k, pair_index[, k]] <- 1
  }
  starts <- seq.int(1L, controls$draws, by = controls$batch_size)
  for (start in starts) {
    finish <- min(start + controls$batch_size - 1L, controls$draws)
    evaluated <- .bm_evaluate_ordinal_draw_batch(
      x = complete$data,
      weights = t(weight_draws$weights[, start:finish, drop = FALSE]),
      supports = complete$supports,
      pair_index = pair_index,
      incidence = incidence,
      zero_tol = zero_tol,
      eta = eta
    )
    pair_h[start:finish, ] <- evaluated$pair
    item_h[start:finish, ] <- evaluated$item
    scale_h[start:finish] <- evaluated$scale
    pair_status[start:finish, ] <- evaluated$pair_status
    item_status[start:finish, ] <- evaluated$item_status
    scale_status[start:finish] <- evaluated$scale_status
    scale_denominator[start:finish] <- evaluated$scale_denominator
    minimum_pair_denominator[start:finish] <- apply(
      evaluated$pair_denominator, 1L, min
    )
  }

  if (identical(weight_law, "complete_record")) {
    # The complete-record procedure types a draw undefined when the scale
    # denominator or any pair denominator is at most 1e-12, a stricter rule
    # than the package's zero tolerance. The item and pair draws keep the
    # package statuses; only the scale draws feed the interval.
    undefined <- !(scale_denominator > .bm_crbb_zero_tolerance &
                     minimum_pair_denominator > .bm_crbb_zero_tolerance)
    typed <- undefined & !is.na(scale_h)
    scale_h[undefined] <- NA_real_
    scale_status[typed] <- "typed_undefined"
  }

  new_bm_cluster_bb_h(
    empirical = empirical,
    draws = list(
      pair = pair_h,
      item = item_h,
      scale = scale_h,
      pair_status = pair_status,
      item_status = item_status,
      scale_status = scale_status,
      draw_id = weight_draws$draw_id,
      scale_denominator = scale_denominator,
      minimum_pair_denominator = minimum_pair_denominator
    ),
    cluster_info = cluster_info,
    weight_draws = weight_draws,
    computation = list(
      batch_size = controls$batch_size,
      workers_requested = controls$workers,
      execution = "deterministic_serial_reference",
      kernel = "vectorized_finite_support_ordinal_moments",
      dense_pattern_space = FALSE,
      replicated_response_matrices = FALSE
    ),
    weight_law = weight_law
  )
}
