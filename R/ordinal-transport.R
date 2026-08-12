# Finite-support ordinal maximum covariance and Mokken H kernels.
#
# Production port from the G18-A-reviewed Paper A v4 research engine
# (R/core/ordinal-h.R). The denominator is the standard finite-support
# comonotone/common-quantile maximum covariance; no new theorem is claimed.

bm_ordinal_max_covariance_transport <- function(
    x_scores, x_mass, y_scores, y_mass, mass_tol = 1e-12) {
  x_scores <- as.numeric(x_scores)
  y_scores <- as.numeric(y_scores)
  x_mass <- as.numeric(x_mass)
  y_mass <- as.numeric(y_mass)
  valid <- length(x_scores) == length(x_mass) &&
    length(y_scores) == length(y_mass) &&
    length(x_scores) >= 2L && length(y_scores) >= 2L &&
    all(is.finite(c(x_scores, y_scores, x_mass, y_mass))) &&
    all(diff(x_scores) > 0) && all(diff(y_scores) > 0) &&
    all(x_mass >= 0) && all(y_mass >= 0) &&
    abs(sum(x_mass) - 1) <= mass_tol &&
    abs(sum(y_mass) - 1) <= mass_tol
  if (!valid) {
    .bm_abort(
      "Invalid finite-support transport input.",
      "bm_internal_error"
    )
  }

  x_mass <- x_mass / sum(x_mass)
  y_mass <- y_mass / sum(y_mass)
  remaining_x <- x_mass
  remaining_y <- y_mass
  i <- 1L
  j <- 1L
  maximum_product <- 0
  moved_total <- 0
  # Two-pointer comonotone transport. Pointers advance only on exactly
  # exhausted mass: a valid positive mass, however small, is shipped, never
  # skipped (`mass_tol` governs the input sum check and the conservation
  # assertion below, not a per-point drop threshold). The exhausted side is
  # set to exact zero so floating-point residue cannot stall or leak mass;
  # each step exhausts at least one support point, so the loop performs at
  # most K + L - 1 positive allocations.
  while (i <= length(x_scores) && j <= length(y_scores)) {
    if (remaining_x[i] == 0) {
      i <- i + 1L
      next
    }
    if (remaining_y[j] == 0) {
      j <- j + 1L
      next
    }
    moved <- min(remaining_x[i], remaining_y[j])
    maximum_product <- maximum_product +
      moved * x_scores[i] * y_scores[j]
    moved_total <- moved_total + moved
    exhaust_x <- remaining_x[i] <= remaining_y[j]
    exhaust_y <- remaining_y[j] <= remaining_x[i]
    remaining_x[i] <- if (exhaust_x) 0 else remaining_x[i] - moved
    remaining_y[j] <- if (exhaust_y) 0 else remaining_y[j] - moved
    if (exhaust_x) {
      i <- i + 1L
    }
    if (exhaust_y) {
      j <- j + 1L
    }
  }
  if (abs(moved_total - 1) > mass_tol) {
    .bm_abort(
      "Transport mass conservation failed.",
      "bm_internal_error"
    )
  }

  mean_x <- sum(x_scores * x_mass)
  mean_y <- sum(y_scores * y_mass)
  list(
    maximum_product = maximum_product,
    maximum_covariance = maximum_product - mean_x * mean_y,
    mean_x = mean_x,
    mean_y = mean_y
  )
}

bm_ordinal_moments <- function(x, weights = NULL, supports) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 1L ||
      ncol(x) < 2L || anyNA(x)) {
    .bm_abort(
      "Internal ordinal moment kernel requires a complete numeric matrix.",
      "bm_internal_error"
    )
  }
  weight_info <- .bm_normalize_weights(weights, nrow(x))
  w <- weight_info$normalized
  means <- as.numeric(crossprod(w, x))
  names(means) <- colnames(x)
  joint_product <- crossprod(x, x * w)
  dimnames(joint_product) <- list(colnames(x), colnames(x))
  category_mass <- lapply(seq_len(ncol(x)), function(j) {
    out <- vapply(
      supports[[j]],
      function(score) sum(w[x[, j] == score]),
      numeric(1)
    )
    names(out) <- as.character(supports[[j]])
    out
  })
  names(category_mass) <- colnames(x)
  list(
    means = means,
    category_mass = category_mass,
    joint_product = joint_product,
    weights = w,
    input_weight_sum = weight_info$input_sum,
    zero_weight_rows = weight_info$zero_weight_rows
  )
}

bm_ordinal_pair_state <- function(moments, supports,
                                  zero_tol = 64 * .Machine$double.eps,
                                  eta = sqrt(.Machine$double.eps)) {
  item_names <- names(moments$means)
  pair_index <- utils::combn(seq_along(item_names), 2L)
  out <- vector("list", ncol(pair_index))
  for (k in seq_len(ncol(pair_index))) {
    i <- pair_index[1L, k]
    j <- pair_index[2L, k]
    covariance <- moments$joint_product[i, j] -
      moments$means[i] * moments$means[j]
    maximum_covariance <- bm_ordinal_max_covariance_transport(
      supports[[i]], moments$category_mass[[i]],
      supports[[j]], moments$category_mass[[j]]
    )$maximum_covariance
    state <- .bm_denominator_state(maximum_covariance, zero_tol, eta)
    out[[k]] <- data.frame(
      item_i = item_names[i],
      item_j = item_names[j],
      mean_i = unname(moments$means[i]),
      mean_j = unname(moments$means[j]),
      cov = unname(covariance),
      cov_max_raw = unname(state$raw),
      cov_max = unname(state$value),
      H_ij = if (identical(state$status, "zero")) {
        NA_real_
      } else {
        unname(covariance / state$value)
      },
      denominator_status = state$status,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

.bm_evaluate_ordinal_draw_batch <- function(
    x, weights, supports, pair_index, incidence, zero_tol, eta) {
  means <- weights %*% x
  survival <- lapply(seq_len(ncol(x)), function(j) {
    thresholds <- supports[[j]][-1L]
    indicator <- vapply(
      thresholds,
      function(threshold) as.numeric(x[, j] >= threshold),
      numeric(nrow(x))
    )
    if (is.null(dim(indicator))) {
      indicator <- matrix(indicator, ncol = 1L)
    }
    weights %*% indicator
  })

  pair_count <- ncol(pair_index)
  covariance <- matrix(NA_real_, nrow(weights), pair_count)
  denominator_raw <- matrix(NA_real_, nrow(weights), pair_count)
  for (k in seq_len(pair_count)) {
    i <- pair_index[1L, k]
    j <- pair_index[2L, k]
    expected_product <- as.numeric(weights %*% (x[, i] * x[, j]))
    covariance[, k] <- expected_product - means[, i] * means[, j]

    gaps_i <- diff(supports[[i]])
    gaps_j <- diff(supports[[j]])
    maximum_shifted_product <- numeric(nrow(weights))
    for (a in seq_along(gaps_i)) {
      for (b in seq_along(gaps_j)) {
        maximum_shifted_product <- maximum_shifted_product +
          gaps_i[a] * gaps_j[b] *
          pmin(survival[[i]][, a], survival[[j]][, b])
      }
    }
    shifted_mean_i <- means[, i] - min(supports[[i]])
    shifted_mean_j <- means[, j] - min(supports[[j]])
    denominator_raw[, k] <- maximum_shifted_product -
      shifted_mean_i * shifted_mean_j
  }

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
