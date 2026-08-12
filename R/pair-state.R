bm_pair_state <- function(covariance, marginals, cmax,
                          zero_tol = 64 * .Machine$double.eps,
                          eta = sqrt(.Machine$double.eps)) {
  pair_index <- utils::combn(seq_along(marginals), 2L)
  out <- vector("list", ncol(pair_index))

  for (k in seq_len(ncol(pair_index))) {
    i <- pair_index[1L, k]
    j <- pair_index[2L, k]
    state <- .bm_denominator_state(cmax$raw[i, j], zero_tol, eta)
    h <- if (identical(state$status, "zero")) {
      NA_real_
    } else {
      covariance[i, j] / state$value
    }
    out[[k]] <- data.frame(
      item_i = names(marginals)[i],
      item_j = names(marginals)[j],
      p_i = unname(marginals[i]),
      p_j = unname(marginals[j]),
      cov = unname(covariance[i, j]),
      cov_max_raw = unname(state$raw),
      cov_max = unname(state$value),
      H_ij = unname(h),
      denominator_status = state$status,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
