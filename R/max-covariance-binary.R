.bm_denominator_state <- function(value,
                                  zero_tol = 64 * .Machine$double.eps,
                                  eta = sqrt(.Machine$double.eps)) {
  if (length(value) != 1L || !is.numeric(value) || !is.finite(value)) {
    .bm_abort("Denominator must be one finite numeric value.", "bm_internal_error")
  }
  if (!is.numeric(zero_tol) || length(zero_tol) != 1L ||
      !is.finite(zero_tol) || zero_tol < 0) {
    .bm_abort("`zero_tol` must be one finite nonnegative number.")
  }
  if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) ||
      eta < zero_tol) {
    .bm_abort("`eta` must be finite and no smaller than `zero_tol`.")
  }
  if (value < -zero_tol) {
    .bm_abort("Maximum-covariance denominator is materially negative.",
              "bm_invariant_error")
  }
  adjusted <- if (value < 0) 0 else value
  status <- if (adjusted <= zero_tol) {
    "zero"
  } else if (adjusted <= eta) {
    "near_zero"
  } else {
    "positive"
  }
  list(raw = value, value = adjusted, status = status)
}

bm_binary_cmax <- function(marginals,
                           zero_tol = 64 * .Machine$double.eps,
                           eta = sqrt(.Machine$double.eps)) {
  if (!is.numeric(marginals) || anyNA(marginals) ||
      any(!is.finite(marginals)) ||
      any(marginals < -zero_tol | marginals > 1 + zero_tol)) {
    .bm_abort("Binary marginals must lie in [0, 1].", "bm_internal_error")
  }
  raw <- outer(marginals, marginals, pmin) - outer(marginals, marginals)
  value <- raw
  status <- matrix(
    "positive",
    nrow = length(marginals),
    ncol = length(marginals),
    dimnames = list(names(marginals), names(marginals))
  )
  for (i in seq_len(nrow(raw))) {
    for (j in seq_len(ncol(raw))) {
      state <- .bm_denominator_state(raw[i, j], zero_tol, eta)
      value[i, j] <- state$value
      status[i, j] <- state$status
    }
  }
  dimnames(raw) <- dimnames(value) <- dimnames(status)
  list(raw = raw, value = value, status = status)
}
