bm_h_pair <- function(masses,
                      zero_tol = 64 * .Machine$double.eps,
                      eta = sqrt(.Machine$double.eps)) {
  covariance <- bm_weighted_covariance(masses)
  cmax <- bm_binary_cmax(masses$marginals, zero_tol = zero_tol, eta = eta)
  list(
    pair = bm_pair_state(
      covariance$covariance,
      masses$marginals,
      cmax,
      zero_tol = zero_tol,
      eta = eta
    ),
    covariance = covariance,
    cmax = cmax
  )
}
