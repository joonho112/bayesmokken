bm_weighted_covariance <- function(masses) {
  mu <- masses$marginals
  covariance <- masses$joint_product - outer(mu, mu)
  dimnames(covariance) <- list(names(mu), names(mu))
  list(
    mean = mu,
    expected_product = masses$joint_product,
    covariance = covariance,
    convention = "probability_measure"
  )
}
