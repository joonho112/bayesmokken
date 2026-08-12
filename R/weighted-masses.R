.bm_normalize_weights <- function(weights, n) {
  if (is.null(weights)) {
    weights <- rep.int(1 / n, n)
  }
  if (!is.numeric(weights) || length(weights) != n || anyNA(weights) ||
      any(!is.finite(weights)) || any(weights < 0)) {
    .bm_abort("`weights` must be finite, nonnegative, and have one value per complete row.")
  }
  weight_sum <- sum(weights)
  if (!is.finite(weight_sum) || weight_sum <= 0) {
    .bm_abort("Complete-row weights must have a strictly positive finite sum.")
  }
  list(
    normalized = weights / weight_sum,
    input_sum = weight_sum,
    zero_weight_rows = which(weights == 0)
  )
}

bm_weighted_masses <- function(x, weights = NULL) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 1L || ncol(x) < 2L ||
      anyNA(x) || any(!x %in% c(0, 1))) {
    .bm_abort("Internal mass kernel requires a complete numeric 0/1 matrix.",
              "bm_internal_error")
  }
  weight_info <- .bm_normalize_weights(weights, nrow(x))
  w <- weight_info$normalized
  mu <- as.numeric(crossprod(w, x))
  names(mu) <- colnames(x)

  category_mass <- cbind(`0` = 1 - mu, `1` = mu)
  rownames(category_mass) <- colnames(x)
  joint_product <- crossprod(x, x * w)
  dimnames(joint_product) <- list(colnames(x), colnames(x))

  pair_tables <- list()
  pair_index <- utils::combn(seq_len(ncol(x)), 2L)
  for (k in seq_len(ncol(pair_index))) {
    i <- pair_index[1L, k]
    j <- pair_index[2L, k]
    p11 <- joint_product[i, j]
    pair_key <- .bm_pair_key(colnames(x)[i], colnames(x)[j])
    pair_tables[[pair_key]] <- unname(c(
      p00 = 1 - mu[i] - mu[j] + p11,
      p01 = mu[j] - p11,
      p10 = mu[i] - p11,
      p11 = p11
    ))
    names(pair_tables[[pair_key]]) <-
      c("p00", "p01", "p10", "p11")
  }

  list(
    marginals = mu,
    category_mass = category_mass,
    joint_product = joint_product,
    pair_tables = pair_tables,
    weights = w,
    input_weight_sum = weight_info$input_sum,
    zero_weight_rows = weight_info$zero_weight_rows
  )
}
