bm_dirichlet_draws <- function(alpha, draws, seed) {
  if (!is.numeric(alpha) || length(alpha) < 1L || anyNA(alpha) ||
      any(!is.finite(alpha)) || any(alpha <= 0)) {
    .bm_abort("Dirichlet `alpha` must contain finite strictly positive values.")
  }
  streams <- bm_rng_streams(seed, draws)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(.bm_restore_rng(old_kind, had_seed, old_seed), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")

  out <- matrix(NA_real_, nrow = as.integer(draws), ncol = length(alpha))
  for (b in seq_len(nrow(out))) {
    assign(".Random.seed", streams[[b]], envir = .GlobalEnv)
    gamma <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
    out[b, ] <- gamma / sum(gamma)
  }
  colnames(out) <- names(alpha)
  structure(
    list(
      weights = out,
      alpha = alpha,
      draws = nrow(out),
      seed = as.integer(seed),
      rng_kind = "L'Ecuyer-CMRG",
      generator = "independent_gamma_normalization",
      stream_map_hash = digest::digest(streams, algo = "sha256")
    ),
    class = "bm_dirichlet_draws"
  )
}

bm_bb_weights <- function(n, draws, seed) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      !is.finite(n) || n < 2 || n != as.integer(n)) {
    .bm_abort("Standard respondent BB requires integer `n >= 2`.")
  }
  out <- bm_dirichlet_draws(rep.int(1, as.integer(n)), draws, seed)
  out$law <- sprintf("Dirichlet(1_%d)", as.integer(n))
  class(out) <- c("bm_bb_weight_draws", class(out))
  out
}
