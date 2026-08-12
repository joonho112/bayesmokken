.bm_restore_rng <- function(old_kind, had_seed, old_seed) {
  do.call(RNGkind, as.list(old_kind))
  if (had_seed) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
}

bm_rng_streams <- function(seed, draws) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != as.integer(seed)) {
    .bm_abort("`seed` must be one finite nonnegative integer.",
              hint = .bm_hints$seed_integer)
  }
  if (!is.numeric(draws) || length(draws) != 1L || is.na(draws) ||
      !is.finite(draws) || draws < 1 || draws != as.integer(draws)) {
    .bm_abort("`draws` must be a positive integer.")
  }
  draws <- as.integer(draws)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(.bm_restore_rng(old_kind, had_seed, old_seed), add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed))
  streams <- vector("list", draws)
  streams[[1L]] <- get(".Random.seed", envir = .GlobalEnv)
  if (draws > 1L) {
    for (b in 2:draws) {
      streams[[b]] <- parallel::nextRNGStream(streams[[b - 1L]])
    }
  }
  streams
}
