# =============================================================================
# Parity fixtures (G-1)
#
# Eight fixed datasets used to prove that bayesmokken v1 reproduces v0
# bit-for-bit. Sourced by BOTH the v0 reference run (dev/parity/10-compute-v0.R,
# executed in a separate R process against the v0 library) and the v1 gate
# (tests/testthat/test-zzz-parity-v0.R).
#
# Rules
#   * base R only. No package calls, so the identical file runs under v0.
#   * Fully seeded. bm_parity_fixtures() must return the same object every time
#     on any machine with the same R version.
#   * Each fixture declares which entry points apply to it. Ordinal data
#     WITHOUT clusters cannot take a random-weight interval (the mode contract
#     forbids it), so those fixtures are deterministic-only.
# =============================================================================

bm_parity_fixtures <- function(regenerate = FALSE) {
  # The fixture inputs are FROZEN, not regenerated.
  #
  # They used to be built here from a seed on every run. That silently broke
  # the gate off macOS: `rbinom()` is not bit-identical across platforms, so
  # Windows and Linux generated *different response matrices* and the test
  # compared this engine on one dataset against the v0 engine on another. The
  # differences it reported were 0.5 against 0.6 in item proportions -- data
  # differences, not engine differences -- while a real engine change would
  # have been invisible underneath them.
  #
  # Loading the frozen inputs makes the comparison what it always claimed to
  # be: the same eight datasets through two engines. Pass regenerate = TRUE
  # only to rebuild the frozen file, and re-baseline the reference with it.
  frozen <- system.file("extdata", "parity", "parity-inputs.rds",
                        package = "bayesmokken")
  if (!regenerate && nzchar(frozen) && file.exists(frozen)) {
    return(readRDS(frozen))
  }

  # --- helpers -------------------------------------------------------------
  gen_binary <- function(n, j, seed) {
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    theta <- stats::rnorm(n)
    b <- seq(-1.2, 1.2, length.out = j)
    p <- stats::plogis(outer(theta, b, "-"))
    m <- matrix(stats::rbinom(n * j, 1, as.vector(p)), nrow = n)
    colnames(m) <- sprintf("item%02d", seq_len(j))
    as.data.frame(m)
  }

  gen_ordinal <- function(n, j, k, seed) {
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    theta <- stats::rnorm(n)
    b <- seq(-1.0, 1.0, length.out = j)
    m <- matrix(0L, nrow = n, ncol = j)
    cuts <- seq(-1.5, 1.5, length.out = k - 1L)
    for (jj in seq_len(j)) {
      eta <- theta - b[jj]
      probs <- stats::plogis(outer(eta, cuts, "-"))
      m[, jj] <- rowSums(probs > matrix(stats::runif(n * (k - 1L)),
                                        nrow = n, ncol = k - 1L))
    }
    colnames(m) <- sprintf("item%02d", seq_len(j))
    as.data.frame(m)
  }

  gen_clustered <- function(n_clusters, avg_size, j, k, seed, icc_sd = 0.7) {
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
    sizes <- pmax(3L, round(avg_size * stats::runif(n_clusters, 0.5, 1.5)))
    g <- rep(seq_len(n_clusters), times = sizes)
    n <- length(g)
    u <- stats::rnorm(n_clusters, sd = icc_sd)
    theta <- u[g] + stats::rnorm(n, sd = sqrt(1 - icc_sd^2 / 2))
    b <- seq(-1.0, 1.0, length.out = j)
    m <- matrix(0L, nrow = n, ncol = j)
    cuts <- seq(-1.5, 1.5, length.out = k - 1L)
    for (jj in seq_len(j)) {
      eta <- theta - b[jj]
      probs <- stats::plogis(outer(eta, cuts, "-"))
      m[, jj] <- rowSums(probs > matrix(stats::runif(n * (k - 1L)),
                                        nrow = n, ncol = k - 1L))
    }
    colnames(m) <- sprintf("item%02d", seq_len(j))
    list(data = as.data.frame(m), cluster = sprintf("g%03d", g))
  }

  supports_of <- function(dat, k) {
    sup <- lapply(seq_along(dat), function(i) seq.int(0L, k - 1L))
    names(sup) <- names(dat)
    sup
  }

  fx <- list()

  # --- F1-F3: binary, deterministic + respondent Bayesian bootstrap --------
  for (cfg in list(list(id = "F1", n = 40L,  j = 5L,  seed = 101L),
                   list(id = "F2", n = 200L, j = 8L,  seed = 102L),
                   list(id = "F3", n = 337L, j = 11L, seed = 103L))) {
    fx[[cfg$id]] <- list(
      id = cfg$id, mode = "binary",
      data = gen_binary(cfg$n, cfg$j, cfg$seed),
      args = list(),
      bb = TRUE, draws = 300L, seed = 7L
    )
  }

  # --- F4-F6: ordinal, no clusters, DETERMINISTIC ONLY ---------------------
  # bb_mokken_h() on cluster-free ordinal data is a validation error by design
  # (the mode contract requires the hierarchical law), so these fixtures pin
  # the deterministic transport path only.
  for (cfg in list(list(id = "F4", n = 60L,  j = 4L, k = 3L, seed = 201L),
                   list(id = "F5", n = 150L, j = 6L, k = 5L, seed = 202L),
                   list(id = "F6", n = 150L, j = 4L, k = 5L, seed = 203L))) {
    dat <- gen_ordinal(cfg$n, cfg$j, cfg$k, cfg$seed)
    fx[[cfg$id]] <- list(
      id = cfg$id, mode = "ordinal",
      data = dat,
      args = list(item_type = "ordinal", supports = supports_of(dat, cfg$k)),
      bb = FALSE, draws = NA_integer_, seed = NA_integer_
    )
  }

  # --- F7-F8: clustered ordinal, both targets ------------------------------
  for (cfg in list(list(id = "F7", g = 12L, sz = 15L, j = 5L, k = 5L,
                        seed = 301L, target = "respondent"),
                   list(id = "F8", g = 30L, sz = 21L, j = 6L, k = 5L,
                        seed = 302L, target = "equal_cluster"))) {
    cl <- gen_clustered(cfg$g, cfg$sz, cfg$j, cfg$k, cfg$seed)
    fx[[cfg$id]] <- list(
      id = cfg$id, mode = "clustered_ordinal",
      data = cl$data,
      args = list(
        item_type = "ordinal",
        supports = supports_of(cl$data, cfg$k),
        cluster = cl$cluster,
        cluster_target = cfg$target,
        weight_law = "hierarchical_cluster"
      ),
      bb = TRUE, draws = 300L, seed = 7L
    )
  }

  fx
}

# -----------------------------------------------------------------------------
# Evaluate one fixture against whichever bayesmokken is on the search path.
# Returns a compact, comparable record: full deterministic tables, the full
# typed interval, and digest-free checksums of the draw arrays (checksums are
# computed by the caller so this file stays dependency-free).
# -----------------------------------------------------------------------------
bm_parity_evaluate <- function(fixture) {
  det <- do.call(
    mokken_h,
    c(list(data = fixture$data, warn = FALSE), fixture$args)
  )
  out <- list(
    id = fixture$id,
    mode = fixture$mode,
    pair = det$pair,
    item = det$item,
    scale = det$scale,
    estimand = det$estimand,
    n_complete = det$provenance$n_complete,
    row_hash = det$provenance$row_hash,
    score_map_hash = det$provenance$score_map_hash,
    item_set_hash = det$provenance$item_set_hash
  )
  if (isTRUE(fixture$bb)) {
    fit <- do.call(
      bb_mokken_h,
      c(list(data = fixture$data, draws = fixture$draws, seed = fixture$seed,
             warn = FALSE), fixture$args)
    )
    out$interval <- fit$interval
    out$draws_scale <- fit$draws$scale
    out$draws_item <- fit$draws$item
    out$draws_pair <- fit$draws$pair
    out$rng <- fit$rng
    out$mc <- fit$mc_diagnostics
  }
  out
}
