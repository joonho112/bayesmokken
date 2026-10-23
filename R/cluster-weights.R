# Locked cluster targets and two-stage hierarchical Bayesian-bootstrap law.
#
# Production port of the confirmed Paper A v4 two-stage law only. Comparator
# laws and method-selection machinery are intentionally absent.

bm_validate_cluster <- function(cluster) {
  if (length(cluster) < 2L || anyNA(cluster)) {
    .bm_abort(
      "`cluster` must be complete and contain one label per input row."
    )
  }
  label <- as.character(cluster)
  if (any(!nzchar(label))) {
    .bm_abort("`cluster` labels must be nonempty.")
  }
  levels <- unique(label)
  index <- match(label, levels)
  sizes <- tabulate(index, nbins = length(levels))
  if (length(levels) < 2L || any(sizes < 1L)) {
    .bm_abort("At least two nonempty complete-scale clusters are required.")
  }
  list(
    label = label,
    levels = levels,
    index = index,
    sizes = as.integer(sizes),
    G = length(levels),
    N = length(label)
  )
}

bm_complete_cluster <- function(cluster, complete) {
  if (is.null(cluster) || length(cluster) != complete$n_total) {
    .bm_abort(
      "`cluster` must contain one nonmissing label per original input row."
    )
  }
  if (anyNA(cluster)) {
    .bm_abort("`cluster` must not contain missing labels.")
  }
  bm_validate_cluster(cluster[complete$included])
}

bm_cluster_hash <- function(cluster_info) {
  digest::digest(
    list(
      labels = cluster_info$levels,
      index = cluster_info$index,
      sizes = cluster_info$sizes
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

bm_cluster_point_weights <- function(
    cluster_info, target = c("respondent", "equal_cluster")) {
  target <- match.arg(target)
  if (identical(target, "respondent")) {
    rep(1 / cluster_info$N, cluster_info$N)
  } else {
    1 / (cluster_info$G * cluster_info$sizes[cluster_info$index])
  }
}

.bm_with_seed <- function(seed, code) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != as.integer(seed)) {
    .bm_abort("`seed` must be one finite nonnegative integer.")
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(.bm_restore_rng(old_kind, had_seed, old_seed), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed))
  force(code)
}

.bm_dirichlet_matrix <- function(k, draws) {
  out <- matrix(stats::rexp(k * draws), nrow = k, ncol = draws)
  sweep(out, 2L, colSums(out), "/")
}

bm_cluster_hbb_weights <- function(
    cluster_info, draws, seed,
    target = c("respondent", "equal_cluster")) {
  target <- match.arg(target)
  weights <- .bm_with_seed(seed, {
    cluster_mass <- .bm_dirichlet_matrix(cluster_info$G, draws)
    out <- matrix(0, nrow = cluster_info$N, ncol = draws)
    denominator <- if (identical(target, "respondent")) {
      colSums(cluster_info$sizes * cluster_mass)
    } else {
      rep(1, draws)
    }
    for (g in seq_len(cluster_info$G)) {
      rows <- which(cluster_info$index == g)
      within_mass <- .bm_dirichlet_matrix(length(rows), draws)
      if (identical(target, "respondent")) {
        out[rows, ] <- sweep(
          within_mass,
          2L,
          cluster_info$sizes[g] * cluster_mass[g, ] / denominator,
          "*"
        )
      } else {
        out[rows, ] <- sweep(
          within_mass, 2L, cluster_mass[g, ], "*"
        )
      }
    }
    out
  })
  law_id <- if (identical(target, "respondent")) {
    "V4-PRI-TWO-STAGE-HBB-RESP-v1"
  } else {
    "V4-SENS-TWO-STAGE-HBB-CLUSTER-v1"
  }
  target_id <- if (identical(target, "respondent")) {
    "V4-TARGET-RESPONDENT-WEIGHTED-v1"
  } else {
    "V4-TARGET-EQUAL-CLUSTER-v1"
  }
  normalization_formula <- if (identical(target, "respondent")) {
    "W_gi = m_g A_g B_gi / sum_h m_h A_h"
  } else {
    "W_gi = A_g B_gi"
  }
  structure(
    list(
      weights = weights,
      law_id = law_id,
      target_id = target_id,
      cluster_id_hash = bm_cluster_hash(cluster_info),
      cluster_sizes = cluster_info$sizes,
      normalization_formula = normalization_formula,
      seed = as.integer(seed),
      rng_kind = "L'Ecuyer-CMRG",
      generator = "two_stage_independent_exponential_normalization",
      draw_id = seq_len(draws),
      draw_identity_hash = digest::digest(weights, algo = "sha256")
    ),
    class = "bm_cluster_weight_draws"
  )
}


# -----------------------------------------------------------------------------
# Complete-record Bayesian-bootstrap law (clustered article, current version;
# protocol C2, archive method `one_stage_basic_G_adjusted`).
#
# One Dirichlet_G(1, ..., 1) vector per draw over the complete cluster records.
# The relative weights inside a record are never perturbed: per respondent the
# weight is A_g / m_g for the equal-cluster target and A_g / sum_h m_h A_h for
# the respondent-weighted target, with the random denominator recomputed in
# every draw.
#
# The stream is the archive's draw-major mapping. Every draw consumes G + N
# unit-rate exponentials from one L'Ecuyer-CMRG stream; the first G are
# normalized to the record weights and the remaining N -- the archive's
# within-record block, used there only by the historical two-stage comparator
# -- are discarded. Consuming them is what makes the package's draws identical
# to the archive's for the same seed, and because each draw's block is
# contiguous in the stream the result does not depend on the block size used
# here or on the batch size used downstream.
# -----------------------------------------------------------------------------

.bm_crbb_rng_mapping <- "V4R-DRAW-MAJOR-EXP-GPLUSN-CMRG-v1"
.bm_crbb_stream_block <- 256L

.bm_with_archive_rng <- function(seed, code) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != as.integer(seed)) {
    .bm_abort("`seed` must be one finite nonnegative integer.",
              hint = .bm_hints$seed_integer)
  }
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(.bm_restore_rng(old_kind, had_seed, old_seed), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  force(code)
}

bm_cluster_crbb_weights <- function(
    cluster_info, draws, seed,
    target = c("respondent", "equal_cluster")) {
  target <- match.arg(target)
  if (!is.numeric(draws) || length(draws) != 1L || is.na(draws) ||
      !is.finite(draws) || draws < 1 || draws != as.integer(draws)) {
    .bm_abort("`draws` must be a positive integer.")
  }
  draws <- as.integer(draws)
  G <- cluster_info$G
  N <- cluster_info$N
  record_mass <- .bm_with_archive_rng(seed, {
    out <- matrix(NA_real_, nrow = G, ncol = draws)
    generated <- 0L
    while (generated < draws) {
      block <- min(.bm_crbb_stream_block, draws - generated)
      exponential <- matrix(
        stats::rexp((G + N) * block), nrow = G + N, ncol = block
      )
      top <- exponential[seq_len(G), , drop = FALSE]
      out[, generated + seq_len(block)] <- sweep(top, 2L, colSums(top), "/")
      generated <- generated + block
    }
    out
  })
  index <- cluster_info$index
  sizes <- cluster_info$sizes
  expanded <- record_mass[index, , drop = FALSE]
  weights <- if (identical(target, "respondent")) {
    sweep(expanded, 2L, colSums(sizes * record_mass), "/")
  } else {
    expanded / sizes[index]
  }
  law_id <- if (identical(target, "respondent")) {
    "V5-CRBB-RESP-v1"
  } else {
    "V5-CRBB-CLUSTER-v1"
  }
  target_id <- if (identical(target, "respondent")) {
    "V4-TARGET-RESPONDENT-WEIGHTED-v1"
  } else {
    "V4-TARGET-EQUAL-CLUSTER-v1"
  }
  normalization_formula <- if (identical(target, "respondent")) {
    "W_gi = A_g / sum_h m_h A_h"
  } else {
    "W_gi = A_g / m_g"
  }
  structure(
    list(
      weights = weights,
      record_mass = record_mass,
      law_id = law_id,
      target_id = target_id,
      cluster_id_hash = bm_cluster_hash(cluster_info),
      cluster_sizes = cluster_info$sizes,
      normalization_formula = normalization_formula,
      within_record_reweighting = FALSE,
      seed = as.integer(seed),
      rng_kind = "L'Ecuyer-CMRG",
      rng_normal_kind = "Inversion",
      rng_sample_kind = "Rejection",
      rng_mapping = .bm_crbb_rng_mapping,
      generator = "draw_major_exponential_normalization_G_plus_N",
      draw_id = seq_len(draws),
      draw_identity_hash = digest::digest(weights, algo = "sha256")
    ),
    class = "bm_cluster_weight_draws"
  )
}
