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
