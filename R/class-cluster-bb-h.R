new_bm_cluster_bb_h <- function(
    empirical, draws, cluster_info, weight_draws, computation) {
  structure(
    list(
      empirical = empirical,
      draws = draws,
      bb_type = "two_stage_hierarchical_cluster_bayesian_bootstrap",
      functional = "ordinal finite-support H(F) for a fixed item set S",
      interpretation = paste(
        "Hierarchical Bayesian-bootstrap approximation to repeated-sampling",
        "uncertainty under the locked cluster target; not a posterior",
        "probability, posterior credible interval, predictive sample,",
        "or scale-selection analysis."
      ),
      grouping = list(
        n_respondents = cluster_info$N,
        n_patterns = NA_integer_,
        counts = cluster_info$sizes,
        pattern_hash = NULL,
        n_clusters = cluster_info$G,
        cluster_id_hash = weight_draws$cluster_id_hash
      ),
      sampling = list(
        weight_law = "hierarchical_cluster",
        law_id = weight_draws$law_id,
        target_id = weight_draws$target_id,
        cluster_target = empirical$estimand$cluster_target,
        normalization_formula = weight_draws$normalization_formula
      ),
      rng = list(
        seed = weight_draws$seed,
        rng_kind = weight_draws$rng_kind,
        generator = weight_draws$generator,
        stream_map_hash = weight_draws$draw_identity_hash,
        draw_identity_hash = weight_draws$draw_identity_hash
      ),
      computation = computation,
      interval = NULL,
      summaries = NULL,
      mc_diagnostics = NULL
    ),
    class = "bm_bb_h"
  )
}
