.bm_cluster_bb_types <- function() {
  c(
    complete_record = "complete_record_cluster_bayesian_bootstrap",
    hierarchical_cluster = "two_stage_hierarchical_cluster_bayesian_bootstrap"
  )
}

# One predicate for every printer and extractor that needs to know whether a
# fit took the clustered path, whichever of the two cluster laws it used.
.bm_is_clustered_fit <- function(x) {
  isTRUE(x$bb_type %in% .bm_cluster_bb_types())
}

new_bm_cluster_bb_h <- function(
    empirical, draws, cluster_info, weight_draws, computation,
    weight_law = c("complete_record", "hierarchical_cluster")) {
  weight_law <- match.arg(weight_law)
  complete_record <- identical(weight_law, "complete_record")
  rng <- list(
    seed = weight_draws$seed,
    rng_kind = weight_draws$rng_kind,
    generator = weight_draws$generator,
    stream_map_hash = weight_draws$draw_identity_hash,
    draw_identity_hash = weight_draws$draw_identity_hash
  )
  if (complete_record) {
    rng$rng_normal_kind <- weight_draws$rng_normal_kind
    rng$rng_sample_kind <- weight_draws$rng_sample_kind
    rng$rng_mapping <- weight_draws$rng_mapping
  }
  structure(
    list(
      empirical = empirical,
      draws = draws,
      bb_type = unname(.bm_cluster_bb_types()[[weight_law]]),
      functional = "ordinal finite-support H(F) for a fixed item set S",
      interpretation = paste(
        if (complete_record) {
          "Complete-record Bayesian-bootstrap approximation to repeated-sampling"
        } else {
          "Hierarchical Bayesian-bootstrap approximation to repeated-sampling"
        },
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
        weight_law = weight_law,
        law_id = weight_draws$law_id,
        target_id = weight_draws$target_id,
        cluster_target = empirical$estimand$cluster_target,
        normalization_formula = weight_draws$normalization_formula,
        within_record_reweighting = !complete_record
      ),
      rng = rng,
      computation = computation,
      interval = NULL,
      summaries = NULL,
      mc_diagnostics = NULL
    ),
    class = "bm_bb_h"
  )
}
