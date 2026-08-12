new_bm_ordinal_empirical_h <- function(
    pair, item, scale, complete, moments, zero_tol, eta,
    cluster_info = NULL, cluster_target = "respondent",
    knot_diagnostics = NULL) {
  target_id <- if (identical(cluster_target, "equal_cluster")) {
    "V4-TARGET-EQUAL-CLUSTER-v1"
  } else {
    "V4-TARGET-RESPONDENT-WEIGHTED-v1"
  }
  target_statement <- if (is.null(cluster_info)) {
    complete$target_statement
  } else if (identical(cluster_target, "equal_cluster")) {
    "Equal-cluster weighted complete-scale respondent distribution"
  } else {
    "Respondent-weighted complete-scale distribution with cluster provenance"
  }

  structure(
    list(
      pair = pair,
      item = item,
      scale = scale,
      estimand = list(
        version = "paperA-HF-ordinal-finite-support-v1",
        item_type = "ordinal",
        item_set = "fixed",
        row_target = complete$row_target,
        target_id = target_id,
        cluster_target = if (is.null(cluster_info)) NULL else cluster_target,
        target_statement = target_statement,
        pair_numerator = "E(Y_i Y_j) - E(Y_i) E(Y_j)",
        pair_denominator =
          "finite-support comonotone maximum covariance",
        item_aggregation = "ratio_of_sums",
        scale_aggregation = "ratio_of_sums",
        weight_law = if (is.null(cluster_info)) {
          "iid_respondent"
        } else {
          "hierarchical_cluster"
        }
      ),
      provenance = list(
        n_total = complete$n_total,
        n_complete = complete$n_complete,
        n_excluded = complete$n_excluded,
        exclusion_fraction = complete$exclusion_fraction,
        included_index = complete$included_index,
        excluded_index = complete$excluded_index,
        included_row_ids = complete$included_row_ids,
        excluded_row_ids = complete$excluded_row_ids,
        analysis_input_hash = complete$analysis_input_hash,
        row_hash = complete$row_hash,
        item_set_hash = complete$item_set_hash,
        score_map_hash = complete$score_map_hash,
        support_hash = complete$support_hash,
        supports = complete$supports,
        score_manifest = complete$score_manifest,
        input_weight_sum = moments$input_weight_sum,
        zero_weight_complete_rows = moments$zero_weight_rows,
        cluster_id_hash = if (is.null(cluster_info)) {
          NULL
        } else {
          bm_cluster_hash(cluster_info)
        },
        cluster_count = if (is.null(cluster_info)) NULL else cluster_info$G,
        cluster_sizes = if (is.null(cluster_info)) NULL else cluster_info$sizes,
        weight_law = if (is.null(cluster_info)) {
          "iid_respondent"
        } else {
          "hierarchical_cluster"
        }
      ),
      knot_diagnostics = knot_diagnostics,
      tolerances = list(zero_tol = zero_tol, eta = eta),
      category_mass = moments$category_mass
    ),
    class = "bm_empirical_h"
  )
}
