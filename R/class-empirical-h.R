new_bm_empirical_h <- function(pair, item, scale, complete, masses,
                               zero_tol, eta) {
  structure(
    list(
      pair = pair,
      item = item,
      scale = scale,
      estimand = list(
        version = "paperA-HF-binary-v1",
        item_type = "binary",
        item_set = "fixed",
        row_target = complete$row_target,
        target_statement = complete$target_statement,
        pair_numerator = "E(Y_i Y_j) - E(Y_i) E(Y_j)",
        pair_denominator = "min(p_i, p_j) - p_i p_j",
        item_aggregation = "ratio_of_sums",
        scale_aggregation = "ratio_of_sums"
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
        score_manifest = complete$score_manifest,
        input_weight_sum = masses$input_weight_sum,
        zero_weight_complete_rows = masses$zero_weight_rows
      ),
      tolerances = list(zero_tol = zero_tol, eta = eta)
    ),
    class = "bm_empirical_h"
  )
}

#' @export
summary.bm_empirical_h <- function(object, ...) {
  structure(
    list(
      scale = object$scale,
      item = object$item,
      n_total = object$provenance$n_total,
      n_complete = object$provenance$n_complete,
      n_excluded = object$provenance$n_excluded,
      pair_status = table(object$pair$denominator_status),
      item_status = table(object$item$denominator_status),
      estimand = object$estimand$target_statement
    ),
    class = "summary_bm_empirical_h"
  )
}
