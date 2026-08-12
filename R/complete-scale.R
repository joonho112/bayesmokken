bm_complete_scale <- function(validated, row_ids = NULL,
                              missing = "complete_scale") {
  valid_class <- inherits(validated, "bm_validated_binary") ||
    inherits(validated, "bm_validated_ordinal")
  if (!valid_class) {
    .bm_abort(
      "Internal error: expected validated binary or ordinal data.",
      "bm_internal_error"
    )
  }
  if (!identical(missing, "complete_scale")) {
    .bm_abort(
      "`missing` must be \"complete_scale\"; pairwise deletion targets a different pseudo-estimand."
    )
  }

  n_total <- nrow(validated$data)
  if (is.null(row_ids)) {
    row_ids <- rownames(validated$data)
    if (is.null(row_ids)) {
      row_ids <- as.character(seq_len(n_total))
    }
  }
  if (length(row_ids) != n_total || anyNA(row_ids) || anyDuplicated(row_ids)) {
    .bm_abort("`row_ids` must be unique, nonmissing, and have one value per input row.")
  }

  included <- stats::complete.cases(validated$data)
  included_index <- which(included)
  excluded_index <- which(!included)
  if (length(included_index) < 2L) {
    .bm_abort("Fewer than two complete-scale respondents remain after filtering.")
  }

  item_set_hash <- digest::digest(validated$item_names, algo = "sha256")
  row_hash <- digest::digest(
    list(index = included_index, id = as.character(row_ids[included])),
    algo = "sha256"
  )

  out <- list(
    data = validated$data[included, , drop = FALSE],
    included = included,
    included_index = included_index,
    excluded_index = excluded_index,
    included_row_ids = as.character(row_ids[included]),
    excluded_row_ids = as.character(row_ids[!included]),
    n_total = n_total,
    n_complete = length(included_index),
    n_excluded = length(excluded_index),
    exclusion_fraction = length(excluded_index) / n_total,
    row_target = "complete_scale",
    target_statement = "F_cc = Law(Y_S | R_all = 1)",
    row_hash = row_hash,
    item_set_hash = item_set_hash,
    item_names = validated$item_names,
    score_manifest = validated$score_manifest,
    score_map_hash = validated$score_map_hash
  )
  if (inherits(validated, "bm_validated_ordinal")) {
    out$item_type <- "ordinal"
    out$supports <- validated$supports
    out$support_hash <- validated$support_hash
  }
  structure(out, class = "bm_complete_scale")
}

# Hash the complete, scored analysis input rather than only its row-selection
# and scoring schema. Component hashes remain useful for diagnosis, but only
# this digest changes when a retained response value or a normalized point
# weight changes.
.bm_analysis_input_hash <- function(
    complete, normalized_weights = NULL,
    cluster_info = NULL, cluster_target = NULL) {
  if (!inherits(complete, "bm_complete_scale")) {
    .bm_abort("Internal analysis hash received an invalid complete scale.",
              "bm_internal_error")
  }
  weight_info <- .bm_normalize_weights(
    normalized_weights, nrow(complete$data)
  )
  cluster_labels <- if (is.null(cluster_info)) {
    NULL
  } else {
    if (!is.list(cluster_info) ||
        length(cluster_info$label) != nrow(complete$data)) {
      .bm_abort("Internal analysis hash received invalid cluster provenance.",
                "bm_internal_error")
    }
    as.character(cluster_info$label)
  }
  digest::digest(
    list(
      version = "bayesmokken-analysis-input-v1",
      scored_response_matrix = unname(complete$data),
      included_row_ids = as.character(complete$included_row_ids),
      item_names = as.character(complete$item_names),
      normalized_point_weights = unname(weight_info$normalized),
      declared_supports = if (is.null(complete$supports)) {
        NULL
      } else {
        complete$supports[complete$item_names]
      },
      score_map_hash = complete$score_map_hash,
      cluster_labels = cluster_labels,
      cluster_target = if (is.null(cluster_info)) NULL else cluster_target
    ),
    algo = "sha256",
    serialize = TRUE
  )
}
