.bm_match_cluster_target <- function(cluster_target) {
  match.arg(cluster_target, c("respondent", "equal_cluster"))
}

.bm_weight_laws <- function() {
  c("auto", "iid_respondent", "complete_record", "hierarchical_cluster")
}

.bm_cluster_weight_laws <- function() {
  c("complete_record", "hierarchical_cluster")
}

.bm_match_weight_law <- function(weight_law) {
  match.arg(weight_law, .bm_weight_laws())
}

.bm_require_cluster_target <- function(cluster, target_is_missing) {
  if (!is.null(cluster) && isTRUE(target_is_missing)) {
    .bm_abort(
      "`cluster_target` must be declared whenever `cluster` is supplied.",
      hint = .bm_hints$cluster_target_missing
    )
  }
  invisible(NULL)
}

.bm_validate_mode_contract <- function(
    item_type, supports, cluster, cluster_target, weight_law,
    random_weight = FALSE) {
  cluster_target <- .bm_match_cluster_target(cluster_target)
  weight_law <- .bm_match_weight_law(weight_law)

  if (identical(item_type, "binary")) {
    if (!is.null(supports)) {
      .bm_abort("`supports` is available only when `item_type = \"ordinal\"`.")
    }
    if (!is.null(cluster)) {
      .bm_abort("Binary scalability with `cluster` is not a supported public method.",
                hint = .bm_hints$cluster_with_binary)
    }
    if (identical(cluster_target, "equal_cluster")) {
      .bm_abort("`cluster_target = \"equal_cluster\"` requires ordinal clustered data.")
    }
    if (weight_law %in% .bm_cluster_weight_laws()) {
      .bm_abort(sprintf(
        "Binary scalability with `weight_law = \"%s\"` is unsupported.",
        weight_law
      ))
    }
    return(list(
      item_type = item_type,
      cluster_target = cluster_target,
      weight_law = "iid_respondent"
    ))
  }

  if (is.null(supports)) {
    .bm_abort(
      "Ordinal scalability requires explicit `supports` for every selected item.",
      hint = .bm_hints$supports_missing
    )
  }

  if (isTRUE(random_weight)) {
    if (is.null(cluster)) {
      .bm_abort(
        "Ordinal `bb_mokken_h()` requires `cluster`; an ordinal iid-respondent interval is not authorized.",
        hint = .bm_hints$ordinal_needs_cluster_law
      )
    }
    if (identical(weight_law, "iid_respondent")) {
      .bm_abort(
        "Ordinal iid-respondent BB intervals are not supported; use a cluster-record weight law.",
        hint = .bm_hints$ordinal_needs_cluster_law
      )
    }
    # `"auto"` resolves to the complete-record law of the clustered article's
    # current version. `"hierarchical_cluster"` is the two-stage law of
    # version 0.9.0 and is kept as an explicitly historical option.
    if (identical(weight_law, "auto")) weight_law <- "complete_record"
  } else if (is.null(cluster)) {
    if (identical(cluster_target, "equal_cluster")) {
      .bm_abort("`cluster_target = \"equal_cluster\"` requires `cluster`.")
    }
    if (weight_law %in% .bm_cluster_weight_laws()) {
      .bm_abort(sprintf("`weight_law = \"%s\"` requires `cluster`.", weight_law))
    }
    weight_law <- "iid_respondent"
  } else {
    if (identical(weight_law, "iid_respondent")) {
      .bm_abort(
        "Clustered ordinal analysis requires `weight_law = \"auto\"`, `\"complete_record\"` or `\"hierarchical_cluster\"`.",
        hint = .bm_hints$ordinal_needs_cluster_law
      )
    }
    if (identical(weight_law, "auto")) weight_law <- "complete_record"
  }

  list(
    item_type = item_type,
    cluster_target = cluster_target,
    weight_law = weight_law
  )
}

# -----------------------------------------------------------------------------
# Path-crossing guard (D-12)
#
# Both articles state that the package refuses to cross the binary and the
# clustered polytomous paths:
#
#   Paper A, Method: "the package enforces the separation by refusing to cross
#   the two paths (binary data with a cluster argument, or ordinal data with
#   the independent-respondent law, produce validation errors rather than
#   silent defaults)."
#
#   Paper B, Method: "a cluster argument with binary data, ordinal data under
#   the independent-respondent law, and cluster-free ordinal random-weight
#   intervals are validation errors, not silent defaults."
#
# Before this guard the claim held only for the CALIBRATED interval: a caller
# who declared item_type = "ordinal" with two-point supports could still obtain
# a deterministic coefficient and a raw equal-tail interval on clustered
# dichotomous data. The coefficient was exact, but the paths had been crossed,
# which is what the sentence says cannot happen.
#
# The guard fires on the situation itself -- dichotomous items together with a
# cluster argument -- so the error names what the reader of either article
# would be checking. It inherits bm_cluster_calibration_scope_error so that
# code and tests keyed to the narrower calibrated-scope class keep working.
# -----------------------------------------------------------------------------

.bm_all_two_point <- function(selected, supports) {
  if (!is.null(supports) && length(supports)) {
    return(all(lengths(supports) == 2L))
  }
  # No declared supports: fall back to what the columns actually contain.
  observed <- lapply(selected, function(x) {
    v <- suppressWarnings(as.numeric(
      if (is.factor(x)) as.character(x) else x
    ))
    sort(unique(v[!is.na(v)]))
  })
  lens <- lengths(observed)
  all(lens > 0L) && all(lens <= 2L) &&
    all(vapply(observed, function(v) all(v %in% c(0, 1)), logical(1)))
}

.bm_guard_path_crossing <- function(selected, supports, cluster) {
  if (is.null(cluster)) return(invisible(NULL))
  if (!.bm_all_two_point(selected, supports)) return(invisible(NULL))
  stop(structure(
    list(
      message = paste0(
        "Dichotomous items with a `cluster` argument cross the two analysis ",
        "paths, which is not a supported public method. Every selected item ",
        "has a two-point support, and the clustered procedure was developed ",
        "and validated for polytomous items."
      ),
      call = NULL,
      validation_scope = "POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB"
    ),
    class = c(
      "bm_path_crossing_error",
      "bm_cluster_calibration_scope_error",
      "bm_validation_scope_error",
      "bm_input_error",
      "error",
      "condition"
    )
  ))
}
