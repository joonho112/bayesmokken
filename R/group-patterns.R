bm_group_patterns <- function(x) {
  if (!is.matrix(x) || !is.numeric(x) || anyNA(x) ||
      any(!x %in% c(0, 1))) {
    .bm_abort("Pattern grouping requires a complete numeric binary matrix.",
              "bm_internal_error")
  }
  keys <- apply(x, 1L, paste0, collapse = "")
  first <- !duplicated(keys)
  unique_keys <- keys[first]
  row_group <- match(keys, unique_keys)
  patterns <- x[first, , drop = FALSE]
  rownames(patterns) <- paste0("pattern_", seq_len(nrow(patterns)))
  counts <- tabulate(row_group, nbins = nrow(patterns))
  names(counts) <- rownames(patterns)
  structure(
    list(
      patterns = patterns,
      counts = counts,
      row_group = row_group,
      keys = unique_keys,
      n_respondents = nrow(x),
      n_patterns = nrow(patterns),
      pattern_hash = digest::digest(
        list(patterns = patterns, counts = counts),
        algo = "sha256"
      )
    ),
    class = "bm_pattern_grouping"
  )
}

bm_group_row_weights <- function(row_weights, grouping) {
  if (!inherits(grouping, "bm_pattern_grouping")) {
    .bm_abort("Internal error: invalid pattern grouping.", "bm_internal_error")
  }
  if (!is.numeric(row_weights) ||
      length(row_weights) != grouping$n_respondents ||
      anyNA(row_weights) || any(!is.finite(row_weights)) ||
      any(row_weights < 0)) {
    .bm_abort("Row weights are invalid for this grouping.", "bm_internal_error")
  }
  grouped <- rowsum(
    matrix(row_weights, ncol = 1L),
    group = grouping$row_group,
    reorder = FALSE
  )[, 1L]
  names(grouped) <- rownames(grouping$patterns)
  grouped
}

bm_grouped_bb_weights <- function(grouping, draws, seed) {
  if (!inherits(grouping, "bm_pattern_grouping")) {
    .bm_abort("Internal error: invalid pattern grouping.", "bm_internal_error")
  }
  out <- bm_dirichlet_draws(
    stats::setNames(as.numeric(grouping$counts), names(grouping$counts)),
    draws = draws,
    seed = seed
  )
  out$law <- sprintf(
    "Dirichlet(pattern counts: %s)",
    paste(grouping$counts, collapse = ",")
  )
  out$pattern_hash <- grouping$pattern_hash
  class(out) <- c("bm_grouped_bb_weight_draws", class(out))
  out
}
