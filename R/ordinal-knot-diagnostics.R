# Weighted marginal cumulative-mass diagnostics for transport path stability.
#
# Production port from the G18-A-reviewed Paper A v4 diagnostic. Exact knots
# are reported as diagnostics and do not authorize regular-inference claims.

bm_ordinal_knot_diagnostics <- function(
    x, weights = NULL, supports,
    exact_tol = 1e-12, near_tol = 1e-4) {
  weight_info <- .bm_normalize_weights(weights, nrow(x))
  w <- weight_info$normalized
  cdfs <- lapply(seq_len(ncol(x)), function(j) {
    vapply(
      supports[[j]][-length(supports[[j]])],
      function(score) sum(w[x[, j] <= score]),
      numeric(1)
    )
  })
  rows <- list()
  index <- 0L
  for (j in seq_len(ncol(x) - 1L)) {
    for (k in (j + 1L):ncol(x)) {
      gap <- abs(outer(cdfs[[j]], cdfs[[k]], "-"))
      index <- index + 1L
      rows[[index]] <- data.frame(
        item_i = colnames(x)[j],
        item_j = colnames(x)[k],
        minimum_internal_cdf_gap = min(gap),
        exact_knot_count = sum(gap <= exact_tol),
        near_knot_count = sum(gap <= near_tol),
        stringsAsFactors = FALSE
      )
    }
  }
  pair <- do.call(rbind, rows)
  list(
    pair = pair,
    minimum_internal_cdf_gap = min(pair$minimum_internal_cdf_gap),
    exact_knot_count = sum(pair$exact_knot_count),
    near_knot_count = sum(pair$near_knot_count),
    exact_tol = exact_tol,
    near_tol = near_tol
  )
}
