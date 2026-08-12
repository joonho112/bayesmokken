new_bm_bb_h <- function(empirical, draws, grouping, rng, computation) {
  structure(
    list(
      empirical = empirical,
      draws = draws,
      bb_type = "standard_respondent_bayesian_bootstrap",
      functional = "H(F) for a fixed item set S",
      interpretation = paste(
        "Bayesian-bootstrap approximation to repeated-sampling uncertainty;",
        "not a posterior probability, posterior credible interval,",
        "predictive sample, or scale-selection analysis."
      ),
      grouping = list(
        n_respondents = grouping$n_respondents,
        n_patterns = grouping$n_patterns,
        counts = grouping$counts,
        pattern_hash = grouping$pattern_hash
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

#' @export
summary.bm_bb_h <- function(object, ...) {
  if (!is.null(object$summaries)) {
    return(object$summaries)
  }
  bm_bb_summary(object, ...)
}
