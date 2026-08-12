# =============================================================================
# Actionable failures
#
# The estimation engine already fails in the right places: a wrong mode, a
# missing support, a fractional seed, a level other than 95% are all typed
# errors rather than silent fallbacks. What v0 did not do was say what to do
# about it.
#
# `.bm_abort(hint =)` appends a suggestion after the original message. The
# original text and the condition class are untouched, so existing tests that
# match on them keep passing, and the G-1 parity gate covers the rest.
#
# The hints below are attached to failures users actually hit. They name a
# function to call or an explicit scientific choice to make.
# =============================================================================

# Hint text is kept in one place so the same failure never gets two different
# pieces of advice.
.bm_hints <- list(
  supports_missing = paste(
    "Ordinal items need `supports`. Build it with",
    "supports <- bm_supports(data, items)."
  ),
  cluster_target_missing = paste(
    "Choose `cluster_target = \"respondent\"` or",
    "`cluster_target = \"equal_cluster\"`; the package will not choose for you."
  ),
  cluster_with_binary = paste(
    "Clustered inference is defined for ordinal items only.",
    "Drop `cluster`, or declare item_type = \"ordinal\" with `supports`."
  ),
  ordinal_needs_cluster_law = paste(
    "Ordinal random-weight intervals use the hierarchical law.",
    "Pass cluster = <labels> and weight_law = \"hierarchical_cluster\"."
  ),
  level_fixed = paste(
    "The calibration was validated at 95% only, so `level` is fixed at 0.95."
  ),
  seed_integer = "Use an integer seed, for example seed = 1L.",
  non_numeric_items = paste(
    "Character, factor and logical columns need an explicit score map.",
    "Build one with score_map <- bm_score_map(data, items, recode = ...)."
  ),
  screen_first = "Run bm_check_data(data) to see what the package sees."
)

#' Report the package's guard rails
#'
#' Lists the situations in which `bayesmokken` refuses to compute, with the
#' reason for each. These are deliberate: every one of them is a case where a
#' plausible-looking number would be produced under an assumption the method
#' has not been validated for.
#'
#' @return A data frame with one row per guard rail: `situation`, `reason` and
#'   `what_to_do`.
#'
#' @examples
#' bm_guardrails()
#'
#' @family bayesmokken-diagnostics
#' @seealso [bm_check_data()]
#' @export
bm_guardrails <- function() {
  data.frame(
    situation = c(
      "item_type = \"binary\" with `cluster`",
      "`cluster` supplied without `cluster_target`",
      "ordinal items without `supports`",
      "ordinal items, random weights, no cluster",
      "level other than 0.95",
      "fractional seed",
      "character or factor items",
      "fewer than two items",
      "too few finite bootstrap draws",
      "dichotomous (two-category) items with `cluster`",
      "fewer than 22 complete-scale clusters for a calibrated interval",
      "more than 80 complete-scale clusters for a calibrated interval"
    ),
    reason = c(
      "the clustered procedure was developed and validated for polytomous items",
      "respondent-weighted and equal-cluster H are different population targets",
      "the maximum covariance is computed at the declared support",
      "the article's cluster-aware law needs the cluster structure",
      "the width calibration was fixed and validated at 95% only",
      "1.1 and 1L would otherwise silently give the same stream",
      "the mapping from labels to scores is a research decision, not a default",
      "a scale coefficient needs at least one item pair",
      "an interval built on too few defined draws is not reportable",
      paste("crossing the binary and clustered paths is refused outright, not",
            "only for the calibrated interval: the clustered procedure was",
            "developed and validated for polytomous items"),
      "cluster counts below 22 were explicitly left for future validation",
      "the confirmation design ended at 80 clusters"
    ),
    what_to_do = c(
      .bm_hints$cluster_with_binary,
      .bm_hints$cluster_target_missing,
      .bm_hints$supports_missing,
      .bm_hints$ordinal_needs_cluster_law,
      .bm_hints$level_fixed,
      .bm_hints$seed_integer,
      .bm_hints$non_numeric_items,
      "Supply at least two item columns.",
      "Increase `draws`, or inspect the denominators with bm_diagnostics().",
      paste("Drop `cluster` and analyse the binary scale on its own path,",
            "which is the procedure the binary article validates. Clustered",
            "inference for dichotomous items is outside this package's scope."),
      paste("Use mokken_h() for the deterministic coefficient, or collect at",
            "least 22 complete-scale clusters before requesting calibration."),
      paste("The interval is returned with an extrapolation warning and scope",
            "metadata; report that it lies outside the confirmed G range.")
    ),
    stringsAsFactors = FALSE
  )
}
