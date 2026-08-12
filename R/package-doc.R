#' bayesmokken: calibrated Bayesian-bootstrap intervals for Mokken scalability
#'
#' Mokken scale analysis summarizes whether a fixed set of items forms a scale
#' using the scalability coefficient \eqn{H}. In practice \eqn{H} is usually
#' reported as a bare number and compared with rule-of-thumb benchmarks, as
#' though it carried no sampling error. This package supplies intervals for it
#' whose coverage has been checked in large prespecified simulations rather
#' than assumed.
#'
#' @section Two paths:
#'
#' **Binary items, independent respondents.** Respondent weights are drawn
#' from a Bayesian bootstrap, the equal-tailed interval is taken from the
#' resulting draws, and its width is multiplied by \eqn{1 + 0.25/\sqrt{n}} to
#' correct a small measured shortfall in coverage.
#'
#' **Polytomous items, respondents nested in clusters.** The coefficient is
#' computed exactly under arbitrary weights by finite comonotone transport.
#' Uncertainty comes from a two-stage hierarchical Bayesian bootstrap over
#' clusters and then respondents, and the interval is *narrowed* by
#' \eqn{1 - 1.5/\sqrt{G}}, because the uncorrected hierarchical interval was
#' measured to be substantially too wide in the confirmation design. The
#' confirmed cluster-count envelope is \eqn{22 \le G \le 80}; results outside
#' that envelope are not described as confirmed.
#'
#' The two corrections run in opposite directions. [bm_plot_calibration()]
#' shows why in one figure.
#'
#' @section A five-line start:
#' ```r
#' library(bayesmokken)
#' bm_check_data(srs)                            # what does the package see?
#' fit <- bb_mokken_h(srs, draws = 2000, seed = 1)
#' fit                                           # the result and its interval
#' bm_plot_draws(fit)                            # what the correction did
#' ```
#'
#' @section Two population targets, when data are clustered:
#' With clusters of unequal size, "the population value" can mean the value
#' for the average *respondent* or for the average *cluster*, and under
#' informative cluster size these differ. The package will not choose for you:
#' `cluster_target` must be declared, and it is recorded on the result. See
#' [bm_cluster_info()] and `?bayesmokken-estimands`.
#'
#' @section What this package does not do:
#'
#' - It does not return posterior credible intervals. The intervals describe
#'   repeated-sampling uncertainty; the Bayesian bootstrap is the mechanism
#'   that generates them, not an assertion about a posterior.
#' - It reports no threshold probabilities. There is no `P(H > .3)` here.
#' - It performs no item selection and propagates no scale-selection
#'   uncertainty. The item set is fixed by you, before the analysis.
#' - It exposes no tuning constant. \eqn{\gamma} and \eqn{\kappa} were fixed by
#'   a frozen train-validate-lock procedure, and coverage at any other value is
#'   simply unknown. `level` is 95% and nothing else.
#' - It offers no verdict on whether a scale is any good. `bm_benchmark()`
#'   reports where an estimate sits relative to the conventional landmarks and
#'   stops there.
#'
#' `bm_guardrails()` lists the situations in which the package refuses to
#' compute, and one situation in which it computes but you should know it is
#' outside the validated range.
#'
#' @section Where to start:
#'
#' | If you want to | Read |
#' |----|----|
#' | see a result in five minutes | `vignette("a1-getting-started")` |
#' | analyze a binary scale | `vignette("a2-binary-srs-workflow")` |
#' | analyze clustered polytomous data | `vignette("a3-clustered-polytomous-workflow")` |
#' | put results in a paper | `vignette("a4-reporting-and-exporting")` |
#' | make the figures | `vignette("a5-visualization-cookbook")` |
#' | know what is being estimated | `?bayesmokken-estimands` |
#' | know what the interval means | `?bayesmokken-intervals` |
#' | look up a term | `?bayesmokken-glossary` |
#'
#' @author JoonHo Lee \email{jlee296@@ua.edu}
#'   (\href{https://orcid.org/0009-0006-4019-8703}{ORCID}),
#'   The University of Alabama.
#'
#' @seealso [mokken_h()], [bb_mokken_h()], [bm_check_data()],
#'   [bm_plot_draws()], `citation("bayesmokken")`
#'
#' @keywords internal
"_PACKAGE"


#' What this package estimates
#'
#' @description
#' The coefficient this package computes is a functional of a response
#' distribution, and which distribution that is has to be decided before any
#' arithmetic happens. This page sets out those decisions, because they are the
#' part of a Mokken analysis most often left implicit.
#'
#' @section The coefficient:
#' For a pair of items, \eqn{H_{ij}} is the covariance divided by the largest
#' covariance those two items could have at their observed margins. For an item
#' and for the scale, the coefficient is a **ratio of sums of covariances**,
#' not an average of pair coefficients:
#'
#' \deqn{H = \frac{\sum_{i<j} \mathrm{Cov}(Y_i, Y_j)}{\sum_{i<j}
#'   \mathrm{Cov}^{\max}(Y_i, Y_j)}}
#'
#' The distinction matters. Averaging pair coefficients gives a different
#' number, and it is not the quantity the Mokken literature reports.
#'
#' @section The item set is fixed:
#' The estimand is defined for the item set **you** supply. The package does
#' not search over item sets, and the interval it returns contains no
#' allowance for the possibility that you might have chosen different items.
#' If your items were selected by looking at these data, the interval
#' understates your uncertainty, and no software can repair that after the
#' fact.
#'
#' @section The row target is complete cases:
#' Respondents with any missing response on the fixed item set are excluded,
#' and the estimand is the coefficient of the distribution of those who
#' remain. That is a choice about *which population* is being described, not a
#' technicality: if missingness is related to the trait, the complete-case
#' population differs from the one you had in mind. The count and the fraction
#' excluded are on every result, and [bm_check_data()] reports them before you
#' fit.
#'
#' @section Two targets under clustering:
#' When respondents are nested in clusters of unequal size, two population
#' quantities present themselves:
#'
#' - **respondent-weighted**: every respondent counts equally, so a cluster of
#'   30 contributes three times as much as a cluster of 10.
#' - **equal-cluster**: every cluster counts equally, whatever its size.
#'
#' The two values can differ whenever cluster size is associated with any part
#' of the response distribution, including covariance or spread rather than
#' only mean level. Reporting one while meaning the other is a substantive
#' error. `cluster_target` is therefore required rather than defaulted quietly.
#' [bm_cluster_info()] describes cluster imbalance, target weights, and the
#' narrow size--mean-score association; it cannot diagnose agreement between
#' the two H values. Assess target sensitivity by fitting the same declared
#' scale under both targets and comparing the deterministic coefficients.
#'
#' @section Boundaries:
#' The denominator can be zero or nearly zero, in which case the coefficient
#' is undefined or unstable. This is a property of the data, not a failure of
#' the software, so the package reports a typed status rather than silently
#' substituting something. [bm_regularity()] shows which items and pairs sit
#' near the boundary.
#'
#' @name bayesmokken-estimands
#' @seealso [bayesmokken-intervals], [bm_cluster_info()], [bm_regularity()]
#' @family bayesmokken-concepts
NULL


#' What the intervals mean, and what they do not
#'
#' @description
#' The interval this package returns is a **repeated-sampling confidence
#' interval**. The Bayesian bootstrap is the machinery that produces it. That
#' combination confuses people often enough to deserve its own page.
#'
#' @section Binary first-order rationale:
#' For a fixed number of binary items and independent complete-scale
#' respondents, drawing respondent weights from a Dirichlet distribution and
#' recomputing H produces a conditional distribution that agrees to first
#' order with the estimator's sampling distribution on a regular marginal-order
#' cell with a positive denominator. This is the rationale for using draw
#' quantiles as sampling-uncertainty endpoints on the binary path. It is not a
#' finite-sample coverage theorem, and it does not license a posterior reading.
#'
#' @section Clustered empirical warrant:
#' The clustered path draws one Dirichlet\eqn{(1,\ldots,1)} vector over
#' clusters and, independently within every cluster, another
#' Dirichlet\eqn{(1,\ldots,1)} vector over respondents. No conditional
#' first-order matching theorem is claimed for that hierarchical law. Its
#' warrant is the target-exact weight construction, numerical diagnostics, and
#' prespecified held-out simulation confirmation. That confirmation covered
#' polytomous designs with cluster counts \eqn{G \in \{22,35,55,80\}}; it does
#' not establish universal coverage for other support structures or cluster
#' counts.
#'
#' @section What the number is not:
#'
#' - **Not a posterior credible interval.** No prior over \eqn{H} is
#'   specified, and none is implied.
#' - **Not a posterior probability.** The package computes no `P(H > .3)`,
#'   deliberately: an earlier development version did, and the feature was
#'   removed because the validated method does not support it.
#' - **Not a predictive distribution.** The draws describe uncertainty about
#'   the coefficient of the population you sampled, not about future responses.
#'
#' @section The width correction:
#' The raw equal-tailed bootstrap interval is not left alone. Its width is
#' multiplied by a fixed factor:
#'
#' \deqn{[m - c\,w,\; m + c\,w], \qquad
#'   c = 1 + \frac{0.25}{\sqrt{n}} \ \text{(binary)}, \qquad
#'   c = 1 - \frac{1.5}{\sqrt{G}} \ \text{(clustered)}}
#'
#' where \eqn{m} is the midpoint of the raw interval and \eqn{w} its
#' half-width. The midpoint never moves, endpoints are never clipped, and no
#' alternative method is ever substituted for a difficult sample. The
#' constants were fixed on training data, checked on disjoint held-out
#' conditions, and then locked; they are not arguments. For the clustered
#' correction, the confirmed cluster-count envelope is 22--80, and smaller
#' cluster counts are refused by the calibrated public path.
#'
#' Both the raw and the corrected endpoints are on every result, and both are
#' drawn by [bm_plot_draws()], so you can always see exactly what the
#' correction did.
#'
#' @section Only the scale coefficient is calibrated:
#' The correction was developed and validated for the **scale-level**
#' coefficient. Item- and pair-level draws are available and useful, but their
#' coverage has never been checked. The package reports them as raw quantiles,
#' labels them `uncalibrated`, and draws them dashed and faded. Treat them as
#' a description of relative precision across items, not as intervals of the
#' same standing as the scale interval.
#'
#' @section How many draws:
#' The frozen default is 2,000. Whether that is enough for your data is an
#' empirical question about Monte Carlo error, which
#' [bm_mc_diagnostics()] and [bm_plot_mc_stability()] answer.
#'
#' @name bayesmokken-intervals
#' @seealso [bayesmokken-estimands], [bb_mokken_h()], [bm_plot_draws()],
#'   [bm_plot_calibration()]
#' @family bayesmokken-concepts
NULL


#' Vocabulary
#'
#' @description
#' Terms used throughout the package's output and documentation.
#'
#' @section Terms:
#'
#' \describe{
#'   \item{complete-scale respondent}{A respondent with no missing response on
#'     the fixed item set. Only these contribute; the others are counted and
#'     reported, not silently dropped.}
#'   \item{support}{The finite, increasing set of scores a polytomous item can
#'     take. Declared, not inferred, because the maximum covariance in the
#'     denominator is computed at the declared support: a category the
#'     instrument allows but nobody chose still belongs there. Build it with
#'     [bm_supports()].}
#'   \item{score map}{The mapping from raw recorded values to scores, used for
#'     character or factor responses, for collapsing categories, and for
#'     reverse keying. Build it with [bm_score_map()].}
#'   \item{maximum covariance}{The largest covariance two items could have at
#'     their observed margins. For binary items it is
#'     \eqn{\min(p_i, p_j) - p_i p_j}; for polytomous items it is obtained by
#'     comonotone transport over the declared supports.}
#'   \item{knot}{A point at which the transport path computing the maximum
#'     covariance changes direction. An empirical diagnostic of that
#'     computation, reported by [bm_knot_diagnostics()]; not a quantity to
#'     interpret substantively.}
#'   \item{denominator status}{`positive`, `near_zero` or `zero`. Anything but
#'     `positive` means the coefficient is unstable or undefined there.}
#'   \item{weight law}{How random weights are generated: `iid_respondent` for
#'     the binary path, `hierarchical_cluster` for the two-stage clustered
#'     path.}
#'   \item{cluster target}{`respondent` or `equal_cluster`. See
#'     `?bayesmokken-estimands`.}
#'   \item{method identifier}{A frozen string such as
#'     `P7-FS-WIDTH-RAW-G025-v1` recording exactly which procedure produced an
#'     interval. It travels with the result, so an archived object is
#'     self-describing.}
#'   \item{defined draws}{Bootstrap draws for which the coefficient is finite.
#'     Undefined draws are retained in the stored arrays and counted, never
#'     quietly discarded.}
#'   \item{uncalibrated}{Marks an interval to which the validated width
#'     correction was not applied, because it was validated only for the scale
#'     coefficient. Applies to every item- and pair-level interval.}
#' }
#'
#' @name bayesmokken-glossary
#' @seealso [bayesmokken-estimands], [bayesmokken-intervals], [bm_guardrails()]
#' @family bayesmokken-concepts
NULL
