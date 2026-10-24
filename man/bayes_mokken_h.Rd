% Please edit documentation in R/bb-mokken-h.R
\name{bayes_mokken_h}
\alias{bayes_mokken_h}
\title{Deprecated posterior-oriented alias}
\usage{
bayes_mokken_h(
  data,
  items = colnames(data),
  score_map = NULL,
  missing_codes = NULL,
  row_ids = NULL,
  missing = "complete_scale",
  draws = 2000L,
  seed = 1L,
  batch_size = 250L,
  workers = 1L,
  threshold = 0.3,
  item_threshold = 0.3,
  pair_threshold = 0,
  interval = c("calibrated", "equal_tail"),
  level = 0.95,
  zero_tol = 64 * .Machine$double.eps,
  eta = sqrt(.Machine$double.eps),
  warn = TRUE,
  item_type = c("auto", "binary", "ordinal"),
  supports = NULL,
  cluster = NULL,
  cluster_target = c("respondent", "equal_cluster"),
  weight_law = c("auto", "iid_respondent", "complete_record", "hierarchical_cluster")
)
}
\arguments{
\item{data}{A matrix or data frame with at least two respondent rows and two
uniquely named item columns.}

\item{items}{Character vector naming the fixed item set. Defaults to every
column, which is what you want only if \code{data} contains items and nothing
else.}

\item{score_map}{Named list mapping raw values to scores, one element per
item that needs one. Required for character, factor and logical columns,
and for reverse keying. Build it with \code{\link[=bm_score_map]{bm_score_map()}}.}

\item{missing_codes}{Named list of raw values to treat as missing before
scoring, for example \code{list(item1 = c(-9, 99))}. Build it with
\code{\link[=bm_missing_codes]{bm_missing_codes()}}.}

\item{row_ids}{Optional unique row identifiers, used to report which
respondents were excluded. Defaults to row names, then to input position.}

\item{missing}{Missing-data target. Only \code{"complete_scale"} is supported;
the argument exists so the choice is visible in the call rather than
implicit.}

\item{draws}{Number of Bayesian-bootstrap draws. The frozen default is
2,000; the clustered article's application and confirmation used 9,999.
Raising it reduces Monte Carlo error in the endpoints and costs time
linearly; \code{\link[=bm_plot_mc_stability]{bm_plot_mc_stability()}} shows whether it is worth it. A
minimum fraction must come out finite -- \code{max(100, ceiling(0.90 * draws))}
on the binary path, \code{max(20, ceiling(0.99 * draws))} on the clustered one
-- and a shortfall produces a typed interval failure rather than an
interval computed from whatever survived.}

\item{seed}{Nonnegative integer seed for the L'Ecuyer-CMRG draw streams.
Fractional values are rejected rather than truncated, because \code{1.1} and
\code{1L} would otherwise silently give the same answer. Always set it. On the
default clustered lane, the caller must use a built-in RNG and must not
use Box-Muller normals, whose hidden cache cannot be restored. Select
Inversion normals before calling. On this lane the stream is the clustered
article's draw-major
mapping, so the archive's seeds reproduce its draws.}

\item{batch_size}{Draws evaluated per processing batch. Affects memory and
nothing else: changing it does not change which draws are taken.}

\item{workers}{Requested worker count. Recorded for provenance; the
reference engine keeps the worker-invariant seed map and currently runs
serially, so results do not depend on this.}

\item{threshold}{Deprecated and ignored.}

\item{item_threshold}{Deprecated and ignored.}

\item{pair_threshold}{Deprecated and ignored.}

\item{interval}{\code{"calibrated"} for the validated construction of the lane,
\code{"equal_tail"} for the uncorrected percentile comparator. The comparator
exists so you can see what the construction did; it is not an alternative
recommendation.}

\item{level}{Confidence level. Must be exactly \code{0.95}.}

\item{zero_tol}{Numerical tolerance below which a denominator counts as
zero. The default is 64 machine epsilons. Change it only with a reason.}

\item{eta}{Margin below which a denominator counts as near zero rather than
positive. Governs when the status field warns rather than the value being
wrong.}

\item{warn}{If \code{TRUE}, emit one structured warning when any reported
denominator is zero or near zero. The statuses are on the result either
way; this only controls whether you are told out loud.}

\item{item_type}{\code{"auto"}, \code{"binary"} or \code{"ordinal"}. \code{"auto"} resolves to
\code{"ordinal"} whenever \code{supports} or \code{cluster} is supplied, and to
\code{"binary"} otherwise. See the decision table in \code{\link[=bb_mokken_h]{bb_mokken_h()}}.}

\item{supports}{Required for every ordinal call: a named list giving each
selected item's finite increasing score support. Every observed value must
appear in it, and unobserved categories the instrument allows should appear
too, because the maximum covariance is computed at the declared support.
Build it with \code{\link[=bm_supports]{bm_supports()}}.}

\item{cluster}{Cluster labels aligned with the original input rows, for
respondents nested in classrooms, clinics and the like. Filtered only
after complete-scale rows are identified, so it must have one entry per
input row, not per retained row.}

\item{cluster_target}{\code{"respondent"} or \code{"equal_cluster"}. These are
different population quantities whenever cluster size is associated with
any aspect of the response distribution, not merely its mean level. This
argument must therefore be declared whenever \code{cluster} is supplied. The
choice is yours to make and to report. See
\code{\link[=bm_cluster_info]{bm_cluster_info()}}.}

\item{weight_law}{Weight-law declaration: \code{"auto"}, \code{"iid_respondent"},
\code{"complete_record"} or \code{"hierarchical_cluster"}. With \code{cluster}, \code{"auto"}
resolves to \code{"complete_record"}, the complete-record Bayesian bootstrap
of the clustered article's current version; \code{"hierarchical_cluster"} is
the two-stage law of version 0.9.0, kept only for reproducing earlier
results and warned about by \code{\link[=bb_mokken_h]{bb_mokken_h()}}. The deterministic
coefficient does not depend on the law; a clustered \code{mokken_h()} call
records the resolved law as provenance.}
}
\value{
A \code{bm_bb_h} object.
}
\description{
\code{bayes_mokken_h()} is retained temporarily for call compatibility. It
returns the same \code{bm_bb_h} object as \code{bb_mokken_h()} and does not compute
threshold probabilities. The threshold arguments are ignored.
}
\seealso{
\code{\link[=bb_mokken_h]{bb_mokken_h()}}, which this forwards to.

Other bayesmokken-estimation:
\code{\link[=bb_mokken_h]{bb_mokken_h()}},
\code{\link[=mokken_h]{mokken_h()}}
}
\concept{bayesmokken-estimation}
