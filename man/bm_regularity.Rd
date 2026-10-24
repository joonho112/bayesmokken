% Please edit documentation in R/diagnostics.R
\name{bm_regularity}
\alias{bm_regularity}
\title{Inspect item margins and denominator headroom}
\usage{
bm_regularity(x)
}
\arguments{
\item{x}{A \code{bm_bb_h} or \code{bm_empirical_h} object.}
}
\value{
A data frame with one row per item: \code{item}, \code{mean_score},
\code{min_score}, \code{max_score}, \code{denominator}, \code{denominator_headroom}, the
backward-compatible alias \code{min_pair_denominator}, and \code{worst_pair}, naming
the partner item in the pair with the least headroom.
}
\description{
Reports, for each item, its target-weighted mean and declared score range,
together with the smallest pair maximum covariance involving that item.
This last quantity is denominator headroom: values near zero identify pairs
at or near the denominator boundary.
}
\examples{
bm_regularity(mokken_h(srs))

}
\seealso{
\code{\link[=bm_diagnostics]{bm_diagnostics()}}, \code{\link[=bm_plot_margins]{bm_plot_margins()}}

Other bayesmokken-diagnostics:
\code{\link[=bm_diagnostics]{bm_diagnostics()}},
\code{\link[=bm_guardrails]{bm_guardrails()}},
\code{\link[=bm_knot_diagnostics]{bm_knot_diagnostics()}},
\code{\link[=bm_mc_diagnostics]{bm_mc_diagnostics()}}
}
\concept{bayesmokken-diagnostics}
