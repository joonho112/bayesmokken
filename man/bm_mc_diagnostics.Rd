% Please edit documentation in R/mc-diagnostics.R
\name{bm_mc_diagnostics}
\alias{bm_mc_diagnostics}
\title{Diagnose Monte Carlo error in interval endpoints}
\usage{
bm_mc_diagnostics(object, level = 0.95, batches = 4L, ...)
}
\arguments{
\item{object}{A completed \code{bm_bb_h} object returned by \code{\link[=bb_mokken_h]{bb_mokken_h()}}.}

\item{level}{Confidence level used for the endpoint quantiles. It must be
exactly \code{0.95}, matching the package's frozen interval contract.}

\item{batches}{Number of nonoverlapping batches. Must be one finite integer
of at least 2. If fewer finite draws than batches are available, the
function returns the explicit \code{"TOO_FEW_DEFINED_DRAWS"} state rather than
silently reducing the requested batch count.}

\item{...}{Must be empty. It is accepted only so misspelled arguments can
produce a typed \code{bm_input_error} instead of an unclassified base error.}
}
\value{
A named list with the same fields and field types in every state:
\itemize{
\item \code{defined_draws}, \code{total_draws}: integer draw counts.
\item \code{defined_fraction}, \code{level}: numeric scalars.
\item \code{lower_endpoint_mcse}, \code{upper_endpoint_mcse}: numeric endpoint Monte Carlo
standard errors, or \code{NA_real_} when they cannot be computed.
\item \code{interval_batch_max_deviation}: numeric maximum absolute difference
between a batch endpoint and its corresponding full-draw endpoint.
\item \code{prefix_half_endpoint_max_difference}: numeric maximum absolute endpoint
difference between the first half and all finite draws; \code{NA_real_} when
fewer than four finite draws are available.
\item \code{batches}: the requested integer batch count. It is never silently
reduced.
\item \code{quantile_type}: integer \code{8}, the quantile convention used throughout the
package.
\item \code{method_id}, \code{contract}, \code{status}: character scalars. \code{status} is one of
\code{"OK"}, \code{"OK_WITH_UNDEFINED_DRAWS"}, \code{"ALL_DRAWS_UNDEFINED"}, or
\code{"TOO_FEW_DEFINED_DRAWS"}.
}
}
\description{
Estimates the numerical variability of the reported interval endpoints by
dividing the finite scale-level bootstrap draws into nonoverlapping batches.
It also compares the endpoints from the first half of the finite draws with
the endpoints from all finite draws. These quantities diagnose whether the
requested number of draws was enough for the endpoints to settle; they do
not validate interval calibration or provide threshold evidence.
}
\section{What the diagnostics describe}{

The batch and prefix endpoints are rebuilt with the construction the
reported interval used: midpoint-and-half-width scaling on the binary and
the historical clustered paths, and reflection about the point estimate
with the expansion by \eqn{k_G} on the default clustered path, where the
diagnostics describe the endpoints before their intersection with
\eqn{(-\infty, 1]}.
}

\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
bm_mc_diagnostics(fit)

}
\seealso{
\code{\link[=bm_diagnostics]{bm_diagnostics()}}, \code{\link[=bm_plot_mc_stability]{bm_plot_mc_stability()}}

Other bayesmokken-diagnostics:
\code{\link[=bm_diagnostics]{bm_diagnostics()}},
\code{\link[=bm_guardrails]{bm_guardrails()}},
\code{\link[=bm_knot_diagnostics]{bm_knot_diagnostics()}},
\code{\link[=bm_regularity]{bm_regularity()}}
}
\concept{bayesmokken-diagnostics}
