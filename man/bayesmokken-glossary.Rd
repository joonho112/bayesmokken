% Please edit documentation in R/package-doc.R
\name{bayesmokken-glossary}
\alias{bayesmokken-glossary}
\title{Vocabulary}
\description{
Terms used throughout the package's output and documentation.
}
\section{Terms}{


\describe{
\item{complete-scale respondent}{A respondent with no missing response on
the fixed item set. Only these contribute; the others are counted and
reported, not silently dropped.}
\item{support}{The finite, increasing set of scores a polytomous item can
take. Declared, not inferred, because the maximum covariance in the
denominator is computed at the declared support: a category the
instrument allows but nobody chose still belongs there. Build it with
\code{\link[=bm_supports]{bm_supports()}}.}
\item{score map}{The mapping from raw recorded values to scores, used for
character or factor responses, for collapsing categories, and for
reverse keying. Build it with \code{\link[=bm_score_map]{bm_score_map()}}.}
\item{maximum covariance}{The largest covariance two items could have at
their observed margins. For binary items it is
\eqn{\min(p_i, p_j) - p_i p_j}; for polytomous items it is obtained by
comonotone transport over the declared supports.}
\item{knot}{A point at which the transport path computing the maximum
covariance changes direction. An empirical diagnostic of that
computation, reported by \code{\link[=bm_knot_diagnostics]{bm_knot_diagnostics()}}; not a quantity to
interpret substantively.}
\item{denominator status}{\code{positive}, \code{near_zero} or \code{zero}. Anything but
\code{positive} means the coefficient is unstable or undefined there.}
\item{weight law}{How random weights are generated: \code{iid_respondent} for
the binary path, \code{complete_record} for the default clustered path (one
Dirichlet weight per cluster record), \code{hierarchical_cluster} for the
historical two-stage clustered path.}
\item{complete record}{A cluster together with every complete-scale
response in it. The default clustered law treats these records as the
independent sampling units and never perturbs the weights inside one.}
\item{cluster target}{\code{respondent} or \code{equal_cluster}. See
\code{?bayesmokken-estimands}.}
\item{reflected endpoints}{On the default clustered path, the quantiles of
the draws reflected about the point estimate and expanded by
\eqn{k_G}, before the intersection with \eqn{(-\infty, 1]}. Kept on the
result as \code{reflected_lower} and \code{reflected_upper}.}
\item{method identifier}{A frozen string such as
\code{P7-FS-WIDTH-RAW-G025-v1} or \code{V5-C2-CRBB-ADJUSTED-RESP-v1} recording
exactly which procedure produced an interval. It travels with the
result, so an archived object is self-describing.}
\item{defined draws}{Bootstrap draws for which the coefficient is finite.
Undefined draws are retained in the stored arrays and counted, never
quietly discarded.}
\item{uncalibrated}{Marks an interval to which the validated construction
was not applied, because it was validated only for the scale
coefficient. Applies to every item- and pair-level interval.}
}
}

\seealso{
\link{bayesmokken-estimands}, \link{bayesmokken-intervals}, \code{\link[=bm_guardrails]{bm_guardrails()}}

Other bayesmokken-concepts:
\code{\link{bayesmokken-estimands}},
\code{\link{bayesmokken-intervals}}
}
\concept{bayesmokken-concepts}
