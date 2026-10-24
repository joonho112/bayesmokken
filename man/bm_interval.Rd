% Please edit documentation in R/extract.R
\name{bm_interval}
\alias{bm_interval}
\title{Extract the typed interval}
\usage{
bm_interval(x)
}
\arguments{
\item{x}{A \code{bm_bb_h} object.}
}
\value{
A one-row data frame containing the scalar fields of the typed
interval. Binary intervals have 21 common fields. Clustered intervals add
the cluster count, target, validation scope and status, confirmed
cluster-count range, and the construction; on the default complete-record
lane they further add the reflected endpoints before the intersection
with \eqn{(-\infty, 1]}, the intersection, empty-set and zero-width
flags, the boundary counts, and the archive method identifier.
}
\description{
Flattens the \code{bm_h_interval} object into a one-row data frame, so an
archived result describes itself without the fitted object beside it.
}
\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
t(bm_interval(fit))

}
\seealso{
\code{\link[=bm_scale_h]{bm_scale_h()}}, \code{\link[=bm_provenance]{bm_provenance()}}

Other bayesmokken-extract:
\code{\link[=as.data.frame.bm_bb_h]{as.data.frame.bm_bb_h()}},
\code{\link[=bm_draws]{bm_draws()}},
\code{\link[=bm_item_h]{bm_item_h()}},
\code{\link[=bm_pair_h]{bm_pair_h()}},
\code{\link[=bm_provenance]{bm_provenance()}},
\code{\link[=bm_scale_h]{bm_scale_h()}},
\code{\link[=tidy.bm_bb_h]{tidy.bm_bb_h()}}
}
\concept{bayesmokken-extract}
