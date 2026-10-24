% Please edit documentation in R/extract.R
\name{bm_draws}
\alias{bm_draws}
\title{Extract the bootstrap draws}
\usage{
bm_draws(x, what = c("scale", "item", "pair"), format = c("wide", "long"))
}
\arguments{
\item{x}{A \code{bm_bb_h} object.}

\item{what}{\code{"scale"}, \code{"item"} or \code{"pair"}.}

\item{format}{\code{"wide"} returns the stored numeric vector or matrix;
\code{"long"} returns a data frame with \code{draw_id}, \code{level}, \code{target}, and
\code{value}. Pair-level long output also carries explicit \code{item_i} and
\code{item_j} columns.}
}
\value{
A numeric vector, a matrix, or a data frame, depending on \code{format}.
Undefined draws are kept as \code{NA} rather than dropped, so draw indices stay
comparable across levels. For pair draws, \code{target} is an opaque,
collision-safe compatibility key; use \code{item_i} and \code{item_j} as the
human-readable pair identity and do not parse \code{target}.
}
\description{
Returns the raw Bayesian-bootstrap draws, either as the stored matrix or in
long form for plotting and summarizing.
}
\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
summary(bm_draws(fit))
head(bm_draws(fit, what = "item", format = "long"))

}
\seealso{
\code{\link[=bm_plot_draws]{bm_plot_draws()}}, \code{\link[=bm_mc_diagnostics]{bm_mc_diagnostics()}}

Other bayesmokken-extract:
\code{\link[=as.data.frame.bm_bb_h]{as.data.frame.bm_bb_h()}},
\code{\link[=bm_interval]{bm_interval()}},
\code{\link[=bm_item_h]{bm_item_h()}},
\code{\link[=bm_pair_h]{bm_pair_h()}},
\code{\link[=bm_provenance]{bm_provenance()}},
\code{\link[=bm_scale_h]{bm_scale_h()}},
\code{\link[=tidy.bm_bb_h]{tidy.bm_bb_h()}}
}
\concept{bayesmokken-extract}
