% Please edit documentation in R/plots-diagnostic.R
\name{bm_plot_margins}
\alias{bm_plot_margins}
\title{Plot item margins and denominator headroom}
\usage{
bm_plot_margins(x, type = c("marginal", "headroom", "gap"), caption = NULL)
}
\arguments{
\item{x}{A \code{bm_bb_h} or \code{bm_empirical_h} object.}

\item{type}{\code{"marginal"} for item response distributions or \code{"headroom"}
for the smallest pair maximum covariance involving each item. \code{"gap"} is
a deprecated alias for \code{"headroom"}.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
Shows binary score-one shares or stacked ordinal category shares. The
headroom view instead shows the smallest pair maximum covariance involving
each item, the quantity that approaches zero at a denominator boundary.
}
\examples{
fit <- mokken_h(srs)
bm_plot_margins(fit)
bm_plot_margins(fit, type = "headroom")

}
\seealso{
\code{\link[=bm_regularity]{bm_regularity()}}, \code{\link[=bm_diagnostics]{bm_diagnostics()}}

Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_alt_text]{bm_alt_text()}},
\code{\link[=bm_palette]{bm_palette()}},
\code{\link[=bm_plot_calibration]{bm_plot_calibration()}},
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}},
\code{\link[=bm_plot_draws]{bm_plot_draws()}},
\code{\link[=bm_plot_item_h]{bm_plot_item_h()}},
\code{\link[=bm_plot_mc_stability]{bm_plot_mc_stability()}},
\code{\link[=bm_plot_overview]{bm_plot_overview()}},
\code{\link[=bm_plot_pair_h]{bm_plot_pair_h()}},
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}},
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
