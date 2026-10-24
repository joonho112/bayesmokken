% Please edit documentation in R/plots-interval.R
\name{bm_alt_text}
\alias{bm_alt_text}
\title{Alternative text for a bayesmokken figure}
\usage{
bm_alt_text(p)
}
\arguments{
\item{p}{A ggplot produced by one of the \verb{bm_plot_*()} functions.}
}
\value{
A single string, or \code{NA} if the plot did not come from this package.
}
\description{
Returns the description the package generated for a figure, for use as
\code{fig.alt} in a report or \code{alt} on a web page.
}
\examples{
bm_alt_text(bm_plot_scale_h(bb_mokken_h(srs, draws = 200, seed = 1)))

}
\seealso{
Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_palette]{bm_palette()}},
\code{\link[=bm_plot_calibration]{bm_plot_calibration()}},
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}},
\code{\link[=bm_plot_draws]{bm_plot_draws()}},
\code{\link[=bm_plot_item_h]{bm_plot_item_h()}},
\code{\link[=bm_plot_margins]{bm_plot_margins()}},
\code{\link[=bm_plot_mc_stability]{bm_plot_mc_stability()}},
\code{\link[=bm_plot_overview]{bm_plot_overview()}},
\code{\link[=bm_plot_pair_h]{bm_plot_pair_h()}},
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}},
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
