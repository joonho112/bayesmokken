% Please edit documentation in R/plots-method.R
\name{bm_plot_mc_stability}
\alias{bm_plot_mc_stability}
\title{Plot Monte Carlo settling of the interval endpoints}
\usage{
bm_plot_mc_stability(x, points = 40, caption = NULL)
}
\arguments{
\item{x}{A \code{bm_bb_h} object.}

\item{points}{Number of prefix lengths to evaluate.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
Shows how the interval endpoints move as more bootstrap draws are used. A
series that has flattened well before the right edge means the draw count is
generous; one still moving means more draws are worth taking.
}
\examples{
bm_plot_mc_stability(bb_mokken_h(srs, draws = 500, seed = 1))

}
\seealso{
\code{\link[=bm_mc_diagnostics]{bm_mc_diagnostics()}}, \code{\link[=bm_plot_draws]{bm_plot_draws()}}

Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_alt_text]{bm_alt_text()}},
\code{\link[=bm_palette]{bm_palette()}},
\code{\link[=bm_plot_calibration]{bm_plot_calibration()}},
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}},
\code{\link[=bm_plot_draws]{bm_plot_draws()}},
\code{\link[=bm_plot_item_h]{bm_plot_item_h()}},
\code{\link[=bm_plot_margins]{bm_plot_margins()}},
\code{\link[=bm_plot_overview]{bm_plot_overview()}},
\code{\link[=bm_plot_pair_h]{bm_plot_pair_h()}},
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}},
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
