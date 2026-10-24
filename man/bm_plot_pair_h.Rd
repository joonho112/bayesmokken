% Please edit documentation in R/plots-structure.R
\name{bm_plot_pair_h}
\alias{bm_plot_pair_h}
\title{Plot the pair coefficients}
\usage{
bm_plot_pair_h(
  x,
  layout = c("lower", "full"),
  show_values = NULL,
  caption = NULL
)
}
\arguments{
\item{x}{A \code{bm_bb_h} or \code{bm_empirical_h} object.}

\item{layout}{\code{"lower"} for the triangle, \code{"full"} for the whole square.}

\item{show_values}{Print the coefficient in each cell. \code{NULL} decides by
the number of items.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
A lower-triangular heatmap of the pair coefficients, using the same item
order as \code{\link[=bm_plot_item_h]{bm_plot_item_h()}} so the two figures can be read together.
}
\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
bm_plot_pair_h(fit)

}
\seealso{
\code{\link[=bm_pair_h]{bm_pair_h()}}, \code{\link[=bm_plot_item_h]{bm_plot_item_h()}}

Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_alt_text]{bm_alt_text()}},
\code{\link[=bm_palette]{bm_palette()}},
\code{\link[=bm_plot_calibration]{bm_plot_calibration()}},
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}},
\code{\link[=bm_plot_draws]{bm_plot_draws()}},
\code{\link[=bm_plot_item_h]{bm_plot_item_h()}},
\code{\link[=bm_plot_margins]{bm_plot_margins()}},
\code{\link[=bm_plot_mc_stability]{bm_plot_mc_stability()}},
\code{\link[=bm_plot_overview]{bm_plot_overview()}},
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}},
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
