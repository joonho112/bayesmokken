% Please edit documentation in R/plots-interval.R
\name{bm_plot_draws}
\alias{bm_plot_draws}
\title{Plot the bootstrap draws with the interval endpoints}
\usage{
bm_plot_draws(
  x,
  type = c("density", "histogram"),
  show_interval = TRUE,
  bins = 40,
  rug = FALSE,
  caption = NULL
)
}
\arguments{
\item{x}{A \code{bm_bb_h} object.}

\item{type}{\code{"density"} or \code{"histogram"}.}

\item{show_interval}{Draw the endpoint rules.}

\item{bins}{Number of histogram bins.}

\item{rug}{Draw a rug of the individual draws.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
Shows the distribution of the Bayesian-bootstrap draws of the scale
coefficient, with the uncorrected quantiles and the reported endpoints
drawn over it. This is the picture of what the method does: reweight, take
quantiles, apply the frozen construction (a width correction on the binary
path; reflection about the point estimate and the expansion by \eqn{k_G}
on the default clustered path).
}
\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
bm_plot_draws(fit)

\donttest{
bm_plot_draws(fit, type = "histogram", rug = TRUE)
}

}
\seealso{
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}}, \code{\link[=bm_mc_diagnostics]{bm_mc_diagnostics()}}

Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_alt_text]{bm_alt_text()}},
\code{\link[=bm_palette]{bm_palette()}},
\code{\link[=bm_plot_calibration]{bm_plot_calibration()}},
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}},
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
