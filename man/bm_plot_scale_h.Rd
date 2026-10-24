% Please edit documentation in R/plots-interval.R
\name{bm_plot_scale_h}
\alias{bm_plot_scale_h}
\title{Plot the scale coefficient and its interval}
\usage{
bm_plot_scale_h(
  x,
  benchmarks = c(0.3, 0.4, 0.5),
  show_raw = TRUE,
  caption = NULL
)
}
\arguments{
\item{x}{A \code{bm_bb_h} or \code{bm_empirical_h} object, or a named list of them to
compare.}

\item{benchmarks}{Numeric vector of reference values, or \code{NULL} for none.}

\item{show_raw}{Draw the uncorrected percentile interval beneath the
reported one. Keeping this \code{TRUE} is what makes the construction visible.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
Draws the scale coefficient as a point with its reported interval, the
uncorrected percentile interval beneath it, and the conventional benchmarks
as reference rules. Several fits can be shown together.
}
\examples{
fit <- bb_mokken_h(srs, draws = 200, seed = 1)
bm_plot_scale_h(fit)

\donttest{
raw <- bb_mokken_h(srs, draws = 200, seed = 1, interval = "equal_tail")
bm_plot_scale_h(list(calibrated = fit, "equal tail" = raw))
}

}
\seealso{
\code{\link[=bm_plot_draws]{bm_plot_draws()}}, \code{\link[=bm_benchmark]{bm_benchmark()}}

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
\code{\link[=bm_plot_pair_h]{bm_plot_pair_h()}},
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
