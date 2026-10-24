% Please edit documentation in R/theme.R
\name{scale_colour_bayesmokken}
\alias{scale_colour_bayesmokken}
\alias{scale_color_bayesmokken}
\alias{scale_fill_bayesmokken}
\title{bayesmokken color and fill scales}
\usage{
scale_colour_bayesmokken(
  type = c("qualitative", "sequential", "diverging"),
  ...
)

scale_color_bayesmokken(
  type = c("qualitative", "sequential", "diverging"),
  ...
)

scale_fill_bayesmokken(type = c("qualitative", "sequential", "diverging"), ...)
}
\arguments{
\item{type}{\code{"qualitative"}, \code{"sequential"} or \code{"diverging"}.}

\item{...}{Passed to the underlying ggplot2 scale constructor, so \code{name},
\code{labels}, \code{guide} and friends all work as usual.}
}
\value{
A ggplot2 scale, to add to a plot with \code{+}.
}
\description{
Discrete and continuous scales drawn from \code{\link[=bm_palette]{bm_palette()}}. Add one to a plot
whose color or fill aesthetic should follow the package's palette.
}
\examples{
library(ggplot2)
d <- data.frame(x = 1:3, y = 1:3, g = factor(c("a", "b", "c")))
ggplot(d, aes(x, y, color = g)) +
  geom_point(size = 4) +
  scale_colour_bayesmokken() +
  theme_bayesmokken()

}
\seealso{
\code{\link[=bm_palette]{bm_palette()}}, \code{\link[=theme_bayesmokken]{theme_bayesmokken()}}

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
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}},
\code{\link[=theme_bayesmokken]{theme_bayesmokken()}}
}
\concept{bayesmokken-plots}
