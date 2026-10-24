% Please edit documentation in R/theme.R
\name{theme_bayesmokken}
\alias{theme_bayesmokken}
\title{The bayesmokken theme}
\usage{
theme_bayesmokken(base_size = 11, base_family = "")
}
\arguments{
\item{base_size}{Base font size in points.}

\item{base_family}{Base font family. The default lets the device decide,
which keeps figures reproducible across machines.}
}
\value{
A ggplot2 theme object, to add to a plot with \code{+}.
}
\description{
A quiet theme: no panel border, horizontal grid lines only, titles aligned
to the plot rather than the panel, and a small left-aligned caption, because
every figure in this package carries its method identifier and its
interpretation there. Default package captions wrap to the current graphics
device width. Code that constructs a plot before opening its output device
can set \code{options(bayesmokken.figure_width = 5)} (in inches); when neither an
option nor an active device supplies a width, wrapping uses a conservative
5-inch fallback.
}
\examples{
library(ggplot2)
ggplot(data.frame(x = 1:5, y = (1:5)^2), aes(x, y)) +
  geom_line(color = bm_palette(1)) +
  labs(title = "A quiet theme", caption = "Captions sit bottom left.") +
  theme_bayesmokken()

}
\seealso{
\code{\link[=bm_palette]{bm_palette()}}, \code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}}

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
\code{\link[=scale_colour_bayesmokken]{scale_colour_bayesmokken()}}
}
\concept{bayesmokken-plots}
