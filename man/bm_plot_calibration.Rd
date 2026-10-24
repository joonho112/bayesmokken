% Please edit documentation in R/plots-method.R
\name{bm_plot_calibration}
\alias{bm_plot_calibration}
\title{Plot the interval multiplier}
\usage{
bm_plot_calibration(
  x = NULL,
  mode = c("binary", "clustered", "clustered_two_stage"),
  range = NULL,
  caption = NULL
)
}
\arguments{
\item{x}{Optional fit. When supplied, its sample size or cluster count is
marked on the curve and \code{mode} is taken from it.}

\item{mode}{\code{"binary"}, \code{"clustered"} (the default complete-record
adjustment) or \code{"clustered_two_stage"} (the historical two-stage
shrinkage). Ignored when \code{x} is supplied.}

\item{range}{Numeric length-2 range for the horizontal axis.}

\item{caption}{Caption text; \code{NULL} builds the default, \code{NA} removes it.}
}
\value{
A ggplot object.
}
\description{
Draws the frozen multiplier against sample size, for the binary width
correction, or against the number of clusters, for the clustered
adjustment, and marks where a given analysis sits on that curve.
}
\details{
Three curves exist. The binary interval is widened about its midpoint by
\eqn{1 + 0.25/\sqrt{n}}, because the uncorrected bootstrap interval was
measured to be slightly too narrow. The default clustered interval reflects
the percentile quantiles about the point estimate and expands the reflected
errors by \eqn{k_G = \sqrt{(G+1)/(G-1)}\, t_{G-1,.975}/z_{.975}}, a
prespecified function of the cluster count that is above one and tends to
one as \eqn{G} grows. The historical two-stage interval of version 0.9.0 was
narrowed about its midpoint by \eqn{1 - 1.5/\sqrt{G}}, because the
uncorrected two-stage interval was measured to be too wide; that curve is
kept under \code{mode = "clustered_two_stage"} for reproducing earlier results.
}
\section{Cluster-count validation}{

The part of a clustered curve inside the confirmed cluster-count range is
solid, and the same formula is drawn dashed outside it to identify the
region as extrapolation of the evidence, not as additional validation. For
the default complete-record adjustment the confirmed range is
\eqn{22 \le G \le 320}; the interval remains computable for \eqn{G \ge 2}
and reports \code{OUTSIDE_CONFIRMED_CLUSTER_COUNT} outside that range. For the
historical two-stage correction the range is \eqn{22 \le G \le 80}; fits
below 22 clusters are refused on that path and fits above 80 carry
\code{EXTRAPOLATED_CLUSTER_COUNT}. When \code{x} is supplied, the point uses its
recorded width multiplier and its validation status is repeated in the
caption and alternative text.
}

\examples{
# As a teaching figure, with no data at all
bm_plot_calibration(mode = "clustered")

# With an analysis marked on it
bm_plot_calibration(bb_mokken_h(srs, draws = 200, seed = 1))

}
\seealso{
\code{\link[=bm_plot_scale_h]{bm_plot_scale_h()}}, \code{vignette("m2-bayesian-bootstrap-calibration", package = "bayesmokken")},
\code{vignette("m4-hierarchical-bb-targets", package = "bayesmokken")}

Other bayesmokken-plots:
\code{\link[=autoplot.bm_bb_h]{autoplot.bm_bb_h()}},
\code{\link[=bm_alt_text]{bm_alt_text()}},
\code{\link[=bm_palette]{bm_palette()}},
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
