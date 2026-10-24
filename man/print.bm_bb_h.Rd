% Please edit documentation in R/print-bb-h.R
\name{print.bm_bb_h}
\alias{print.bm_bb_h}
\title{Print a Bayesian-bootstrap Mokken fit}
\usage{
\method{print}{bm_bb_h}(x, digits = 3L, ...)
}
\arguments{
\item{x}{A \code{bm_bb_h} object from \code{\link[=bb_mokken_h]{bb_mokken_h()}}.}

\item{digits}{Number of digits for coefficients.}

\item{...}{Ignored.}
}
\value{
\code{x}, invisibly.
}
\description{
Shows the estimand, the frozen method identifier, the sample, and the scale
coefficient with both its raw percentile interval and the reported
interval.
}
\examples{
bb_mokken_h(srs, draws = 200, seed = 1)

}
\seealso{
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}}, \code{\link[=bm_scale_h]{bm_scale_h()}}, \code{\link[=bm_plot_draws]{bm_plot_draws()}}

Other bayesmokken-console:
\code{\link[=format_bm_fit_cli]{format_bm_fit_cli()}},
\code{\link[=format_bm_interval_cli]{format_bm_interval_cli()}},
\code{\link[=format_bm_items_cli]{format_bm_items_cli()}},
\code{\link[=print.bm_benchmark]{print.bm_benchmark()}},
\code{\link[=print.bm_cluster_info]{print.bm_cluster_info()}},
\code{\link[=print.bm_data_check]{print.bm_data_check()}},
\code{\link[=print.bm_diagnostic_report]{print.bm_diagnostic_report()}},
\code{\link[=print.bm_empirical_h]{print.bm_empirical_h()}},
\code{\link[=print.bm_h_interval]{print.bm_h_interval()}},
\code{\link[=print.bm_report]{print.bm_report()}},
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}},
\code{\link[=summary.bm_empirical_h]{summary.bm_empirical_h()}}
}
\concept{bayesmokken-console}
