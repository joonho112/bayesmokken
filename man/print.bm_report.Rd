% Please edit documentation in R/print-methods.R
\name{print.bm_report}
\alias{print.bm_report}
\title{Print a results table}
\usage{
\method{print}{bm_report}(x, ...)
}
\arguments{
\item{x}{A \code{bm_report} object from \code{\link[=bm_report]{bm_report()}}.}

\item{...}{Ignored.}
}
\value{
\code{x}, invisibly.
}
\description{
Print a results table
}
\examples{
bm_report(bb_mokken_h(srs, draws = 200, seed = 1))

}
\seealso{
Other bayesmokken-console:
\code{\link[=format_bm_fit_cli]{format_bm_fit_cli()}},
\code{\link[=format_bm_interval_cli]{format_bm_interval_cli()}},
\code{\link[=format_bm_items_cli]{format_bm_items_cli()}},
\code{\link[=print.bm_bb_h]{print.bm_bb_h()}},
\code{\link[=print.bm_benchmark]{print.bm_benchmark()}},
\code{\link[=print.bm_cluster_info]{print.bm_cluster_info()}},
\code{\link[=print.bm_data_check]{print.bm_data_check()}},
\code{\link[=print.bm_diagnostic_report]{print.bm_diagnostic_report()}},
\code{\link[=print.bm_empirical_h]{print.bm_empirical_h()}},
\code{\link[=print.bm_h_interval]{print.bm_h_interval()}},
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}},
\code{\link[=summary.bm_empirical_h]{summary.bm_empirical_h()}}
}
\concept{bayesmokken-console}
