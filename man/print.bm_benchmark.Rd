% Please edit documentation in R/print-methods.R
\name{print.bm_benchmark}
\alias{print.bm_benchmark}
\title{Print a benchmark comparison}
\usage{
\method{print}{bm_benchmark}(x, ...)
}
\arguments{
\item{x}{A \code{bm_benchmark} object from \code{\link[=bm_benchmark]{bm_benchmark()}}.}

\item{...}{Ignored.}
}
\value{
\code{x}, invisibly.
}
\description{
Print a benchmark comparison
}
\examples{
bm_benchmark(bb_mokken_h(srs, draws = 200, seed = 1))

}
\seealso{
Other bayesmokken-console:
\code{\link[=format_bm_fit_cli]{format_bm_fit_cli()}},
\code{\link[=format_bm_interval_cli]{format_bm_interval_cli()}},
\code{\link[=format_bm_items_cli]{format_bm_items_cli()}},
\code{\link[=print.bm_bb_h]{print.bm_bb_h()}},
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
