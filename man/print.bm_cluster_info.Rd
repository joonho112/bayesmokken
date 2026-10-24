% Please edit documentation in R/print-methods.R
\name{print.bm_cluster_info}
\alias{print.bm_cluster_info}
\title{Print a cluster summary}
\usage{
\method{print}{bm_cluster_info}(x, ...)
}
\arguments{
\item{x}{A \code{bm_cluster_info} object from \code{\link[=bm_cluster_info]{bm_cluster_info()}}.}

\item{...}{Ignored.}
}
\value{
\code{x}, invisibly.
}
\description{
Print a cluster summary
}
\examples{
bm_cluster_info(classroom_wb$class_id,
                data = classroom_wb[, paste0("wb", 1:6)])

}
\seealso{
Other bayesmokken-console:
\code{\link[=format_bm_fit_cli]{format_bm_fit_cli()}},
\code{\link[=format_bm_interval_cli]{format_bm_interval_cli()}},
\code{\link[=format_bm_items_cli]{format_bm_items_cli()}},
\code{\link[=print.bm_bb_h]{print.bm_bb_h()}},
\code{\link[=print.bm_benchmark]{print.bm_benchmark()}},
\code{\link[=print.bm_data_check]{print.bm_data_check()}},
\code{\link[=print.bm_diagnostic_report]{print.bm_diagnostic_report()}},
\code{\link[=print.bm_empirical_h]{print.bm_empirical_h()}},
\code{\link[=print.bm_h_interval]{print.bm_h_interval()}},
\code{\link[=print.bm_report]{print.bm_report()}},
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}},
\code{\link[=summary.bm_empirical_h]{summary.bm_empirical_h()}}
}
\concept{bayesmokken-console}
