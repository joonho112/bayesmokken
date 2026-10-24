% Please edit documentation in R/print-empirical-h.R
\name{summary.bm_empirical_h}
\alias{summary.bm_empirical_h}
\alias{print.summary_bm_empirical_h}
\title{Summarize a deterministic Mokken fit}
\usage{
\method{summary}{bm_empirical_h}(object, ...)

\method{print}{summary_bm_empirical_h}(x, digits = 3L, ...)
}
\arguments{
\item{object}{A \code{bm_empirical_h} object.}

\item{...}{Ignored.}

\item{x}{A \code{summary_bm_empirical_h} object.}

\item{digits}{Number of digits for coefficients.}
}
\value{
An object of class \code{summary_bm_empirical_h}.
}
\description{
Adds the full item table, the pair denominator counts and the preprocessing
hashes to what \code{print()} shows.
}
\examples{
summary(mokken_h(srs))

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
\code{\link[=print.bm_report]{print.bm_report()}},
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}}
}
\concept{bayesmokken-console}
