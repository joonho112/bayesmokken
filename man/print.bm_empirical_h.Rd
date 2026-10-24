% Please edit documentation in R/print-empirical-h.R
\name{print.bm_empirical_h}
\alias{print.bm_empirical_h}
\title{Print a deterministic Mokken fit}
\usage{
\method{print}{bm_empirical_h}(x, digits = 3L, ...)
}
\arguments{
\item{x}{A \code{bm_empirical_h} object from \code{\link[=mokken_h]{mokken_h()}}.}

\item{digits}{Number of digits for coefficients.}

\item{...}{Ignored.}
}
\value{
\code{x}, invisibly.
}
\description{
Shows what was estimated, how many respondents contributed, the scale
coefficient, and the extreme items. The full item table is in \code{summary()}.
}
\examples{
mokken_h(srs)

}
\seealso{
\code{\link[=summary.bm_empirical_h]{summary.bm_empirical_h()}}, \code{\link[=bm_item_h]{bm_item_h()}}, \code{\link[=bb_mokken_h]{bb_mokken_h()}}

Other bayesmokken-console:
\code{\link[=format_bm_fit_cli]{format_bm_fit_cli()}},
\code{\link[=format_bm_interval_cli]{format_bm_interval_cli()}},
\code{\link[=format_bm_items_cli]{format_bm_items_cli()}},
\code{\link[=print.bm_bb_h]{print.bm_bb_h()}},
\code{\link[=print.bm_benchmark]{print.bm_benchmark()}},
\code{\link[=print.bm_cluster_info]{print.bm_cluster_info()}},
\code{\link[=print.bm_data_check]{print.bm_data_check()}},
\code{\link[=print.bm_diagnostic_report]{print.bm_diagnostic_report()}},
\code{\link[=print.bm_h_interval]{print.bm_h_interval()}},
\code{\link[=print.bm_report]{print.bm_report()}},
\code{\link[=summary.bm_bb_h]{summary.bm_bb_h()}},
\code{\link[=summary.bm_empirical_h]{summary.bm_empirical_h()}}
}
\concept{bayesmokken-console}
