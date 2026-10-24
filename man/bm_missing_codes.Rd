% Please edit documentation in R/prepare.R
\name{bm_missing_codes}
\alias{bm_missing_codes}
\title{Declare values that mean missing}
\usage{
bm_missing_codes(items, codes)
}
\arguments{
\item{items}{Character vector of item names.}

\item{codes}{Numeric or character vector of values to treat as missing,
applied to every item, or a named list giving codes per item.}
}
\value{
A named list, ready to pass as \code{missing_codes}.
}
\description{
A small helper that builds the \code{missing_codes} argument: a named list saying
which raw values should be treated as missing before scoring.
}
\examples{
bm_missing_codes(paste0("SRS", 1:3), codes = c(-9, 99))

}
\seealso{
Other bayesmokken-prepare:
\code{\link[=bm_check_data]{bm_check_data()}},
\code{\link[=bm_cluster_info]{bm_cluster_info()}},
\code{\link[=bm_score_map]{bm_score_map()}},
\code{\link[=bm_supports]{bm_supports()}}
}
\concept{bayesmokken-prepare}
