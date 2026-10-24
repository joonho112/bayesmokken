% Please edit documentation in R/data.R
\docType{data}
\name{srs}
\alias{srs}
\title{Scientific Reasoning Scale responses (Italian sample)}
\format{
A data frame with 337 rows and 11 integer columns, each 0 or 1.
Row names are the original respondent identifiers.
\describe{
\item{SRS1}{First item of the Scientific Reasoning Scale, 0 or 1.}
\item{SRS2, SRS3, SRS4, SRS5, SRS6}{Items 2 to 6, 0 or 1.}
\item{SRS7, SRS8, SRS9, SRS10, SRS11}{Items 7 to 11, 0 or 1.}
}
}
\source{
Caliciuri, R., & Lanz, M. (2024). \emph{Psychometric properties of the Scientific
Reasoning Scale: Application to the Italian context.} OSF project
\url{https://osf.io/jk9dp/}, \doi{10.17605/OSF.IO/JK9DP}.
Distributed under CC BY 4.0 and redistributed here with attribution; see
\code{system.file("extdata", "DATA-LICENSES.md", package = "bayesmokken")}.

The instrument is due to Drummond, C., & Fischhoff, B. (2017). Development
and validation of the Scientific Reasoning Scale. \emph{Journal of Behavioral
Decision Making}, 30(1), 26-38. \doi{10.1002/bdm.1906}.

Retrieved through the Item Response Warehouse and rebuilt by
\code{data-raw/srs.R}, which verifies the source file against a recorded
SHA-256 digest before use.
}
\usage{
srs
}
\description{
Responses of 337 Italian adults to the 11 dichotomous items of the
Scientific Reasoning Scale. This is the dataset used in the empirical
application of the binary method implemented here, and it is the fastest way
to see the package working on real data.
}
\details{
The scale is scored so that 1 marks the answer keyed as scientifically
correct. Items are stored in the fixed order \code{SRS1} through \code{SRS11}; that
order is part of the analysis contract of the published application, which
forbids item deletion, reversal and reselection, so treat the eleven columns
as one fixed scale rather than a pool to select from.
}
\section{Sampling caveats}{

The 337 rows are a complete-case subset of 606 consent-eligible
respondents, recruited by snowball sampling. That is a nonprobability
design, so descriptive statements about a wider Italian population are not
supported by it. The package's intervals describe sampling uncertainty
under a working independent-respondent assumption; no recruiter or network
identifier is available, so clustering induced by snowball recruitment
cannot be adjusted for.
}

\examples{
data(srs)
dim(srs)

# The deterministic coefficient
mokken_h(srs)

# With its interval (raise `draws` for real work)
bb_mokken_h(srs, draws = 200, seed = 1)

}
\seealso{
\link{classroom_wb} for a clustered polytomous example.

Other bayesmokken-data:
\code{\link[=bm_simulate]{bm_simulate()}},
\code{\link{classroom_wb}}
}
\concept{bayesmokken-data}
\keyword{datasets}
