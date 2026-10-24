% Please edit documentation in R/prepare.R
\name{bm_supports}
\alias{bm_supports}
\title{Build the score supports for polytomous items}
\usage{
bm_supports(
  data,
  items = NULL,
  from = c("observed", "range"),
  extend = NULL,
  score_map = NULL
)
}
\arguments{
\item{data}{A matrix or data frame of item responses.}

\item{items}{Character vector of item columns; defaults to every column.}

\item{from}{\code{"observed"} (the default) uses exactly the values that occur;
\code{"range"} fills in the integer grid from the smallest to the largest
observed value, which is usually what you want when a middle category
happens to be empty.}

\item{extend}{Optional numeric vector of extra scores to add to every item,
or a named list giving extra scores per item. Use it for categories the
instrument allows but nobody chose.}

\item{score_map}{Optional score map, as passed to \code{\link[=mokken_h]{mokken_h()}}. When given,
supports are built from the mapped values.}
}
\value{
A named list of numeric vectors, one per item, each sorted and
without duplicates, ready to pass as \code{supports}.
}
\description{
Produces the \code{supports} argument that \code{\link[=mokken_h]{mokken_h()}} and \code{\link[=bb_mokken_h]{bb_mokken_h()}}
require for ordinal items: a named list giving, for each item, the finite
increasing set of scores that item can take.
}
\details{
Declaring the support is not bureaucracy. The maximum covariance in the
denominator of a polytomous coefficient is computed at the declared support,
so a category that exists in the instrument but happens to be unobserved in
your sample still belongs in the support. That is what \code{extend} is for.
}
\examples{
items <- paste0("wb", 1:6)
sup <- bm_supports(classroom_wb, items = items)
str(sup[1:2])

# A five-point instrument where nobody used category 5
d <- data.frame(a = c(1, 2, 3, 4), b = c(2, 2, 3, 4))
bm_supports(d, from = "range", extend = 5)

}
\seealso{
\code{\link[=bm_check_data]{bm_check_data()}}, \code{\link[=bm_score_map]{bm_score_map()}}

Other bayesmokken-prepare:
\code{\link[=bm_check_data]{bm_check_data()}},
\code{\link[=bm_cluster_info]{bm_cluster_info()}},
\code{\link[=bm_missing_codes]{bm_missing_codes()}},
\code{\link[=bm_score_map]{bm_score_map()}}
}
\concept{bayesmokken-prepare}
