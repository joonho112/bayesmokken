% Please edit documentation in R/prepare.R
\name{bm_score_map}
\alias{bm_score_map}
\title{Build a score map}
\usage{
bm_score_map(data, items = NULL, reverse = NULL, recode = NULL, to = NULL)
}
\arguments{
\item{data}{A matrix or data frame of item responses.}

\item{items}{Character vector of item columns; defaults to every column.}

\item{reverse}{Character vector of items to reverse. An item's scores are
mirrored within its own observed range, so \code{1:5} becomes \code{5:1}.}

\item{recode}{Optional named list of explicit maps, one per item, each a
named vector whose names are raw values and whose values are scores. Takes
precedence over \code{reverse} for the same item.}

\item{to}{Optional target support for reversal, as a numeric vector or a
named list per item. Defaults to the item's own observed values, which is
right when the observed range covers the instrument's range and wrong when
it does not -- pass \code{to} in that case.}
}
\value{
A named list of named numeric vectors, ready to pass as \code{score_map}.
Items you did not mention are omitted, which is what the estimation
functions expect.
}
\description{
Produces the \code{score_map} argument of \code{\link[=mokken_h]{mokken_h()}} and \code{\link[=bb_mokken_h]{bb_mokken_h()}}: a
named list that says, for each item, which raw value becomes which score.
Use it to reverse an item, to collapse categories, or to score character and
factor responses.
}
\section{Do not reverse twice}{

Published datasets frequently arrive already scored, with reverse-keyed
items flipped by whoever prepared them. \code{mokken::SWMDK} is one such case:
its documentation lists three reverse-keyed items, and its stored values
already reflect them. Reversing again would corrupt the analysis while
producing perfectly plausible-looking numbers. Check the data's
documentation before using \code{reverse}.
}

\examples{
# Reverse one item of a five-point scale
map <- bm_score_map(classroom_wb, items = paste0("wb", 1:6),
                    reverse = "wb3")
map$wb3

# Score character responses explicitly
d <- data.frame(q1 = c("no", "yes", "yes"), q2 = c("yes", "no", "yes"))
bm_score_map(d, recode = list(q1 = c(no = 0, yes = 1),
                              q2 = c(no = 0, yes = 1)))

}
\seealso{
\code{\link[=bm_supports]{bm_supports()}}, \code{\link[=bm_check_data]{bm_check_data()}}

Other bayesmokken-prepare:
\code{\link[=bm_check_data]{bm_check_data()}},
\code{\link[=bm_cluster_info]{bm_cluster_info()}},
\code{\link[=bm_missing_codes]{bm_missing_codes()}},
\code{\link[=bm_supports]{bm_supports()}}
}
\concept{bayesmokken-prepare}
