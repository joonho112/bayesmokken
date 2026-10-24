% Please edit documentation in R/prepare.R
\name{bm_check_data}
\alias{bm_check_data}
\title{Screen a dataset before fitting}
\usage{
bm_check_data(
  data,
  items = NULL,
  cluster = NULL,
  item_type = c("auto", "binary", "ordinal"),
  score_map = NULL,
  missing_codes = NULL,
  cluster_target = NULL
)
}
\arguments{
\item{data}{A matrix or data frame of item responses.}

\item{items}{Character vector of item columns. Defaults to every column
except \code{cluster} if \code{cluster} was given by name.}

\item{cluster}{Optional cluster labels, either a vector as long as \code{nrow}
or the name of a column in \code{data}.}

\item{item_type}{\code{"auto"} (the default) infers binary or ordinal from the
observed values; \code{"binary"} or \code{"ordinal"} force the reading.}

\item{score_map, missing_codes}{Optional named lists, as passed to
\code{\link[=mokken_h]{mokken_h()}}. Supplying them makes the screen reflect the scored values
rather than the raw ones.}

\item{cluster_target}{Optional clustered population target, either
\code{"respondent"} or \code{"equal_cluster"}. Supplying it allows the suggested
clustered call to use that exact value. When it is omitted, the suggestion
contains a quoted choice marker and is explicitly labeled non-runnable;
the package does not select a target.}
}
\value{
An object of class \code{bm_data_check} with elements \code{items} (a data
frame, one row per item), \code{rows} (counts), \code{clusters} (a
\code{\link[=bm_cluster_info]{bm_cluster_info()}} object or \code{NULL}), \code{flags} (a data frame of issues
worth reading), \code{next_call} (a parseable suggested command),
\code{next_call_runnable}, \code{next_call_reason}, \code{next_call_method}, and
\code{call_expressions}, which records the captured \code{data}, \code{items}, and
\code{cluster} expressions.
}
\description{
Reports what the data look like from the package's point of view: item type,
observed values, complete-scale loss, constant items, items that may need
reverse keying, and cluster structure. Run it once before \code{mokken_h()} or
\code{bb_mokken_h()} on data you have not analyzed before.
}
\details{
Nothing is modified and nothing is fitted. The result records a suggested
next call built from the expressions supplied by the caller and the complete
selected item vector. \code{next_call_runnable} is \code{TRUE} only when that call has
no unresolved scientific choice. In particular, a clustered call is not
labeled runnable until \code{cluster_target} has been supplied explicitly.
}
\section{Reverse keying is reported, never applied}{

An item whose rest-score correlation is negative is flagged, because that is
what a reverse-keyed item looks like. It is not corrected. Automatic
reversal would silently double-reverse data that arrived already scored,
which is the more common situation with published instruments. To reverse an
item, say so: \code{bm_score_map(data, reverse = "item3")}.
}

\examples{
bm_check_data(srs)

bm_check_data(classroom_wb, items = paste0("wb", 1:6),
              cluster = "class_id", cluster_target = "respondent")

}
\seealso{
\code{\link[=bm_supports]{bm_supports()}}, \code{\link[=bm_score_map]{bm_score_map()}}, \code{\link[=bm_cluster_info]{bm_cluster_info()}}

Other bayesmokken-prepare:
\code{\link[=bm_cluster_info]{bm_cluster_info()}},
\code{\link[=bm_missing_codes]{bm_missing_codes()}},
\code{\link[=bm_score_map]{bm_score_map()}},
\code{\link[=bm_supports]{bm_supports()}}
}
\concept{bayesmokken-prepare}
