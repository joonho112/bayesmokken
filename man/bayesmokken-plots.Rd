% Please edit documentation in R/theme.R
\name{bayesmokken-plots}
\alias{bayesmokken-plots}
\title{bayesmokken plotting internals}
\description{
Import anchor for the figure layer. The plotting code uses many ggplot2
verbs, so the whole namespace is imported; data-frame columns are reached
through the \code{rlang} \code{.data} pronoun inside \code{aes()} to avoid the R CMD check
note about undefined global variables.
}
\keyword{internal}
