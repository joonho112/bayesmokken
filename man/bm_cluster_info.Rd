% Please edit documentation in R/prepare.R
\name{bm_cluster_info}
\alias{bm_cluster_info}
\title{Summarize cluster structure}
\usage{
bm_cluster_info(cluster, data = NULL, items = NULL)
}
\arguments{
\item{cluster}{Cluster labels, one per respondent.}

\item{data}{Optional item data, used to compute each cluster's mean total
score and hence the size--mean-score Spearman statistic.}

\item{items}{Optional item columns of \code{data}; defaults to every column.}
}
\value{
An object of class \code{bm_cluster_info} with elements \code{G} (number of
clusters), \code{sizes}, \code{table} (one row per cluster, including the weight
each target gives it), \code{size_mean_score_spearman}, the deprecated alias
\code{informative_index}, and \code{singletons}.
}
\description{
Describes the clusters respondents are nested in: how many there are, how
unequal they are, the weight each population target gives them, and the
narrow association between cluster size and cluster mean total score.
}
\section{The size--mean-score statistic}{

This is the Spearman correlation between cluster size and the cluster's mean
total score. It is only a descriptive location diagnostic. Mokken H also
depends on covariance and the full response distribution, so a value near
zero does not show that respondent-weighted and equal-cluster H agree, and a
large value does not quantify their difference. To assess target sensitivity,
fit the same declared scale under both targets and compare the two
deterministic H values directly.
}

\examples{
info <- bm_cluster_info(classroom_wb$class_id,
                        data = classroom_wb[, paste0("wb", 1:6)])
info
head(info$table)

}
\seealso{
\code{\link[=bm_plot_clusters]{bm_plot_clusters()}}, \code{vignette("m4-hierarchical-bb-targets", package = "bayesmokken")}

Other bayesmokken-prepare:
\code{\link[=bm_check_data]{bm_check_data()}},
\code{\link[=bm_missing_codes]{bm_missing_codes()}},
\code{\link[=bm_score_map]{bm_score_map()}},
\code{\link[=bm_supports]{bm_supports()}}
}
\concept{bayesmokken-prepare}
