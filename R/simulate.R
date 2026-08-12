# =============================================================================
# Teaching simulator
#
# Generates monotone-homogeneity-style item responses so that every example,
# test and vignette in the package has data to run on without depending on an
# external dataset.
#
# This is NOT the simulation engine of either article. Those designs -- four
# data-generating families at 108,000 replications, and a 24-cell clustered
# confirmation -- live in the research code base and are deliberately not
# shipped here. Do not use bm_simulate() to make claims about coverage.
# =============================================================================

#' Simulate item responses for examples and teaching
#'
#' Draws binary or polytomous item responses from a monotone
#' homogeneity-style model, optionally with respondents nested in clusters.
#' Use it to try the package out, to build reproducible examples, and to see
#' how the two clustered targets separate when cluster size carries
#' information about cluster level.
#'
#' The latent trait is standard normal. With `clusters` supplied it is split
#' into a cluster part and a within-cluster part whose variances are `icc` and
#' `1 - icc`, so the intraclass correlation of the latent trait is `icc` by
#' construction (the ICC of the observed total score is somewhat lower).
#' Binary items follow a two-parameter logistic model, and polytomous items a
#' graded-response model with evenly spaced thresholds.
#'
#' @section What this function is not:
#' This is a teaching generator. The simulation evidence behind the package's
#' calibrated intervals comes from prespecified designs described in the two
#' articles, which are not implemented here. Coverage claims cannot be
#' reproduced or contested with this function.
#'
#' @param n Number of respondents. With `clusters` supplied this is the target
#'   total; the realized total can differ by a few respondents because cluster
#'   sizes are drawn at random.
#' @param n_items Number of items.
#' @param type `"binary"` for 0/1 items, `"ordinal"` for graded items.
#' @param n_categories Number of response categories for ordinal items, scored
#'   `1:n_categories`. Ignored when `type = "binary"`.
#' @param discrimination Common item slope. Larger values give a stronger
#'   scale and a larger `H`.
#' @param difficulty Optional numeric vector of `n_items` item locations. The
#'   default spreads them evenly over `[-1, 1]`.
#' @param clusters Number of clusters, or `NULL` for independent respondents.
#' @param icc Intraclass correlation of the latent trait, in `[0, 1)`. Ignored
#'   when `clusters` is `NULL`.
#' @param informative_size If `TRUE`, cluster size is tied to the cluster's
#'   latent mean *and* to its within-cluster spread, so that the
#'   respondent-weighted and equal-cluster targets differ. Tying size to
#'   location alone is not enough: `H` is a ratio of covariance sums, so
#'   moving a cluster up or down leaves its internal covariances almost
#'   unchanged. Spread is what moves the coefficient. This is the situation
#'   Paper B is about.
#' @param seed Optional integer seed. Supply one whenever the result is used in
#'   a test, an example or a vignette.
#'
#' @return A data frame with `n_items` item columns named `item1`, `item2`, and
#'   so on, plus a leading `cluster` column when `clusters` is supplied.
#'   `attr(x, "truth")` holds the generating parameters and the latent trait.
#'
#' @examples
#' # A small binary scale
#' d <- bm_simulate(n = 200, n_items = 6, seed = 1)
#' mokken_h(d)
#'
#' # A clustered polytomous scale whose two targets disagree
#' cd <- bm_simulate(n = 400, n_items = 5, type = "ordinal", n_categories = 5,
#'                   clusters = 25, icc = 0.15, informative_size = TRUE,
#'                   seed = 2)
#' str(attr(cd, "truth")[c("icc", "informative_size")])
#'
#' @seealso [classroom_wb] for a stored clustered example, [srs] for real data.
#' @family bayesmokken-data
#' @export
bm_simulate <- function(n = 300L,
                        n_items = 6L,
                        type = c("binary", "ordinal"),
                        n_categories = 5L,
                        discrimination = 1.6,
                        difficulty = NULL,
                        clusters = NULL,
                        icc = 0,
                        informative_size = FALSE,
                        seed = NULL) {
  type <- match.arg(type)
  .bm_sim_check_count(n, "n", min = 4L)
  .bm_sim_check_count(n_items, "n_items", min = 2L)
  if (!is.numeric(discrimination) || length(discrimination) != 1L ||
      !is.finite(discrimination) || discrimination <= 0) {
    .bm_abort("`discrimination` must be one positive finite number.")
  }
  if (identical(type, "ordinal")) {
    .bm_sim_check_count(n_categories, "n_categories", min = 3L)
  }
  if (!is.logical(informative_size) || length(informative_size) != 1L ||
      is.na(informative_size)) {
    .bm_abort("`informative_size` must be one nonmissing logical value.")
  }
  if (is.null(difficulty)) {
    difficulty <- seq(-1, 1, length.out = n_items)
  } else if (!is.numeric(difficulty) || length(difficulty) != n_items ||
             anyNA(difficulty) || any(!is.finite(difficulty))) {
    .bm_abort("`difficulty` must be `n_items` finite numeric values.")
  }
  clustered <- !is.null(clusters)
  if (clustered) {
    .bm_sim_check_count(clusters, "clusters", min = 2L)
    if (!is.numeric(icc) || length(icc) != 1L || !is.finite(icc) ||
        icc < 0 || icc >= 1) {
      .bm_abort("`icc` must be one number in [0, 1).")
    }
    if (clusters * 2 > n) {
      .bm_abort("`n` must allow at least two respondents per cluster.")
    }
  }

  restore <- .bm_sim_seed(seed)
  on.exit(restore(), add = TRUE)

  # --- latent trait ---------------------------------------------------------
  if (clustered) {
    g <- as.integer(clusters)
    cluster_mean <- stats::rnorm(g, sd = sqrt(icc))
    sizes <- .bm_sim_sizes(n, g, cluster_mean, informative_size)
    # Informative cluster size acts on the within-cluster SPREAD as well as on
    # the cluster mean. Location alone barely moves H, because H is a ratio of
    # covariance sums and shifting a cluster up or down leaves its internal
    # covariances almost untouched. Tying size to spread is what actually
    # separates the respondent-weighted and equal-cluster targets.
    spread <- if (isTRUE(informative_size)) {
      0.65 + 0.70 * (rank(sizes, ties.method = "first") - 1) / max(1, g - 1)
    } else {
      rep(1, g)
    }
    cluster_id <- rep(seq_len(g), times = sizes)
    n_real <- length(cluster_id)
    theta <- cluster_mean[cluster_id] +
      stats::rnorm(n_real, sd = sqrt(1 - icc) * spread[cluster_id])
  } else {
    g <- NA_integer_
    sizes <- NULL
    cluster_id <- NULL
    n_real <- as.integer(n)
    theta <- stats::rnorm(n_real)
  }

  # --- responses ------------------------------------------------------------
  if (identical(type, "binary")) {
    p <- stats::plogis(discrimination * outer(theta, difficulty, "-"))
    y <- matrix(stats::rbinom(length(p), 1L, as.vector(p)), nrow = n_real)
    storage.mode(y) <- "integer"
    thresholds <- NULL
  } else {
    k <- as.integer(n_categories)
    thresholds <- seq(-1.4, 1.4, length.out = k - 1L)
    y <- matrix(1L, nrow = n_real, ncol = n_items)
    for (j in seq_len(n_items)) {
      eta <- discrimination * (theta - difficulty[j])
      cum <- stats::plogis(outer(eta, thresholds, "-"))
      u <- matrix(stats::runif(n_real * (k - 1L)), nrow = n_real)
      y[, j] <- 1L + as.integer(rowSums(cum > u))
    }
  }
  colnames(y) <- paste0("item", seq_len(n_items))

  out <- as.data.frame(y)
  if (clustered) {
    out <- cbind(
      cluster = factor(sprintf("c%02d", cluster_id)),
      out, stringsAsFactors = FALSE
    )
  }

  attr(out, "truth") <- list(
    generator = "bm_simulate",
    note = paste(
      "Teaching generator. Not the simulation engine of either article;",
      "coverage claims cannot be evaluated with it."
    ),
    type = type, n_items = n_items,
    n_categories = if (identical(type, "ordinal")) as.integer(n_categories) else 2L,
    discrimination = discrimination, difficulty = difficulty,
    thresholds = thresholds, theta = theta,
    clusters = g, cluster_sizes = sizes,
    cluster_spread = if (clustered) spread else NULL,
    icc = if (clustered) icc else 0,
    informative_size = informative_size, seed = seed
  )
  out
}

# --- helpers -----------------------------------------------------------------

.bm_sim_check_count <- function(x, name, min) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x != as.integer(x) || x < min) {
    .bm_abort(sprintf("`%s` must be one integer of at least %d.", name, min))
  }
  invisible(TRUE)
}

# Reproducibility without disturbing the caller's stream: save .Random.seed,
# set it if a seed was given, and restore on exit.
.bm_sim_seed <- function(seed) {
  if (is.null(seed)) return(function() invisible(NULL))
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed != as.integer(seed)) {
    .bm_abort("`seed` must be one integer, or NULL.")
  }
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  old_kind <- RNGkind()
  set.seed(as.integer(seed), kind = "Mersenne-Twister",
           normal.kind = "Inversion", sample.kind = "Rejection")
  function() {
    do.call(RNGkind, as.list(old_kind))
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
    invisible(NULL)
  }
}

# Cluster sizes summing to about `n`. When informative_size is TRUE the sizes
# are attached to clusters in ascending order of the cluster mean, so that
# larger clusters sit higher on the latent trait and the respondent-weighted
# and equal-cluster targets separate.
.bm_sim_sizes <- function(n, g, cluster_mean, informative_size) {
  base <- n / g
  raw <- stats::runif(g, 0.55, 1.45) * base
  sizes <- pmax(2L, as.integer(round(raw)))
  if (isTRUE(informative_size)) {
    sizes <- sort(sizes)[rank(cluster_mean, ties.method = "first")]
  }
  sizes
}
