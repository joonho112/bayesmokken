# =============================================================================
# Extraction
#
# A fitted object already carries everything a user needs. What it lacked was a
# way to get it out without knowing that the item table lives at
# `fit$empirical$item` and is called `H_i` there.
#
# Every function here returns a plain data frame whose columns are fixed by
# inst/extdata/spec/df-schemas.yml. tests/testthat/test-extract.R reads that
# file and checks the returned frames against it, so the schema and the code
# cannot drift apart.
# =============================================================================

# Literals that also appear in printers, plot captions and help pages. Defined
# once here; the schema file carries the same strings for the tests to compare.
.bm_uncalibrated <- "uncalibrated"
.bm_uncalibrated_note <- paste(
  "Item- and pair-level intervals are uncalibrated: they are raw equal-tail",
  "Bayesian-bootstrap quantiles. The width calibration was validated for the",
  "scale coefficient only, so these should not be read as having the same",
  "standing as the scale interval."
)
.bm_interpretation_note <- paste(
  "Bayesian-bootstrap confidence interval for repeated-sampling uncertainty."
)


#' Extract the scale coefficient
#'
#' Returns the scale-level coefficient as a one-row data frame, with the
#' interval when the object has one. This is the level, and the only level, at
#' which the interval constructions of this package have been validated.
#' `raw_lower` and `raw_upper` are always the uncorrected percentile
#' quantiles; `lower` and `upper` are the reported endpoints.
#'
#' @param x A `bm_bb_h` object from [bb_mokken_h()], or a `bm_empirical_h`
#'   object from [mokken_h()]. With a deterministic fit the interval columns
#'   are `NA`.
#'
#' @return A one-row data frame: `H`, `lower`, `upper`, `level`, `raw_lower`,
#'   `raw_upper`, `width_multiplier`, `method_id`, `interval_status`,
#'   `denominator_status`, `n`, `n_items`, `n_clusters`, `cluster_target`.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_scale_h(fit)
#'
#' # Without an interval
#' bm_scale_h(mokken_h(srs))
#'
#' @family bayesmokken-extract
#' @seealso [bm_item_h()], [bm_interval()], [bm_plot_scale_h()]
#' @export
bm_scale_h <- function(x) {
  parts <- .bm_parts(x)
  emp <- parts$empirical
  itv <- parts$interval
  data.frame(
    H = emp$scale$H,
    lower = if (is.null(itv)) NA_real_ else itv$lower,
    upper = if (is.null(itv)) NA_real_ else itv$upper,
    level = if (is.null(itv)) NA_real_ else itv$level,
    raw_lower = if (is.null(itv)) NA_real_ else itv$raw_lower,
    raw_upper = if (is.null(itv)) NA_real_ else itv$raw_upper,
    width_multiplier = if (is.null(itv)) NA_real_ else itv$width_multiplier,
    method_id = if (is.null(itv)) NA_character_ else itv$method_id,
    interval_status = if (is.null(itv)) NA_character_ else itv$status,
    denominator_status = emp$scale$denominator_status,
    n = as.integer(emp$provenance$n_complete),
    n_items = nrow(emp$item),
    n_clusters = .bm_int_or_na(emp$provenance$cluster_count),
    cluster_target = .bm_chr_or_na(emp$estimand$cluster_target),
    stringsAsFactors = FALSE
  )
}


#' Extract the item coefficients
#'
#' Returns one row per item, ordered from the smallest coefficient to the
#' largest. This is a descriptive ordering of the declared fixed scale.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param include_interval If `TRUE`, add the raw equal-tail quantiles of the
#'   item draws when `x` carries them. The default is `FALSE`; item intervals
#'   are explicitly opt-in because they are uncalibrated.
#' @param interval Deprecated alias for `include_interval`. Supply only one of
#'   these arguments. The alias will be removed after the 0.9.x series.
#'
#' @return A data frame: `item`, `H`, `numerator`, `denominator`,
#'   `denominator_status`, and when intervals are added `bb_mean`, `bb_lower`,
#'   `bb_upper`, `defined_draws`, `interval_calibration`.
#'
#' @section These intervals are not calibrated:
#' The width correction this package applies was validated for the scale
#' coefficient. Item-level draws exist in the fitted object and are genuinely
#' useful for seeing which items are precisely and imprecisely estimated, but
#' their coverage has never been checked. They are therefore reported as raw
#' equal-tail quantiles and carry an `interval_calibration` column reading
#' `"uncalibrated"`. Do not present them as if they had the same standing as
#' the scale interval.
#'
#' @section Fixed-scale item inspection:
#' The ordering and optional raw intervals describe the declared item set.
#' Removing items and refitting defines a new analysis; scale-level interval
#' claims from the original fit do not carry over.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_item_h(fit)
#'
#' # First row in ascending coefficient order
#' head(bm_item_h(fit), 1)
#'
#' @family bayesmokken-extract
#' @seealso [bm_scale_h()], [bm_plot_item_h()]
#' @export
bm_item_h <- function(x, include_interval = FALSE, interval = NULL) {
  include_interval <- .bm_resolve_include_interval(
    include_interval,
    interval,
    include_interval_missing = missing(include_interval)
  )
  parts <- .bm_parts(x)
  it <- parts$empirical$item
  out <- data.frame(
    item = as.character(it$item),
    H = it$H_i,
    numerator = it$cov_sum,
    denominator = it$cov_max_sum,
    denominator_status = as.character(it$denominator_status),
    stringsAsFactors = FALSE
  )
  draws <- parts$draws$item
  if (include_interval && !is.null(draws)) {
    q <- .bm_draw_quantiles(draws, out$item)
    out$bb_mean <- q$mean
    out$bb_lower <- q$lower
    out$bb_upper <- q$upper
    out$defined_draws <- q$defined
    out$interval_calibration <- .bm_uncalibrated
  }
  out[order(out$H, out$item, method = "radix"), , drop = FALSE] -> out
  rownames(out) <- NULL
  out
}


#' Extract the pair coefficients
#'
#' Returns one row per unordered item pair, in the fixed item order.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param include_interval If `TRUE`, add raw equal-tail quantiles of the pair
#'   draws. The default is `FALSE`, because the number of pairs grows
#'   quadratically and the quantiles are rarely what you want at this level.
#' @param interval Deprecated alias for `include_interval`. Supply only one of
#'   these arguments. The alias will be removed after the 0.9.x series.
#'
#' @return A data frame: `item_i`, `item_j`, `H`, `covariance`,
#'   `max_covariance`, `denominator_status`, and with
#'   `include_interval = TRUE` also `bb_mean`, `bb_lower`, `bb_upper`, integer
#'   `defined_draws`, and `interval_calibration`.
#'
#' @section These intervals are not calibrated:
#' The same caution applies as for [bm_item_h()]: pair-level quantiles are raw,
#' their coverage was never checked, and they are labeled accordingly.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' head(bm_pair_h(fit))
#'
#' # Pairs whose denominator sits on the boundary
#' pairs <- bm_pair_h(fit)
#' pairs[pairs$denominator_status != "positive", ]
#'
#' @family bayesmokken-extract
#' @seealso [bm_item_h()], [bm_plot_pair_h()]
#' @export
bm_pair_h <- function(x, include_interval = FALSE, interval = NULL) {
  include_interval <- .bm_resolve_include_interval(
    include_interval,
    interval,
    include_interval_missing = missing(include_interval)
  )
  parts <- .bm_parts(x)
  pr <- parts$empirical$pair
  out <- data.frame(
    item_i = as.character(pr$item_i),
    item_j = as.character(pr$item_j),
    H = pr$H_ij,
    covariance = pr$cov,
    max_covariance = pr$cov_max,
    denominator_status = as.character(pr$denominator_status),
    stringsAsFactors = FALSE
  )
  if (include_interval) {
    draws <- parts$draws$pair
    if (is.null(draws)) {
      .bm_abort(
        "This object carries no pair draws, so pair intervals are unavailable.",
        hint = paste(
          "Deterministic fits have no draws. Cached fits may have had pair",
          "draws removed for size; check fit$draws$dropped_for_size."
        )
      )
    }
    aligned <- .bm_align_pair_draws(draws, out$item_i, out$item_j)
    q <- .bm_draw_quantiles(aligned$draws, aligned$key)
    out$bb_mean <- q$mean
    out$bb_lower <- q$lower
    out$bb_upper <- q$upper
    out$defined_draws <- as.integer(q$defined)
    out$interval_calibration <- .bm_uncalibrated
  }
  rownames(out) <- NULL
  out
}


#' Extract the typed interval
#'
#' Flattens the `bm_h_interval` object into a one-row data frame, so an
#' archived result describes itself without the fitted object beside it.
#'
#' @param x A `bm_bb_h` object.
#'
#' @return A one-row data frame containing the scalar fields of the typed
#'   interval. Binary intervals have 21 common fields. Clustered intervals add
#'   the cluster count, target, validation scope and status, confirmed
#'   cluster-count range, and the construction; on the default complete-record
#'   lane they further add the reflected endpoints before the intersection
#'   with \eqn{(-\infty, 1]}, the intersection, empty-set and zero-width
#'   flags, the boundary counts, and the archive method identifier.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' t(bm_interval(fit))
#'
#' @family bayesmokken-extract
#' @seealso [bm_scale_h()], [bm_provenance()]
#' @export
bm_interval <- function(x) {
  parts <- .bm_parts(x)
  itv <- parts$interval
  if (is.null(itv)) {
    .bm_abort("This object has no interval.",
              hint = "Use bb_mokken_h() rather than mokken_h().")
  }
  as.data.frame(itv[vapply(itv, function(z) length(z) == 1L, logical(1))],
                stringsAsFactors = FALSE)
}


#' Extract the bootstrap draws
#'
#' Returns the raw Bayesian-bootstrap draws, either as the stored matrix or in
#' long form for plotting and summarizing.
#'
#' @param x A `bm_bb_h` object.
#' @param what `"scale"`, `"item"` or `"pair"`.
#' @param format `"wide"` returns the stored numeric vector or matrix;
#'   `"long"` returns a data frame with `draw_id`, `level`, `target`, and
#'   `value`. Pair-level long output also carries explicit `item_i` and
#'   `item_j` columns.
#'
#' @return A numeric vector, a matrix, or a data frame, depending on `format`.
#'   Undefined draws are kept as `NA` rather than dropped, so draw indices stay
#'   comparable across levels. For pair draws, `target` is an opaque,
#'   collision-safe compatibility key; use `item_i` and `item_j` as the
#'   human-readable pair identity and do not parse `target`.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' summary(bm_draws(fit))
#' head(bm_draws(fit, what = "item", format = "long"))
#'
#' @family bayesmokken-extract
#' @seealso [bm_plot_draws()], [bm_mc_diagnostics()]
#' @export
bm_draws <- function(x,
                     what = c("scale", "item", "pair"),
                     format = c("wide", "long")) {
  what <- match.arg(what)
  format <- match.arg(format)
  parts <- .bm_parts(x)
  d <- parts$draws[[what]]
  if (is.null(d)) {
    dropped <- parts$draws$dropped_for_size
    .bm_abort(
      sprintf("This object carries no %s draws.", what),
      hint = if (!is.null(dropped)) {
        paste0("Draw levels removed for size: ", paste(dropped, collapse = ", "),
               ". Refit to obtain them.")
      } else {
        "Deterministic fits have no draws; use bb_mokken_h()."
      }
    )
  }
  if (identical(format, "wide")) return(d)
  if (identical(what, "scale")) {
    return(data.frame(draw_id = seq_along(d), level = "scale",
                      target = "scale", value = as.numeric(d),
                      stringsAsFactors = FALSE))
  }
  if (identical(what, "pair")) {
    pair <- parts$empirical$pair
    aligned <- .bm_align_pair_draws(
      d,
      as.character(pair$item_i),
      as.character(pair$item_j)
    )
    d <- aligned$draws
    return(data.frame(
      draw_id = rep(seq_len(nrow(d)), times = ncol(d)),
      level = "pair",
      target = rep(aligned$key, each = nrow(d)),
      item_i = rep(as.character(pair$item_i), each = nrow(d)),
      item_j = rep(as.character(pair$item_j), each = nrow(d)),
      value = as.numeric(d),
      stringsAsFactors = FALSE
    ))
  }
  targets <- colnames(d)
  data.frame(
    draw_id = rep(seq_len(nrow(d)), times = ncol(d)),
    level = what,
    target = rep(targets, each = nrow(d)),
    value = as.numeric(d),
    stringsAsFactors = FALSE
  )
}


#' Extract the reproducibility record
#'
#' Returns the information needed to state in a paper's supplement exactly how
#' a result was produced: the seed and generator, the draw counts, the
#' full analysis-input hash, diagnostic preprocessing hashes, and the frozen
#' method identifier. `analysis_input_hash` covers the complete scored response
#' matrix, row IDs, item order, normalized point weights, declared supports and
#' clustered labels/target where applicable.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#'
#' @return A one-row data frame. Fields that do not apply to the object are
#'   `NA`; for example a deterministic fit has no seed.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' t(bm_provenance(fit))
#'
#' @family bayesmokken-extract
#' @seealso [bm_cite()], [bm_interval()]
#' @export
bm_provenance <- function(x) {
  parts <- .bm_parts(x)
  emp <- parts$empirical
  itv <- parts$interval
  rng <- parts$rng
  data.frame(
    package_version = as.character(utils::packageVersion("bayesmokken")),
    r_version = R.version.string,
    method_id = if (is.null(itv)) NA_character_ else itv$method_id,
    estimand_version = emp$estimand$version,
    item_type = emp$estimand$item_type,
    cluster_target = .bm_chr_or_na(emp$estimand$cluster_target),
    weight_law = if (!is.null(parts$sampling$weight_law)) {
      as.character(parts$sampling$weight_law)[1L]
    } else if (!is.null(emp$provenance$weight_law)) {
      as.character(emp$provenance$weight_law)[1L]
    } else if (!is.null(emp$estimand$weight_law)) {
      as.character(emp$estimand$weight_law)[1L]
    } else if (!is.null(emp$estimand$cluster_target)) {
      # Compatibility for archived clustered empirical objects predating the
      # additive provenance field.
      "hierarchical_cluster"
    } else if (!is.null(parts$interval)) {
      "iid_respondent"          # the binary path has no `sampling` element
    } else {
      NA_character_
    },
    seed = .bm_int_or_na(rng$seed),
    rng_kind = .bm_chr_or_na(rng$rng_kind),
    total_draws = .bm_int_or_na(if (is.null(itv)) NULL else itv$total_draws),
    defined_draws = .bm_int_or_na(if (is.null(itv)) NULL else itv$defined_draws),
    n_total = as.integer(emp$provenance$n_total),
    n_complete = as.integer(emp$provenance$n_complete),
    n_excluded = as.integer(emp$provenance$n_excluded),
    analysis_input_hash = .bm_chr_or_na(emp$provenance$analysis_input_hash),
    row_hash = .bm_chr_or_na(emp$provenance$row_hash),
    item_set_hash = .bm_chr_or_na(emp$provenance$item_set_hash),
    score_map_hash = .bm_chr_or_na(emp$provenance$score_map_hash),
    support_hash = .bm_chr_or_na(emp$provenance$support_hash),
    cluster_id_hash = .bm_chr_or_na(emp$provenance$cluster_id_hash),
    stringsAsFactors = FALSE
  )
}


#' Coerce a fit to a data frame
#'
#' Returns the item table, which is what people usually mean by "the results".
#' For the other levels use [bm_scale_h()], [bm_pair_h()] or [bm_draws()].
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param row.names,optional Ignored; present for compatibility with the
#'   generic.
#' @param ... Passed to [bm_item_h()].
#'
#' @return A data frame, as returned by [bm_item_h()].
#'
#' @examples
#' as.data.frame(mokken_h(srs))
#'
#' @family bayesmokken-extract
#' @export
as.data.frame.bm_bb_h <- function(x, row.names = NULL, optional = FALSE, ...) {
  bm_item_h(x, ...)
}

#' @rdname as.data.frame.bm_bb_h
#' @export
as.data.frame.bm_empirical_h <- function(x, row.names = NULL,
                                         optional = FALSE, ...) {
  bm_item_h(x, ...)
}


#' Tidy a fit for the broom ecosystem
#'
#' `tidy()` returns one row per item; `glance()` returns one row for the fit as
#' a whole. They are thin wrappers over [bm_item_h()] and [bm_scale_h()], so
#' the `interval_calibration` column travels with them.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param ... Passed on to the underlying extractor.
#'
#' @return A data frame.
#'
#' @examplesIf requireNamespace("generics", quietly = TRUE)
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' generics::tidy(fit)
#' generics::glance(fit)
#'
#' @family bayesmokken-extract
#' @importFrom generics tidy glance
#' @export
tidy.bm_bb_h <- function(x, ...) bm_item_h(x, ...)

#' @rdname tidy.bm_bb_h
#' @export
tidy.bm_empirical_h <- function(x, ...) bm_item_h(x, ...)

#' @rdname tidy.bm_bb_h
#' @export
glance.bm_bb_h <- function(x, ...) bm_scale_h(x, ...)

#' @rdname tidy.bm_bb_h
#' @export
glance.bm_empirical_h <- function(x, ...) bm_scale_h(x, ...)


# --- internals ---------------------------------------------------------------

.bm_resolve_include_interval <- function(
    include_interval, interval, include_interval_missing) {
  if (!is.null(interval)) {
    if (!isTRUE(include_interval_missing)) {
      .bm_abort(
        "Supply only one of `include_interval` and the deprecated `interval` alias."
      )
    }
    .Deprecated(
      new = "include_interval",
      package = "bayesmokken",
      msg = "`interval` is deprecated; use `include_interval` instead."
    )
    include_interval <- interval
  }
  if (!is.logical(include_interval) || length(include_interval) != 1L ||
      is.na(include_interval)) {
    .bm_abort("`include_interval` must be one nonmissing logical value.")
  }
  include_interval
}

# Align stored pair draws to the empirical pair table. Current objects use the
# injective key. Version-0 objects are accepted only when their full legacy
# label vector is positionally identical to the empirical table; matching the
# delimiter-based strings would be unsafe because distinct pairs can collide.
.bm_align_pair_draws <- function(draws, item_i, item_j) {
  key <- .bm_pair_key(item_i, item_j)
  draw_key <- colnames(draws)
  legacy_key <- paste(item_i, item_j, sep = "::")
  if (!is.null(draw_key) && length(draw_key) == length(key) &&
      identical(draw_key, legacy_key)) {
    idx <- seq_along(key)
  } else {
    if (is.null(draw_key) || anyDuplicated(draw_key) ||
        length(draw_key) != length(key)) {
      .bm_abort(
        "Pair draws do not carry one unique column for every empirical pair.",
        "bm_invariant_error"
      )
    }
    idx <- match(key, draw_key)
    if (anyNA(idx) || anyDuplicated(idx)) {
      .bm_abort(
        "Pair-draw identities do not match the empirical pair table.",
        "bm_invariant_error"
      )
    }
  }
  aligned <- draws[, idx, drop = FALSE]
  colnames(aligned) <- key
  list(draws = aligned, key = key)
}

# One accessor for both object classes, so every extractor above can be written
# once instead of twice.
.bm_parts <- function(x) {
  if (inherits(x, "bm_bb_h")) {
    return(list(empirical = x$empirical, interval = x$interval,
                draws = x$draws, rng = x$rng, sampling = x$sampling,
                mc = x$mc_diagnostics))
  }
  if (inherits(x, "bm_empirical_h")) {
    return(list(empirical = x, interval = NULL, draws = NULL,
                rng = NULL, sampling = NULL, mc = NULL))
  }
  .bm_abort(
    "Expected a fit from bb_mokken_h() or mokken_h().",
    hint = sprintf("Got an object of class %s.",
                   paste(class(x), collapse = "/"))
  )
}

# Raw equal-tail quantiles, type 8, matching the scale-level convention. No
# calibration is applied here: see the note carried by every caller.
.bm_draw_quantiles <- function(draws, targets) {
  idx <- match(targets, colnames(draws))
  n <- length(targets)
  out <- list(mean = rep(NA_real_, n), lower = rep(NA_real_, n),
              upper = rep(NA_real_, n), defined = rep(0L, n))
  for (k in seq_len(n)) {
    if (is.na(idx[k])) next
    v <- draws[, idx[k]]
    v <- v[is.finite(v)]
    out$defined[k] <- length(v)
    if (!length(v)) next
    q <- stats::quantile(v, c(0.025, 0.975), names = FALSE, type = 8)
    out$mean[k] <- mean(v)
    out$lower[k] <- q[1L]
    out$upper[k] <- q[2L]
  }
  out
}

.bm_chr_or_na <- function(x) {
  if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1L]
}

.bm_int_or_na <- function(x) {
  if (is.null(x) || !length(x)) NA_integer_ else as.integer(x)[1L]
}
