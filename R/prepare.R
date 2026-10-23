# =============================================================================
# Data preparation
#
# The place users get stuck in v0 is not the estimation call, it is assembling
# `supports` and `score_map` by hand and then reading a validation error that
# says what is wrong but not what to do about it. These five functions build
# those arguments from the data, and report what they found.
# =============================================================================

#' Screen a dataset before fitting
#'
#' Reports what the data look like from the package's point of view: item type,
#' observed values, complete-scale loss, constant items, items that may need
#' reverse keying, and cluster structure. Run it once before `mokken_h()` or
#' `bb_mokken_h()` on data you have not analyzed before.
#'
#' Nothing is modified and nothing is fitted. The result records a suggested
#' next call built from the expressions supplied by the caller and the complete
#' selected item vector. `next_call_runnable` is `TRUE` only when that call has
#' no unresolved scientific choice. In particular, a clustered call is not
#' labeled runnable until `cluster_target` has been supplied explicitly.
#'
#' @param data A matrix or data frame of item responses.
#' @param items Character vector of item columns. Defaults to every column
#'   except `cluster` if `cluster` was given by name.
#' @param cluster Optional cluster labels, either a vector as long as `nrow`
#'   or the name of a column in `data`.
#' @param cluster_target Optional clustered population target, either
#'   `"respondent"` or `"equal_cluster"`. Supplying it allows the suggested
#'   clustered call to use that exact value. When it is omitted, the suggestion
#'   contains a quoted choice marker and is explicitly labeled non-runnable;
#'   the package does not select a target.
#' @param item_type `"auto"` (the default) infers binary or ordinal from the
#'   observed values; `"binary"` or `"ordinal"` force the reading.
#' @param score_map,missing_codes Optional named lists, as passed to
#'   [mokken_h()]. Supplying them makes the screen reflect the scored values
#'   rather than the raw ones.
#'
#' @return An object of class `bm_data_check` with elements `items` (a data
#'   frame, one row per item), `rows` (counts), `clusters` (a
#'   [bm_cluster_info()] object or `NULL`), `flags` (a data frame of issues
#'   worth reading), `next_call` (a parseable suggested command),
#'   `next_call_runnable`, `next_call_reason`, `next_call_method`, and
#'   `call_expressions`, which records the captured `data`, `items`, and
#'   `cluster` expressions.
#'
#' @section Reverse keying is reported, never applied:
#' An item whose rest-score correlation is negative is flagged, because that is
#' what a reverse-keyed item looks like. It is not corrected. Automatic
#' reversal would silently double-reverse data that arrived already scored,
#' which is the more common situation with published instruments. To reverse an
#' item, say so: `bm_score_map(data, reverse = "item3")`.
#'
#' @examples
#' bm_check_data(srs)
#'
#' bm_check_data(classroom_wb, items = paste0("wb", 1:6),
#'               cluster = "class_id", cluster_target = "respondent")
#'
#' @family bayesmokken-prepare
#' @seealso [bm_supports()], [bm_score_map()], [bm_cluster_info()]
#' @export
bm_check_data <- function(data,
                          items = NULL,
                          cluster = NULL,
                          item_type = c("auto", "binary", "ordinal"),
                          score_map = NULL,
                          missing_codes = NULL,
                          cluster_target = NULL) {
  data_expr <- substitute(data)
  items_expr <- if (missing(items)) NULL else substitute(items)
  cluster_expr <- if (missing(cluster)) NULL else substitute(cluster)
  score_map_expr <- if (missing(score_map)) NULL else substitute(score_map)
  missing_codes_expr <- if (missing(missing_codes)) {
    NULL
  } else {
    substitute(missing_codes)
  }
  item_type <- match.arg(item_type)
  parsed <- .bm_prep_input(data, items, cluster)
  data <- parsed$data
  items <- parsed$items
  cluster <- parsed$cluster
  if (!is.null(cluster_target) &&
      (!is.character(cluster_target) || length(cluster_target) != 1L ||
       is.na(cluster_target) ||
       !cluster_target %in% c("respondent", "equal_cluster"))) {
    .bm_abort(
      "`cluster_target` must be `\"respondent\"`, `\"equal_cluster\"`, or NULL."
    )
  }
  if (is.null(cluster) && !is.null(cluster_target)) {
    .bm_abort("`cluster_target` requires `cluster`.")
  }

  scored <- .bm_prep_scored(data, items, score_map, missing_codes)
  n_total <- nrow(scored)
  complete <- stats::complete.cases(scored)
  n_complete <- sum(complete)

  values <- lapply(scored, function(x) sort(unique(x[!is.na(x)])))
  n_values <- vapply(values, length, integer(1))
  detected <- if (identical(item_type, "auto")) {
    if (all(vapply(values, function(v) all(v %in% c(0, 1)), logical(1)))) {
      "binary"
    } else {
      "ordinal"
    }
  } else {
    item_type
  }

  # Rest-score correlation, computed on complete rows only. A negative value is
  # the signature of an item keyed the other way round.
  rest_cor <- rep(NA_real_, length(items))
  if (n_complete >= 3L) {
    cc <- as.matrix(scored[complete, , drop = FALSE])
    total <- rowSums(cc)
    for (j in seq_along(items)) {
      rest <- total - cc[, j]
      rest_cor[j] <- if (stats::sd(cc[, j]) > 0 && stats::sd(rest) > 0) {
        stats::cor(cc[, j], rest)
      } else {
        NA_real_
      }
    }
  }

  item_tab <- data.frame(
    item = items,
    n_missing = vapply(scored, function(x) sum(is.na(x)), integer(1)),
    n_values = n_values,
    min = vapply(values, function(v) if (length(v)) min(v) else NA_real_, numeric(1)),
    max = vapply(values, function(v) if (length(v)) max(v) else NA_real_, numeric(1)),
    observed = vapply(values, function(v) paste(v, collapse = " "), character(1)),
    rest_correlation = rest_cor,
    stringsAsFactors = FALSE
  )
  rownames(item_tab) <- NULL

  flags <- .bm_prep_flags(item_tab, detected, n_total, n_complete, values,
                          clustered = !is.null(cluster))

  cluster_info <- if (is.null(cluster)) {
    NULL
  } else {
    bm_cluster_info(cluster[complete], data = scored[complete, , drop = FALSE])
  }
  next_step <- .bm_prep_next_call(
    items = items,
    detected = detected,
    data_expr = data_expr,
    items_expr = items_expr,
    cluster_expr = cluster_expr,
    cluster_name = parsed$cluster_name,
    cluster_target = cluster_target,
    cluster_count = if (is.null(cluster_info)) NULL else cluster_info$G,
    score_map_expr = score_map_expr,
    missing_codes_expr = missing_codes_expr,
    score_map_supplied = !is.null(score_map),
    missing_codes_supplied = !is.null(missing_codes)
  )

  structure(
    list(
      items = item_tab,
      rows = list(n_total = n_total, n_complete = n_complete,
                  n_excluded = n_total - n_complete),
      item_type = detected,
      item_type_source = if (identical(item_type, "auto")) "detected" else "declared",
      clusters = cluster_info,
      flags = flags,
      next_call = next_step$code,
      next_call_runnable = next_step$runnable,
      next_call_reason = next_step$reason,
      next_call_method = next_step$method,
      call_expressions = next_step$call_expressions
    ),
    class = "bm_data_check"
  )
}


#' Build the score supports for polytomous items
#'
#' Produces the `supports` argument that [mokken_h()] and [bb_mokken_h()]
#' require for ordinal items: a named list giving, for each item, the finite
#' increasing set of scores that item can take.
#'
#' Declaring the support is not bureaucracy. The maximum covariance in the
#' denominator of a polytomous coefficient is computed at the declared support,
#' so a category that exists in the instrument but happens to be unobserved in
#' your sample still belongs in the support. That is what `extend` is for.
#'
#' @param data A matrix or data frame of item responses.
#' @param items Character vector of item columns; defaults to every column.
#' @param from `"observed"` (the default) uses exactly the values that occur;
#'   `"range"` fills in the integer grid from the smallest to the largest
#'   observed value, which is usually what you want when a middle category
#'   happens to be empty.
#' @param extend Optional numeric vector of extra scores to add to every item,
#'   or a named list giving extra scores per item. Use it for categories the
#'   instrument allows but nobody chose.
#' @param score_map Optional score map, as passed to [mokken_h()]. When given,
#'   supports are built from the mapped values.
#'
#' @return A named list of numeric vectors, one per item, each sorted and
#'   without duplicates, ready to pass as `supports`.
#'
#' @examples
#' items <- paste0("wb", 1:6)
#' sup <- bm_supports(classroom_wb, items = items)
#' str(sup[1:2])
#'
#' # A five-point instrument where nobody used category 5
#' d <- data.frame(a = c(1, 2, 3, 4), b = c(2, 2, 3, 4))
#' bm_supports(d, from = "range", extend = 5)
#'
#' @family bayesmokken-prepare
#' @seealso [bm_check_data()], [bm_score_map()]
#' @export
bm_supports <- function(data,
                        items = NULL,
                        from = c("observed", "range"),
                        extend = NULL,
                        score_map = NULL) {
  from <- match.arg(from)
  parsed <- .bm_prep_input(data, items, NULL)
  data <- parsed$data
  items <- parsed$items
  scored <- .bm_prep_scored(data, items, score_map, NULL)

  out <- lapply(items, function(it) {
    v <- scored[[it]]
    v <- sort(unique(v[!is.na(v)]))
    if (!length(v)) {
      .bm_abort(sprintf("Item `%s` has no non-missing values.", it))
    }
    if (identical(from, "range")) {
      if (any(v != floor(v))) {
        .bm_abort(
          sprintf("`from = \"range\"` needs integer scores; `%s` has %s.",
                  it, paste(utils::head(v, 4), collapse = ", ")),
          hint = "Use from = \"observed\", or map the scores first with bm_score_map()."
        )
      }
      v <- seq.int(min(v), max(v))
    }
    extra <- if (is.list(extend)) extend[[it]] else extend
    if (!is.null(extra)) {
      if (!is.numeric(extra) || anyNA(extra) || any(!is.finite(extra))) {
        .bm_abort("`extend` must be finite numeric values.")
      }
      v <- sort(unique(c(v, extra)))
    }
    if (length(v) < 2L) {
      .bm_abort(
        sprintf("Item `%s` has only one distinct score, so it cannot be scaled.", it),
        hint = "Drop the item, or check whether a missing code was left unmapped."
      )
    }
    as.numeric(v)
  })
  stats::setNames(out, items)
}


#' Build a score map
#'
#' Produces the `score_map` argument of [mokken_h()] and [bb_mokken_h()]: a
#' named list that says, for each item, which raw value becomes which score.
#' Use it to reverse an item, to collapse categories, or to score character and
#' factor responses.
#'
#' @param data A matrix or data frame of item responses.
#' @param items Character vector of item columns; defaults to every column.
#' @param reverse Character vector of items to reverse. An item's scores are
#'   mirrored within its own observed range, so `1:5` becomes `5:1`.
#' @param recode Optional named list of explicit maps, one per item, each a
#'   named vector whose names are raw values and whose values are scores. Takes
#'   precedence over `reverse` for the same item.
#' @param to Optional target support for reversal, as a numeric vector or a
#'   named list per item. Defaults to the item's own observed values, which is
#'   right when the observed range covers the instrument's range and wrong when
#'   it does not -- pass `to` in that case.
#'
#' @return A named list of named numeric vectors, ready to pass as `score_map`.
#'   Items you did not mention are omitted, which is what the estimation
#'   functions expect.
#'
#' @section Do not reverse twice:
#' Published datasets frequently arrive already scored, with reverse-keyed
#' items flipped by whoever prepared them. `mokken::SWMDK` is one such case:
#' its documentation lists three reverse-keyed items, and its stored values
#' already reflect them. Reversing again would corrupt the analysis while
#' producing perfectly plausible-looking numbers. Check the data's
#' documentation before using `reverse`.
#'
#' @examples
#' # Reverse one item of a five-point scale
#' map <- bm_score_map(classroom_wb, items = paste0("wb", 1:6),
#'                     reverse = "wb3")
#' map$wb3
#'
#' # Score character responses explicitly
#' d <- data.frame(q1 = c("no", "yes", "yes"), q2 = c("yes", "no", "yes"))
#' bm_score_map(d, recode = list(q1 = c(no = 0, yes = 1),
#'                               q2 = c(no = 0, yes = 1)))
#'
#' @family bayesmokken-prepare
#' @seealso [bm_supports()], [bm_check_data()]
#' @export
bm_score_map <- function(data,
                         items = NULL,
                         reverse = NULL,
                         recode = NULL,
                         to = NULL) {
  parsed <- .bm_prep_input(data, items, NULL)
  data <- parsed$data
  items <- parsed$items

  if (!is.null(reverse)) {
    if (!is.character(reverse) || anyNA(reverse)) {
      .bm_abort("`reverse` must be a character vector of item names.")
    }
    unknown <- setdiff(reverse, items)
    if (length(unknown)) {
      .bm_abort(
        sprintf("Unknown item(s) in `reverse`: %s.",
                paste(unknown, collapse = ", ")),
        hint = paste0("Available items: ", paste(items, collapse = ", "), ".")
      )
    }
  }
  if (!is.null(recode)) {
    if (!is.list(recode) || is.null(names(recode)) ||
        anyDuplicated(names(recode))) {
      .bm_abort("`recode` must be a uniquely named list, one element per item.")
    }
    unknown <- setdiff(names(recode), items)
    if (length(unknown)) {
      .bm_abort(sprintf("Unknown item(s) in `recode`: %s.",
                        paste(unknown, collapse = ", ")))
    }
  }

  out <- list()
  for (it in items) {
    if (!is.null(recode) && it %in% names(recode)) {
      m <- recode[[it]]
      if (is.null(names(m)) || anyDuplicated(names(m))) {
        .bm_abort(sprintf("`recode$%s` must have unique names (the raw values).", it))
      }
      out[[it]] <- stats::setNames(as.numeric(m), names(m))
      next
    }
    if (!is.null(reverse) && it %in% reverse) {
      raw <- sort(unique(data[[it]][!is.na(data[[it]])]))
      target <- if (is.list(to)) to[[it]] else to
      target <- if (is.null(target)) as.numeric(raw) else sort(as.numeric(target))
      if (length(target) != length(raw)) {
        .bm_abort(
          sprintf("Cannot reverse `%s`: %d observed values but %d target scores.",
                  it, length(raw), length(target)),
          hint = "Pass `to` with one score per observed value of that item."
        )
      }
      out[[it]] <- stats::setNames(rev(target), as.character(raw))
    }
  }
  out
}


#' Declare values that mean missing
#'
#' A small helper that builds the `missing_codes` argument: a named list saying
#' which raw values should be treated as missing before scoring.
#'
#' @param items Character vector of item names.
#' @param codes Numeric or character vector of values to treat as missing,
#'   applied to every item, or a named list giving codes per item.
#'
#' @return A named list, ready to pass as `missing_codes`.
#'
#' @examples
#' bm_missing_codes(paste0("SRS", 1:3), codes = c(-9, 99))
#'
#' @family bayesmokken-prepare
#' @export
bm_missing_codes <- function(items, codes) {
  if (!is.character(items) || !length(items) || anyNA(items)) {
    .bm_abort("`items` must be a character vector of item names.")
  }
  if (is.list(codes)) {
    unknown <- setdiff(names(codes), items)
    if (length(unknown)) {
      .bm_abort(sprintf("Unknown item(s) in `codes`: %s.",
                        paste(unknown, collapse = ", ")))
    }
    return(codes[intersect(items, names(codes))])
  }
  if (!length(codes) || anyNA(codes)) {
    .bm_abort("`codes` must be one or more non-missing values.")
  }
  stats::setNames(rep(list(codes), length(items)), items)
}


#' Summarize cluster structure
#'
#' Describes the clusters respondents are nested in: how many there are, how
#' unequal they are, the weight each population target gives them, and the
#' narrow association between cluster size and cluster mean total score.
#'
#' @param cluster Cluster labels, one per respondent.
#' @param data Optional item data, used to compute each cluster's mean total
#'   score and hence the size--mean-score Spearman statistic.
#' @param items Optional item columns of `data`; defaults to every column.
#'
#' @return An object of class `bm_cluster_info` with elements `G` (number of
#'   clusters), `sizes`, `table` (one row per cluster, including the weight
#'   each target gives it), `size_mean_score_spearman`, the deprecated alias
#'   `informative_index`, and `singletons`.
#'
#' @section The size--mean-score statistic:
#' This is the Spearman correlation between cluster size and the cluster's mean
#' total score. It is only a descriptive location diagnostic. Mokken H also
#' depends on covariance and the full response distribution, so a value near
#' zero does not show that respondent-weighted and equal-cluster H agree, and a
#' large value does not quantify their difference. To assess target sensitivity,
#' fit the same declared scale under both targets and compare the two
#' deterministic H values directly.
#'
#' @examples
#' info <- bm_cluster_info(classroom_wb$class_id,
#'                         data = classroom_wb[, paste0("wb", 1:6)])
#' info
#' head(info$table)
#'
#' @family bayesmokken-prepare
#' @seealso [bm_plot_clusters()], `vignette("m4-hierarchical-bb-targets", package = "bayesmokken")`
#' @export
bm_cluster_info <- function(cluster, data = NULL, items = NULL) {
  if (is.null(cluster) || !length(cluster)) {
    .bm_abort("`cluster` must be a vector of cluster labels.")
  }
  if (anyNA(cluster)) {
    .bm_abort("`cluster` must not contain missing labels.",
              hint = "Drop those respondents, or give them their own label.")
  }
  cl <- as.character(cluster)
  sizes <- table(cl)
  g <- length(sizes)
  if (g < 2L) {
    .bm_abort("At least two clusters are needed.")
  }
  n <- length(cl)

  mean_score <- rep(NA_real_, g)
  if (!is.null(data)) {
    data <- as.data.frame(data)
    if (is.null(items)) items <- names(data)
    if (nrow(data) != n) {
      .bm_abort("`data` must have one row per element of `cluster`.")
    }
    m <- as.matrix(data[, items, drop = FALSE])
    storage.mode(m) <- "double"
    total <- rowSums(m)
    mean_score <- as.numeric(tapply(total, factor(cl, levels = names(sizes)),
                                    mean, na.rm = TRUE))
  }

  tab <- data.frame(
    cluster = names(sizes),
    size = as.integer(sizes),
    weight_respondent = as.numeric(sizes) / n,
    weight_equal = 1 / g,
    mean_total_score = mean_score,
    stringsAsFactors = FALSE
  )
  ord <- order(-tab$size, tab$cluster, method = "radix")
  tab <- tab[ord, , drop = FALSE]
  rownames(tab) <- NULL

  idx <- if (all(is.na(tab$mean_total_score)) ||
             stats::sd(tab$size) == 0) {
    NA_real_
  } else {
    suppressWarnings(
      stats::cor(tab$size, tab$mean_total_score, method = "spearman")
    )
  }

  structure(
    list(
      G = g, N = n,
      sizes = stats::setNames(as.integer(sizes), names(sizes)),
      table = tab,
      size_mean_score_spearman = idx,
      # Compatibility alias retained during 0.9.x. It must not be interpreted
      # as a diagnostic of agreement between the two target H functionals.
      informative_index = idx,
      singletons = tab$cluster[tab$size == 1L]
    ),
    class = "bm_cluster_info"
  )
}


# --- internals ---------------------------------------------------------------

# Normalise (data, items, cluster) once, so five entry points behave the same.
# `cluster` may be a vector or the name of a column, and naming a column must
# also remove it from the default item set -- otherwise the cluster label
# silently becomes an item.
.bm_prep_input <- function(data, items, cluster) {
  if (is.null(data) || (!is.data.frame(data) && !is.matrix(data))) {
    .bm_abort("`data` must be a matrix or a data frame.")
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (is.null(colnames(data)) || anyDuplicated(colnames(data))) {
    .bm_abort("`data` must have unique column names.")
  }
  cluster_name <- NULL
  if (!is.null(cluster) && is.character(cluster) && length(cluster) == 1L &&
      cluster %in% names(data)) {
    cluster_name <- cluster
    cluster <- data[[cluster]]
  }
  if (is.null(items)) {
    items <- setdiff(names(data), cluster_name)
  }
  if (!is.character(items) || length(items) < 2L || anyDuplicated(items)) {
    .bm_abort("`items` must name at least two distinct columns.")
  }
  unknown <- setdiff(items, names(data))
  if (length(unknown)) {
    .bm_abort(
      sprintf("Column(s) not found: %s.", paste(unknown, collapse = ", ")),
      hint = paste0("Available: ", paste(utils::head(names(data), 12),
                                         collapse = ", "),
                    if (ncol(data) > 12) ", ..." else "", ".")
    )
  }
  if (!is.null(cluster) && length(cluster) != nrow(data)) {
    .bm_abort("`cluster` must have one label per row of `data`.")
  }
  list(
    data = data,
    items = items,
    cluster = cluster,
    cluster_name = cluster_name
  )
}

# Apply missing codes and the score map, so the screen describes the values the
# estimator will actually see rather than the raw ones.
.bm_prep_scored <- function(data, items, score_map, missing_codes) {
  out <- data[, items, drop = FALSE]
  for (it in items) {
    x <- out[[it]]
    codes <- .bm_item_option(missing_codes, it)
    if (!is.null(codes)) x[x %in% codes] <- NA
    map <- .bm_item_option(score_map, it)
    if (!is.null(map)) {
      x <- unname(map[as.character(x)])
    } else if (is.factor(x)) {
      x <- suppressWarnings(as.numeric(as.character(x)))
    } else if (is.character(x)) {
      x <- suppressWarnings(as.numeric(x))
    } else if (is.logical(x)) {
      x <- as.numeric(x)
    }
    out[[it]] <- as.numeric(x)
  }
  out
}

.bm_prep_flags <- function(item_tab, detected, n_total, n_complete, values,
                           clustered = FALSE) {
  add <- function(df, level, item, message) {
    rbind(df, data.frame(level = level, item = item, message = message,
                         stringsAsFactors = FALSE))
  }
  f <- data.frame(level = character(), item = character(),
                  message = character(), stringsAsFactors = FALSE)

  const <- item_tab$item[item_tab$n_values <= 1L]
  for (it in const) {
    f <- add(f, "error", it,
             "constant: only one distinct value, so it cannot contribute")
  }
  rev_flag <- item_tab$item[!is.na(item_tab$rest_correlation) &
                              item_tab$rest_correlation < 0]
  for (it in rev_flag) {
    f <- add(f, "warning", it,
             "negative rest-score correlation: may need reverse keying")
  }
  miss <- item_tab$item[item_tab$n_missing > 0]
  for (it in miss) {
    f <- add(f, "note", it,
             sprintf("%d missing value(s)", item_tab$n_missing[item_tab$item == it]))
  }
  if (n_complete < n_total) {
    f <- add(f, "note", NA_character_,
             sprintf("%d of %d rows dropped by complete-scale filtering (%.1f%%)",
                     n_total - n_complete, n_total,
                     100 * (n_total - n_complete) / n_total))
  }
  # Dichotomous items together with a cluster argument cross the binary and
  # the clustered paths; the clustered procedure was developed and confirmed
  # for polytomous items, so an all-two-point support is refused there.
  binary_values <- all(vapply(values, function(v) all(v %in% c(0, 1)),
                              logical(1)))
  if (isTRUE(clustered) && binary_values) {
    f <- add(f, "warning", NA_character_,
             paste("0/1 items with clusters: the clustered interval",
                   "refuses an all-two-point support, because the",
                   "procedure was confirmed for polytomous items.",
                   "See bm_guardrails()."))
  }
  if (identical(detected, "ordinal")) {
    mins <- vapply(values, function(v) if (length(v)) min(v) else NA_real_, numeric(1))
    if (all(!is.na(mins)) && all(mins >= 1)) {
      f <- add(f, "note", NA_character_,
               paste("scores start at", min(mins),
                     "- if your codebook documents a 0-based coding this is a",
                     "shift, not an error; declare supports on the observed scale"))
    }
  }
  rownames(f) <- NULL
  f
}

.bm_deparse_code <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

.bm_call_code <- function(function_name, arguments, assign = "fit") {
  paste0(
    assign, " <- ", function_name, "(\n  ",
    paste(arguments, collapse = ",\n  "),
    "\n)"
  )
}

.bm_prep_next_call <- function(
    items, detected, data_expr, items_expr, cluster_expr, cluster_name,
    cluster_target, cluster_count, score_map_expr, missing_codes_expr,
    score_map_supplied, missing_codes_supplied) {
  data_code <- .bm_deparse_code(data_expr)
  items_code <- .bm_deparse_code(items)
  original_items_code <- if (is.null(items_expr)) {
    NULL
  } else {
    .bm_deparse_code(items_expr)
  }
  original_cluster_code <- if (is.null(cluster_expr)) {
    NULL
  } else {
    .bm_deparse_code(cluster_expr)
  }
  clustered <- !is.null(cluster_count)
  cluster_code <- if (!is.null(cluster_name)) {
    paste0("(", data_code, ")[[", .bm_deparse_code(cluster_name), "]]")
  } else {
    original_cluster_code
  }
  mapped_argument <- if (isTRUE(score_map_supplied)) {
    paste0("score_map = ", .bm_deparse_code(score_map_expr))
  } else {
    NULL
  }
  missing_argument <- if (isTRUE(missing_codes_supplied)) {
    paste0("missing_codes = ", .bm_deparse_code(missing_codes_expr))
  } else {
    NULL
  }
  common_arguments <- c(
    paste0("data = ", data_code),
    paste0("items = ", items_code),
    mapped_argument,
    missing_argument
  )

  if (!clustered && identical(detected, "binary")) {
    code <- .bm_call_code(
      "bb_mokken_h",
      c(
        common_arguments,
        "item_type = \"binary\"",
        "draws = 2000L",
        "seed = 1L"
      )
    )
    return(list(
      code = code,
      runnable = TRUE,
      reason = NA_character_,
      method = "bb_mokken_h",
      call_expressions = list(
        data = data_code,
        items = original_items_code,
        cluster = original_cluster_code
      )
    ))
  }

  support_arguments <- c(
    paste0("data = ", data_code),
    paste0("items = ", items_code),
    mapped_argument
  )
  support_code <- .bm_call_code(
    "bm_supports",
    support_arguments,
    assign = "supports"
  )
  if (!clustered) {
    fit_code <- .bm_call_code(
      "mokken_h",
      c(
        common_arguments,
        "item_type = \"ordinal\"",
        "supports = supports"
      )
    )
    return(list(
      code = paste(support_code, fit_code, sep = "\n"),
      runnable = TRUE,
      reason = paste(
        "Ordinal data without clusters receive a deterministic next call;",
        "the public ordinal interval requires clusters."
      ),
      method = "mokken_h",
      call_expressions = list(
        data = data_code,
        items = original_items_code,
        cluster = original_cluster_code
      )
    ))
  }

  has_target <- !is.null(cluster_target)
  target_code <- .bm_deparse_code(if (has_target) {
    cluster_target
  } else {
    "<choose respondent or equal_cluster>"
  })
  # The complete-record interval is computable for G >= 2, so the next call
  # is the interval call for every polytomous clustered screen; the cluster
  # count only decides whether the reason line points at the confirmed range.
  interval_lane <- identical(detected, "ordinal")
  outside_confirmed <- interval_lane &&
    (cluster_count < .bm_crbb_confirmed_cluster_min ||
       cluster_count > .bm_crbb_confirmed_cluster_max)
  method <- if (interval_lane) "bb_mokken_h" else "mokken_h"
  fit_arguments <- c(
    common_arguments,
    "item_type = \"ordinal\"",
    "supports = supports",
    paste0("cluster = ", cluster_code),
    paste0("cluster_target = ", target_code),
    "weight_law = \"complete_record\""
  )
  if (identical(method, "bb_mokken_h")) {
    fit_arguments <- c(
      fit_arguments,
      "draws = 2000L",
      "seed = 1L"
    )
  }
  reason <- if (!has_target) {
    paste(
      "Choose `cluster_target = \"respondent\"` or",
      "`cluster_target = \"equal_cluster\"` before running this call."
    )
  } else if (!interval_lane && identical(detected, "binary")) {
    paste(
      "A deterministic ordinal point-coefficient call is shown because the",
      "clustered interval refuses an all-two-category support."
    )
  } else if (outside_confirmed) {
    paste0(
      "The complete-record interval is computable with only ",
      cluster_count, " complete-scale clusters, but the confirmation ",
      "evidence covers ", .bm_crbb_confirmed_cluster_min, "-",
      .bm_crbb_confirmed_cluster_max, " clusters; the fit will report ",
      "OUTSIDE_CONFIRMED_CLUSTER_COUNT."
    )
  } else {
    NA_character_
  }
  list(
    code = paste(
      support_code,
      .bm_call_code(method, fit_arguments),
      sep = "\n"
    ),
    runnable = has_target,
    reason = reason,
    method = method,
    call_expressions = list(
      data = data_code,
      items = original_items_code,
      cluster = original_cluster_code
    )
  )
}
