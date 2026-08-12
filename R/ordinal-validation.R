# Ordinal validation for the G18-A-reviewed finite-support coefficient.

.bm_resolve_item_type <- function(item_type, score_map = NULL,
                                  supports = NULL, cluster = NULL) {
  item_type <- match.arg(item_type, c("auto", "binary", "ordinal"))
  if (!identical(item_type, "auto")) {
    return(item_type)
  }
  if (!is.null(cluster) || !is.null(supports)) {
    return("ordinal")
  }
  # Preserve the pre-G18-B binary default, including calls whose score map
  # would previously have failed binary validation. Ordinal calls without a
  # cluster/support declaration must set item_type = "ordinal" explicitly.
  "binary"
}

.bm_validate_named_item_list <- function(x, argument) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.list(x) || is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x))) {
    .bm_abort(sprintf(
      "`%s` must be a uniquely named list keyed by item.", argument
    ))
  }
  invisible(NULL)
}

.bm_map_ordinal_item <- function(x, item, score_map, missing_codes, support) {
  raw_class <- class(x)
  raw_levels <- if (is.factor(x)) {
    levels(x)
  } else {
    sort(unique(.bm_raw_key(x[!is.na(x)])))
  }
  if (is.numeric(x) && any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
    .bm_abort(sprintf("Item `%s` contains NaN or infinite values.", item))
  }

  missing_codes <- missing_codes %||% vector(mode = mode(x), length = 0L)
  declared_missing <- !is.na(x) &
    .bm_raw_key(x) %in% as.character(missing_codes)
  x[declared_missing] <- NA

  needs_map <- is.factor(x) || is.character(x) || is.logical(x)
  if (needs_map && is.null(score_map)) {
    .bm_abort(sprintf(
      "Ordinal item `%s` has class %s and requires an explicit named score map.",
      item, paste(raw_class, collapse = "/")
    ), hint = .bm_hints$non_numeric_items)
  }

  if (is.null(score_map)) {
    scored <- as.numeric(x)
    used_map <- NULL
  } else {
    if (!is.numeric(score_map) || anyNA(score_map) ||
        any(!is.finite(score_map)) || is.null(names(score_map)) ||
        any(!nzchar(names(score_map))) || anyDuplicated(names(score_map))) {
      .bm_abort(sprintf(
        "Ordinal score map for `%s` must be a finite numeric vector with unique nonempty names.",
        item
      ))
    }
    keys <- .bm_raw_key(x)
    unmatched <- !is.na(x) & !keys %in% names(score_map)
    if (any(unmatched)) {
      .bm_abort(sprintf(
        "Ordinal score map for `%s` does not cover observed value(s): %s.",
        item, paste(sort(unique(keys[unmatched])), collapse = ", ")
      ))
    }
    scored <- unname(score_map[keys])
    scored[is.na(x)] <- NA_real_
    names(scored) <- NULL
    used_map <- score_map
  }
  if (any(!is.na(scored) & !is.finite(scored))) {
    .bm_abort(sprintf(
      "Ordinal item `%s` is not finite after scoring.", item
    ))
  }

  observed <- sort(unique(scored[!is.na(scored)]))
  if (is.null(support)) {
    .bm_abort(
      sprintf("Support for `%s` must be declared explicitly.", item),
      hint = .bm_hints$supports_missing
    )
  }
  if (!is.numeric(support) || length(support) < 2L ||
      anyNA(support) || any(!is.finite(support)) ||
      any(diff(support) <= 0) || any(!observed %in% support)) {
    .bm_abort(sprintf(
      "Support for `%s` must contain at least two finite, strictly increasing scores and cover all observed scores.",
      item
    ))
  }
  support <- as.numeric(support)

  manifest <- list(
    item_id = item,
    raw_class = raw_class,
    raw_levels = unname(raw_levels),
    score_map = if (is.null(used_map)) NULL else as.list(used_map),
    reverse_coded = if (is.null(used_map)) {
      FALSE
    } else {
      length(used_map) > 1L && all(diff(unname(used_map)) < 0)
    },
    missing_codes = as.list(missing_codes),
    observed_scores = observed,
    declared_support = support,
    unused_declared_scores = setdiff(support, observed)
  )
  list(values = scored, support = support, manifest = manifest)
}

bm_validate_ordinal_data <- function(data, score_map = NULL,
                                     missing_codes = NULL,
                                     supports = NULL) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    .bm_abort("`data` must be a matrix or data frame.")
  }
  if (nrow(data) < 2L) {
    .bm_abort("Ordinal scalability requires at least two respondent rows.")
  }
  if (ncol(data) < 2L) {
    .bm_abort("A fixed scale must contain at least two items.")
  }
  item_names <- colnames(data)
  if (is.null(item_names) || anyNA(item_names) ||
      any(!nzchar(item_names)) || anyDuplicated(item_names)) {
    .bm_abort("Item columns require unique, nonempty names.")
  }
  .bm_validate_named_item_list(score_map, "score_map")
  .bm_validate_named_item_list(missing_codes, "missing_codes")
  .bm_validate_named_item_list(supports, "supports")
  if (is.null(supports) || any(!item_names %in% names(supports))) {
    .bm_abort("`supports` must declare one score support for every selected item.",
              hint = .bm_hints$supports_missing)
  }

  scored <- matrix(
    NA_real_,
    nrow = nrow(data),
    ncol = ncol(data),
    dimnames = list(rownames(data), item_names)
  )
  manifests <- vector("list", length(item_names))
  declared_supports <- vector("list", length(item_names))
  names(manifests) <- names(declared_supports) <- item_names

  for (j in seq_along(item_names)) {
    item <- item_names[[j]]
    mapped <- .bm_map_ordinal_item(
      data[, j],
      item = item,
      score_map = .bm_item_option(score_map, item),
      missing_codes = .bm_item_option(missing_codes, item),
      support = .bm_item_option(supports, item)
    )
    scored[, j] <- mapped$values
    declared_supports[[item]] <- mapped$support
    manifests[[item]] <- mapped$manifest
  }

  structure(
    list(
      data = scored,
      item_names = item_names,
      supports = declared_supports,
      support_hash = digest::digest(declared_supports, algo = "sha256"),
      score_manifest = manifests,
      score_map_hash = digest::digest(manifests, algo = "sha256"),
      n_total = nrow(scored)
    ),
    class = "bm_validated_ordinal"
  )
}
