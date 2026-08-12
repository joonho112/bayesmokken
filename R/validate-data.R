# v1 note: `hint` was added in 0.9.0. It is optional and appended AFTER the
# original message, so the condition class and the first sentence of every
# pre-existing error are byte-identical to 0.0.0.9001. Tests that match on
# those messages keep passing; see the G-1 parity gate.
.bm_abort <- function(message, class = "bm_input_error", hint = NULL) {
  if (!is.null(hint) && nzchar(hint)) {
    message <- paste0(message, "\n  Hint: ", hint)
  }
  stop(structure(
    list(message = message, call = NULL),
    class = c(class, "error", "condition")
  ))
}

.bm_item_option <- function(x, item, default = NULL) {
  if (is.null(x)) {
    return(default)
  }
  if (!is.list(x) || is.null(names(x)) || !item %in% names(x)) {
    return(default)
  }
  x[[item]]
}

.bm_raw_key <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  as.character(x)
}

.bm_map_binary_item <- function(x, item, score_map, missing_codes) {
  raw_class <- class(x)
  raw_levels <- if (is.factor(x)) levels(x) else sort(unique(.bm_raw_key(x[!is.na(x)])))

  if (is.numeric(x) && any(is.nan(x) | is.infinite(x), na.rm = TRUE)) {
    .bm_abort(sprintf("Item `%s` contains NaN or infinite values.", item))
  }

  missing_codes <- missing_codes %||% vector(mode = mode(x), length = 0L)
  is_declared_missing <- !is.na(x) & .bm_raw_key(x) %in% as.character(missing_codes)
  x[is_declared_missing] <- NA

  needs_map <- is.factor(x) || is.character(x) || is.logical(x)
  if (needs_map && is.null(score_map)) {
    .bm_abort(sprintf(
      "Item `%s` has class %s and requires an explicit named score map.",
      item, paste(raw_class, collapse = "/")
    ), hint = .bm_hints$non_numeric_items)
  }

  if (is.null(score_map)) {
    scored <- as.numeric(x)
    used_map <- c("0" = 0, "1" = 1)
  } else {
    if (is.null(names(score_map)) || any(!nzchar(names(score_map))) ||
        anyDuplicated(names(score_map))) {
      .bm_abort(sprintf("Score map for `%s` must have unique nonempty names.", item))
    }
    if (!is.numeric(score_map) || any(!score_map %in% c(0, 1))) {
      .bm_abort(sprintf("Score map for `%s` must map to numeric 0/1.", item))
    }
    keys <- .bm_raw_key(x)
    unmatched <- !is.na(x) & !keys %in% names(score_map)
    if (any(unmatched)) {
      .bm_abort(sprintf(
        "Score map for `%s` does not cover observed value(s): %s.",
        item, paste(sort(unique(keys[unmatched])), collapse = ", ")
      ))
    }
    scored <- unname(score_map[keys])
    scored[is.na(x)] <- NA_real_
    names(scored) <- NULL
    used_map <- score_map
  }

  if (any(!is.na(scored) & !scored %in% c(0, 1))) {
    .bm_abort(sprintf("Item `%s` is not scored exactly 0/1 after mapping.", item))
  }

  reverse_coded <- identical(
    unname(used_map[c("0", "1")]),
    c(1, 0)
  )
  manifest <- list(
    item_id = item,
    raw_class = raw_class,
    raw_levels = unname(raw_levels),
    score_map = as.list(used_map),
    reverse_coded = reverse_coded,
    missing_codes = as.list(missing_codes),
    observed_scores = sort(unique(scored[!is.na(scored)])),
    unused_declared_scores = setdiff(c(0, 1), sort(unique(scored[!is.na(scored)])))
  )

  list(values = scored, manifest = manifest)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

bm_validate_binary_data <- function(data, score_map = NULL, missing_codes = NULL) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    .bm_abort("`data` must be a matrix or data frame.")
  }
  if (nrow(data) < 2L) {
    .bm_abort("Binary scalability requires at least two respondent rows.")
  }
  if (ncol(data) < 2L) {
    .bm_abort("A fixed scale must contain at least two items.")
  }

  item_names <- colnames(data)
  if (is.null(item_names) || anyNA(item_names) || any(!nzchar(item_names)) ||
      anyDuplicated(item_names)) {
    .bm_abort("Item columns require unique, nonempty names.")
  }
  if (!is.null(score_map) &&
      (!is.list(score_map) || is.null(names(score_map)) ||
       anyDuplicated(names(score_map)))) {
    .bm_abort("`score_map` must be a uniquely named list keyed by item.")
  }
  if (!is.null(missing_codes) &&
      (!is.list(missing_codes) || is.null(names(missing_codes)) ||
       anyDuplicated(names(missing_codes)))) {
    .bm_abort("`missing_codes` must be a uniquely named list keyed by item.")
  }

  scored <- matrix(
    NA_real_,
    nrow = nrow(data),
    ncol = ncol(data),
    dimnames = list(rownames(data), item_names)
  )
  manifests <- vector("list", length(item_names))
  names(manifests) <- item_names

  for (j in seq_along(item_names)) {
    item <- item_names[[j]]
    mapped <- .bm_map_binary_item(
      data[, j],
      item = item,
      score_map = .bm_item_option(score_map, item),
      missing_codes = .bm_item_option(missing_codes, item)
    )
    scored[, j] <- mapped$values
    manifests[[item]] <- mapped$manifest
  }

  structure(
    list(
      data = scored,
      item_names = item_names,
      score_manifest = manifests,
      score_map_hash = digest::digest(manifests, algo = "sha256"),
      n_total = nrow(scored)
    ),
    class = "bm_validated_binary"
  )
}
