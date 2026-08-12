# =============================================================================
# G-3  Interpretation firewall
#
# Whitelist entries are bound to the normalized matched line, its SHA-256 hash,
# and its expected occurrence count. A path/pattern exemption alone is never
# sufficient: adding endorsement language to an already-whitelisted file must
# create a new, unwhitelisted hash and fail this gate.
#
# Generated Rd exemptions must additionally trace to a live, whitelisted source
# sentence in R/. Duplicate, unused, stale, hash-mismatched, count-mismatched,
# and untraceable entries are gate failures.
#
#     Rscript inst/scripts/scan-forbidden-language.R
# =============================================================================

bm_forbidden_patterns <- function() {
  list(
    credible_interval = list(
      regex = "credible\\s+interval",
      scope = "all"
    ),
    posterior_probability = list(
      regex = "posterior\\s+probabilit",
      scope = "all"
    ),
    posterior_predictive = list(
      regex = "posterior\\s+predictive",
      scope = "all"
    ),
    threshold_probability = list(
      regex = "threshold\\s+probabilit",
      scope = "all"
    ),
    prob_h_exceeds = list(
      regex = "P\\s*\\(\\s*H\\s*[><]",
      scope = "all"
    ),
    significance_claim = list(
      regex = "statistically\\s+significant",
      scope = "all"
    ),
    scale_verdict = list(
      regex = "the\\s+scale\\s+is\\s+(valid|acceptable|unacceptable)",
      scope = "all"
    ),
    item_selection = list(
      regex = paste0(
        "\\bitem\\s+selection\\s+(is\\s+)?",
        "(performed|supported|available)\\b"
      ),
      scope = "all"
    ),
    passes_threshold = list(
      regex = "\\b(passes|fails)\\s+the\\s+\\.?[0-9]*\\s*threshold\\b",
      scope = "all"
    ),
    selection_affordance = list(
      regex = paste0(
        "\\b(which\\s+items?\\s+carry\\s+the\\s+scale|",
        "which\\s+(item\\s+)?pairs?\\s+(are\\s+)?doing\\s+the\\s+work|",
        "weakest\\s+(item|first)|items?\\s+dragging\\s+",
        "(the\\s+)?scale\\s+down|pull(s|ing)?\\s+(the\\s+)?",
        "scale\\s+down)\\b"
      ),
      scope = "source"
    ),
    benchmark_classification = list(
      regex = paste0(
        "\\b(entirely\\s+(above|below)|spans\\s+(the\\s+)?",
        "(benchmark|threshold)|data\\s+place\\s+h\\s+",
        "(above|below|on\\s+one\\s+side))\\b"
      ),
      scope = "source"
    ),
    interval_landmark_classification = list(
      regex = paste0(
        "\\b(position\\s+is\\s+not\\s+in\\s+question|",
        "(confidence\\s+)?interval.{0,60}",
        "(lies|falls|sits|is)\\s+(entirely\\s+)?(above|below).{0,40}",
        "(landmark|reference\\s+guide|benchmark))\\b"
      ),
      scope = "source"
    )
  )
}

bm_language_targets <- function(root = ".") {
  relative_files <- function(directory, pattern) {
    path <- file.path(root, directory)
    if (!dir.exists(path)) {
      return(character())
    }
    file.path(
      directory,
      list.files(path, pattern = pattern, full.names = FALSE)
    )
  }
  candidates <- c(
    relative_files("R", "\\.R$"),
    relative_files("man", "\\.Rd$"),
    relative_files("vignettes", "\\.(Rmd|qmd)$"),
    relative_files("inst/extdata/spec", "\\.(yml|yaml)$"),
    "DESCRIPTION",
    "README.md",
    "README.Rmd",
    "NEWS.md",
    "_pkgdown.yml"
  )
  candidates <- unique(gsub("\\\\", "/", candidates))
  sort(candidates[file.exists(file.path(root, candidates))])
}

bm_normalize_matched_sentence <- function(text) {
  text <- enc2utf8(text)
  text <- trimws(gsub("[[:space:]]+", " ", text, perl = TRUE))
  tolower(text)
}

bm_sentence_hash <- function(text) {
  digest::digest(
    bm_normalize_matched_sentence(text),
    algo = "sha256",
    serialize = FALSE
  )
}

bm_collect_forbidden_hits <- function(root = ".", targets = NULL,
                                      patterns = bm_forbidden_patterns()) {
  if (is.null(targets)) {
    targets <- bm_language_targets(root)
  }
  targets <- unique(gsub("\\\\", "/", targets))
  hits <- list()
  for (path in targets) {
    lines <- readLines(file.path(root, path), warn = FALSE)
    for (pattern_name in names(patterns)) {
      specification <- patterns[[pattern_name]]
      if (identical(specification$scope, "source") &&
          startsWith(path, "man/")) {
        next
      }
      index <- grep(
        specification$regex,
        lines,
        ignore.case = TRUE,
        perl = TRUE
      )
      for (line_number in index) {
        normalized <- bm_normalize_matched_sentence(lines[[line_number]])
        hash <- bm_sentence_hash(lines[[line_number]])
        hits[[length(hits) + 1L]] <- data.frame(
          path = path,
          line = as.integer(line_number),
          pattern = pattern_name,
          sentence_hash = hash,
          normalized_sentence = normalized,
          text = trimws(lines[[line_number]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(hits)) {
    return(data.frame(
      path = character(),
      line = integer(),
      pattern = character(),
      sentence_hash = character(),
      normalized_sentence = character(),
      text = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, hits)
  out$key <- paste(out$path, out$pattern, out$sentence_hash, sep = "::")
  rownames(out) <- NULL
  out
}

bm_empty_language_whitelist <- function() {
  data.frame(
    path = character(),
    pattern = character(),
    sentence_hash = character(),
    expected_count = integer(),
    source_path = character(),
    source_pattern = character(),
    source_hash = character(),
    stringsAsFactors = FALSE
  )
}

bm_read_language_whitelist <- function(path) {
  if (!file.exists(path)) {
    return(bm_empty_language_whitelist())
  }
  out <- utils::read.delim(
    path,
    header = TRUE,
    sep = "\t",
    quote = "",
    comment.char = "#",
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  required <- names(bm_empty_language_whitelist())
  if (!identical(names(out), required)) {
    stop(sprintf(
      "Whitelist columns must be exactly: %s.",
      paste(required, collapse = ", ")
    ))
  }
  for (column in required) {
    out[[column]][is.na(out[[column]])] <- ""
    out[[column]][out[[column]] == "."] <- ""
  }
  out$path <- gsub("\\\\", "/", out$path)
  out$source_path <- gsub("\\\\", "/", out$source_path)
  out
}

bm_language_issue <- function(type, key = "", detail = "") {
  data.frame(
    type = as.character(type),
    key = as.character(key),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

bm_validate_language_whitelist <- function(hits, whitelist, targets,
                                           patterns) {
  issues <- list()
  add_issue <- function(type, key = "", detail = "") {
    issues[[length(issues) + 1L]] <<- bm_language_issue(type, key, detail)
  }
  required <- names(bm_empty_language_whitelist())
  if (!identical(names(whitelist), required)) {
    add_issue(
      "INVALID_WHITELIST_SCHEMA",
      detail = paste("Expected columns:", paste(required, collapse = ", "))
    )
    return(do.call(rbind, issues))
  }
  if (!nrow(whitelist)) {
    if (nrow(hits)) {
      for (key in unique(hits$key)) {
        add_issue("UNWHITELISTED_HIT", key)
      }
    }
    if (!length(issues)) {
      return(bm_language_issue(character(), character(), character()))
    }
    return(do.call(rbind, issues))
  }

  whitelist$key <- paste(
    whitelist$path,
    whitelist$pattern,
    whitelist$sentence_hash,
    sep = "::"
  )
  duplicated_key <- duplicated(whitelist$key) |
    duplicated(whitelist$key, fromLast = TRUE)
  for (key in unique(whitelist$key[duplicated_key])) {
    add_issue("DUPLICATE_WHITELIST_KEY", key)
  }

  expected_numeric <- suppressWarnings(as.numeric(whitelist$expected_count))
  valid_count <- is.finite(expected_numeric) & expected_numeric >= 1 &
    expected_numeric == floor(expected_numeric)
  valid_hash <- grepl("^[0-9a-f]{64}$", whitelist$sentence_hash)
  valid_pattern <- whitelist$pattern %in% names(patterns)
  for (row in which(!valid_count | !valid_hash | !valid_pattern |
                    !nzchar(whitelist$path))) {
    add_issue(
      "INVALID_WHITELIST_ROW",
      whitelist$key[[row]],
      "Path, known pattern, SHA-256 hash, and positive expected count are required."
    )
  }

  for (row in seq_len(nrow(whitelist))) {
    key <- whitelist$key[[row]]
    path <- whitelist$path[[row]]
    pattern <- whitelist$pattern[[row]]
    if (!path %in% targets) {
      add_issue(
        "STALE_WHITELIST_ENTRY",
        key,
        "The whitelisted path is absent from the current scan targets."
      )
      next
    }
    exact_count <- sum(hits$key == key)
    same_pattern <- hits$path == path & hits$pattern == pattern
    expected <- suppressWarnings(as.integer(whitelist$expected_count[[row]]))
    if (exact_count == 0L) {
      if (any(same_pattern)) {
        add_issue(
          "HASH_MISMATCH",
          key,
          paste0(
            "Current hash(es): ",
            paste(unique(hits$sentence_hash[same_pattern]), collapse = ", ")
          )
        )
      } else {
        add_issue(
          "UNUSED_WHITELIST_ENTRY",
          key,
          "No current scanner hit uses this path and pattern."
        )
      }
    } else if (is.finite(expected) && exact_count != expected) {
      add_issue(
        "EXPECTED_COUNT_MISMATCH",
        key,
        sprintf("Expected %d occurrence(s); found %d.", expected, exact_count)
      )
    }

    is_rd <- startsWith(path, "man/") && endsWith(path, ".Rd")
    trace <- c(
      whitelist$source_path[[row]],
      whitelist$source_pattern[[row]],
      whitelist$source_hash[[row]]
    )
    if (is_rd) {
      if (any(!nzchar(trace))) {
        add_issue(
          "MISSING_RD_SOURCE_TRACE",
          key,
          "Generated Rd exemptions require source path, pattern, and hash."
        )
      } else {
        source_key <- paste(trace, collapse = "::")
        source_row <- whitelist$key == source_key
        source_hit <- hits$key == source_key
        if (!startsWith(trace[[1L]], "R/") ||
            !identical(trace[[2L]], pattern) ||
            !any(source_row) || !any(source_hit)) {
          add_issue(
            "INVALID_RD_SOURCE_TRACE",
            key,
            paste("No live whitelisted R source hit matches", source_key)
          )
        }
      }
    } else if (any(nzchar(trace))) {
      add_issue(
        "UNEXPECTED_SOURCE_TRACE",
        key,
        "Only generated man/*.Rd entries may carry a source trace."
      )
    }
  }

  live_keys <- unique(hits$key)
  whitelisted_keys <- unique(whitelist$key)
  for (key in setdiff(live_keys, whitelisted_keys)) {
    add_issue("UNWHITELISTED_HIT", key)
  }
  if (!length(issues)) {
    return(bm_language_issue(character(), character(), character()))
  }
  out <- do.call(rbind, issues)
  rownames(out) <- NULL
  out
}

bm_scan_forbidden_language <- function(
    root = ".",
    whitelist_file = file.path(
      root, "inst", "extdata", "language-whitelist.txt"
    ),
    targets = NULL,
    whitelist = NULL,
    patterns = bm_forbidden_patterns()) {
  if (is.null(targets)) {
    targets <- bm_language_targets(root)
  }
  targets <- unique(gsub("\\\\", "/", targets))
  if (is.null(whitelist)) {
    whitelist <- tryCatch(
      bm_read_language_whitelist(whitelist_file),
      error = function(error) error
    )
    if (inherits(whitelist, "error")) {
      return(list(
        status = 1L,
        targets = targets,
        hits = bm_collect_forbidden_hits(root, targets, patterns),
        whitelist = bm_empty_language_whitelist(),
        issues = bm_language_issue(
          "INVALID_WHITELIST_SCHEMA",
          detail = conditionMessage(whitelist)
        )
      ))
    }
  }
  hits <- bm_collect_forbidden_hits(root, targets, patterns)
  issues <- bm_validate_language_whitelist(
    hits = hits,
    whitelist = whitelist,
    targets = targets,
    patterns = patterns
  )
  list(
    status = if (nrow(issues)) 1L else 0L,
    targets = targets,
    hits = hits,
    whitelist = whitelist,
    issues = issues
  )
}

bm_print_language_scan <- function(result) {
  cat("G-3 forbidden-language scan\n")
  cat("  files scanned  : ", length(result$targets), "\n", sep = "")
  cat("  hits           : ", nrow(result$hits), "\n", sep = "")
  cat("  whitelist rows : ", nrow(result$whitelist), "\n", sep = "")
  if (result$status != 0L) {
    cat("  STATUS: FAIL\n\n")
    print(result$issues, row.names = FALSE)
  } else {
    cat("  STATUS: PASS (all exact sentence hashes and counts match)\n")
  }
  invisible(result)
}

bm_scanner_is_main <- function() {
  file_argument <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  if (!length(file_argument)) {
    return(FALSE)
  }
  invoked <- sub("^--file=", "", file_argument[[1L]])
  identical(
    basename(invoked),
    "scan-forbidden-language.R"
  )
}

if (bm_scanner_is_main()) {
  result <- bm_scan_forbidden_language()
  bm_print_language_scan(result)
  quit(status = result$status, save = "no")
}
