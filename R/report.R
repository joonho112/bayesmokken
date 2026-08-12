# =============================================================================
# Reporting
#
# Turning a fit into the table and the sentence that go into a paper.
# =============================================================================

# The benchmark footnote is a single constant. It appears in bm_benchmark(),
# in the print method, in the help page and in the vignettes, and it must read
# identically in all four.
.bm_benchmark_note <- paste(
  "Conventional descriptive landmarks from the Mokken tradition, not",
  "hypothesis tests or decision rules. Each row reports only the point",
  "estimate minus the landmark and a neutral arithmetic relation."
)


#' Assemble a results table
#'
#' Builds a table suitable for a manuscript, at the scale, item or pair level,
#' in plain, Markdown or LaTeX form. The method identifier and the
#' interpretation note travel with the table so that a table lifted into a
#' paper carries its own provenance.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param what `"scale"`, `"item"` or `"pair"`.
#' @param format `"data.frame"`, `"markdown"` or `"latex"`.
#' @param digits Number of digits for the rendered forms.
#' @param include_interval If `TRUE`, include raw, uncalibrated draw summaries
#'   in item- or pair-level reports. The default is `FALSE`; the calibrated
#'   interval is defined for the fixed scale coefficient only.
#'
#' @return An object of class `bm_report`. Its `table` element is always a data
#'   frame; `rendered` holds the character lines for the Markdown and LaTeX
#'   forms. Printing it shows the rendered form when there is one.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_report(fit)
#'
#' \donttest{
#' # Paste straight into a manuscript
#' cat(bm_report(fit, what = "item", format = "markdown")$rendered, sep = "\n")
#' }
#'
#' @family bayesmokken-report
#' @seealso [bm_benchmark()], [bm_compare()], [bm_provenance()]
#' @export
bm_report <- function(x,
                      what = c("scale", "item", "pair"),
                      format = c("data.frame", "markdown", "latex"),
                      digits = 3,
                      include_interval = FALSE) {
  what <- match.arg(what)
  format <- match.arg(format)
  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) {
    .bm_abort("`digits` must be one nonnegative number.")
  }
  if (!is.logical(include_interval) || length(include_interval) != 1L ||
      is.na(include_interval)) {
    .bm_abort("`include_interval` must be one nonmissing logical value.")
  }
  parts <- .bm_parts(x)

  tab <- switch(
    what,
    scale = bm_scale_h(x),
    item  = bm_item_h(x, include_interval = include_interval),
    pair  = bm_pair_h(x, include_interval = include_interval)
  )

  notes <- if (is.null(parts$interval)) {
    character()
  } else if (is.null(parts$interval$interpretation) ||
             !length(parts$interval$interpretation)) {
    .bm_interpretation_note
  } else {
    as.character(parts$interval$interpretation)[1L]
  }
  if (what %in% c("item", "pair") &&
      "interval_calibration" %in% names(tab)) {
    notes <- c(notes, .bm_uncalibrated_note)
  }
  method_id <- if (is.null(parts$interval)) {
    NA_character_
  } else {
    parts$interval$method_id
  }

  rendered <- switch(
    format,
    data.frame = NULL,
    markdown = .bm_render_markdown(tab, digits, method_id, notes),
    latex = .bm_render_latex(tab, digits, method_id, notes)
  )

  structure(
    list(table = tab, notes = notes, method_id = method_id,
         what = what, format = format, digits = digits, rendered = rendered),
    class = "bm_report"
  )
}


#' Relate an estimate to the conventional benchmarks
#'
#' Reports the numeric difference between the point estimate and values such
#' as `.30`, `.40` and `.50` that the Mokken literature uses as descriptive
#' landmarks. Intervals are deliberately not inspected or classified.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param benchmarks Numeric vector of landmark values.
#'
#' @return A data frame with one row per benchmark: `benchmark`, `H`,
#'   `difference` (`H - benchmark`), `point_relation`, and `note`. Undefined
#'   coefficients retain numeric `NA` in `H` and `difference` and use the
#'   character state `"undefined"`.
#'
#' @section Benchmarks are descriptive:
#' `r bayesmokken:::.bm_benchmark_note`
#'
#' These landmarks are conventions about how to describe a coefficient, not
#' decision rules and not hypotheses. This function reports point differences
#' and neutral arithmetic relations only. It does not invert an interval into
#' a threshold classification or make a verdict about the scale.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_benchmark(fit)
#'
#' @family bayesmokken-report
#' @seealso [bm_report()], [bm_plot_scale_h()]
#' @export
bm_benchmark <- function(x, benchmarks = c(0.30, 0.40, 0.50)) {
  if (!is.numeric(benchmarks) || !length(benchmarks) ||
      any(!is.finite(benchmarks))) {
    .bm_abort("`benchmarks` must be one or more finite numbers.")
  }
  s <- bm_scale_h(x)
  benchmarks <- sort(unique(as.numeric(benchmarks)))
  point <- rep(as.numeric(s$H), length(benchmarks))
  difference <- point - benchmarks
  point_rel <- if (!is.finite(s$H)) {
    rep("undefined", length(benchmarks))
  } else {
    ifelse(difference < 0, "below", ifelse(difference > 0, "above", "at"))
  }
  note <- vapply(seq_along(benchmarks), function(k) {
    landmark <- format(benchmarks[k], trim = TRUE, scientific = FALSE,
                       digits = 8L)
    if (!is.finite(point[k])) {
      return(sprintf(
        "H is undefined; H - landmark %s is unavailable.", landmark
      ))
    }
    estimate <- format(point[k], trim = TRUE, scientific = FALSE,
                       digits = 8L)
    delta <- format(difference[k], trim = TRUE, scientific = FALSE,
                    digits = 8L)
    sprintf(
      "H = %s; H - landmark %s = %s (%s).",
      estimate, landmark, delta, point_rel[k]
    )
  }, character(1))

  structure(
    data.frame(benchmark = benchmarks, H = point, difference = difference,
               point_relation = as.character(point_rel), note = note,
               stringsAsFactors = FALSE),
    class = c("bm_benchmark", "data.frame")
  )
}


#' Compare several fits
#'
#' Puts the scale-level results of two or more fits side by side. Use it to
#' show a calibrated interval against its raw comparator, the two cluster
#' targets against each other, or the same analysis at different seeds.
#'
#' @param ... Named fits, for example
#'   `bm_compare(calibrated = f1, equal_tail = f2)`. Names become the `fit`
#'   column.
#' @param what Currently `"scale"` only; the argument exists so that later
#'   levels can be added without changing the call.
#' @param allow_different If `FALSE` (the default), fits with different fixed
#'   item-set hashes are rejected. Set `TRUE` only to request an explicitly
#'   labelled side-by-side table despite that incompatibility.
#'
#' @return A data frame with one row per fit. Its `comparability` attribute
#'   records the overall state, item-set state, respondent-row state, and the
#'   named hashes used for those checks.
#'
#' @section What is and is not comparable:
#' Fits of the *same items on the same respondents* under different methods,
#' targets or seeds are directly comparable. Different fixed item sets are
#' rejected unless `allow_different = TRUE`. Different respondent-row hashes
#' produce a typed warning and remain labelled in the comparability metadata.
#'
#' @examples
#' calibrated <- bb_mokken_h(srs, draws = 200, seed = 1)
#' raw <- bb_mokken_h(srs, draws = 200, seed = 1, interval = "equal_tail")
#' bm_compare(calibrated = calibrated, equal_tail = raw)
#'
#' @family bayesmokken-report
#' @seealso [bm_report()], [bm_plot_scale_h()]
#' @export
bm_compare <- function(..., what = "scale", allow_different = FALSE) {
  fits <- list(...)
  if (length(fits) < 2L) {
    .bm_abort("`bm_compare()` needs at least two fits.")
  }
  nms <- names(fits)
  if (is.null(nms) || any(!nzchar(nms))) {
    .bm_abort("Every fit must be named, for example bm_compare(a = f1, b = f2).")
  }
  if (!is.logical(allow_different) || length(allow_different) != 1L ||
      is.na(allow_different)) {
    .bm_abort("`allow_different` must be one nonmissing logical value.")
  }
  what <- match.arg(what, "scale")

  provenance <- lapply(fits, function(fit) {
    .bm_parts(fit)$empirical$provenance
  })
  item_hash <- stats::setNames(vapply(
    provenance, function(p) .bm_chr_or_na(p$item_set_hash), character(1)
  ), nms)
  row_hash <- stats::setNames(vapply(
    provenance, function(p) .bm_chr_or_na(p$row_hash), character(1)
  ), nms)
  same_items <- !anyNA(item_hash) && length(unique(item_hash)) == 1L
  same_rows <- !anyNA(row_hash) && length(unique(row_hash)) == 1L

  if (!same_items && !allow_different) {
    .bm_abort(
      "The fits use different or unavailable fixed item-set hashes.",
      "bm_comparability_error",
      hint = paste(
        "Compare the same prespecified items, or set `allow_different = TRUE`",
        "for an explicitly labelled side-by-side table."
      )
    )
  }
  if (!same_rows) {
    warning(structure(
      list(
        message = paste(
          "The fits use different or unavailable respondent-row hashes;",
          "their estimates are not based on the same complete-scale rows."
        ),
        call = NULL,
        row_hash = row_hash
      ),
      class = c("bm_row_comparability_warning", "warning", "condition")
    ))
  }

  rows <- lapply(seq_along(fits), function(k) {
    s <- bm_scale_h(fits[[k]])
    data.frame(fit = nms[k], s, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$length <- out$upper - out$lower
  rownames(out) <- NULL
  item_state <- if (same_items) "same" else if (anyNA(item_hash)) {
    "unknown_override"
  } else {
    "different_override"
  }
  row_state <- if (same_rows) "same" else if (anyNA(row_hash)) {
    "unknown"
  } else {
    "different"
  }
  state <- if (same_items && same_rows) {
    "same_items_same_rows"
  } else if (same_items) {
    paste0("same_items_", row_state, "_rows")
  } else if (anyNA(item_hash)) {
    paste0("unknown_items_override_", row_state, "_rows")
  } else {
    paste0("different_items_override_", row_state, "_rows")
  }
  attr(out, "comparability") <- list(
    state = state,
    item_set = item_state,
    rows = row_state,
    item_set_hash = item_hash,
    row_hash = row_hash
  )
  out
}


#' Citation information for a fit
#'
#' Returns the references to cite for the method a fit actually used, together
#' with the software citation.
#'
#' @param x Optional fit. When supplied, only the article matching that fit's
#'   method is returned; otherwise both are.
#' @param format `"text"` or `"bibtex"`.
#'
#' @return A character vector, one element per line. BibTeX output contains
#'   only paste-ready BibTeX; its separate `guidance` attribute records which
#'   method article applies. Text output appends that guidance as prose.
#'
#' @examples
#' cat(bm_cite(), sep = "\n")
#'
#' @family bayesmokken-report
#' @seealso [bm_provenance()], `citation("bayesmokken")`
#' @export
bm_cite <- function(x = NULL, format = c("text", "bibtex")) {
  format <- match.arg(format)
  which <- "both"
  if (!is.null(x)) {
    parts <- .bm_parts(x)
    which <- if (identical(parts$empirical$estimand$item_type, "ordinal")) {
      "clustered"
    } else {
      "binary"
    }
  }
  cit <- utils::citation("bayesmokken")
  keys <- vapply(seq_along(cit), function(k) {
    .bm_chr_or_na(cit[[k]]$key)
  }, character(1))
  software <- which(is.na(keys))
  selected <- switch(
    which,
    binary = unique(c(software, which(grepl("binary", keys, fixed = TRUE)))),
    clustered = unique(c(software, which(grepl("clustered", keys, fixed = TRUE)))),
    seq_along(cit)
  )
  cit <- cit[selected]
  guidance <- switch(
    which,
    binary = "Cite the software and the fixed-binary-scale article.",
    clustered = "Cite the software and the clustered-polytomous article.",
    "Cite the software and whichever article describes the method you used."
  )
  if (identical(format, "bibtex")) {
    lines <- .bm_trim_blank_lines(
      utils::capture.output(print(cit, style = "Bibtex"))
    )
    lines <- sub("^@Manual\\{,$", "@Manual{bayesmokken,", lines)
    attr(lines, "guidance") <- guidance
    return(lines)
  }
  lines <- .bm_trim_blank_lines(
    utils::capture.output(print(cit, style = "text"))
  )
  c(lines, "", guidance)
}


# --- rendering ---------------------------------------------------------------

.bm_fmt_table <- function(tab, digits) {
  out <- tab
  for (j in seq_along(out)) {
    if (is.integer(out[[j]])) {
      # Counts are counts. Rendering n = 337 as "337.000" looks like a
      # rounding decision that was never made.
      out[[j]] <- ifelse(is.na(out[[j]]), "", format(out[[j]]))
    } else if (is.numeric(out[[j]])) {
      out[[j]] <- formatC(out[[j]], digits = digits, format = "f")
      out[[j]][grepl("NA", out[[j]])] <- ""
    } else {
      out[[j]] <- ifelse(is.na(out[[j]]), "", as.character(out[[j]]))
    }
  }
  out
}

.bm_render_markdown <- function(tab, digits, method_id, notes) {
  f <- .bm_fmt_table(tab, digits)
  header <- paste0(
    "| ", paste(.bm_escape_markdown(names(f)), collapse = " | "), " |"
  )
  rule <- paste0("|", paste(rep("---", ncol(f)), collapse = "|"), "|")
  body <- apply(f, 1L, function(r) paste0(
    "| ", paste(.bm_escape_markdown(r), collapse = " | "), " |"
  ))
  c(header, rule, unname(body), "",
    if (!is.na(method_id)) {
      paste0("Method: ", .bm_escape_markdown(method_id))
    } else NULL,
    paste0("Note: ", .bm_escape_markdown(notes)))
}

.bm_render_latex <- function(tab, digits, method_id, notes) {
  f <- .bm_fmt_table(tab, digits)
  align <- paste(c("l", rep("r", ncol(f) - 1L)), collapse = "")
  body <- apply(f, 1L, function(r) paste0(
    paste(.bm_escape_latex(r), collapse = " & "), " \\\\"
  ))
  c(paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(.bm_escape_latex(names(f)), collapse = " & "), " \\\\"),
    "\\hline",
    unname(body),
    "\\hline",
    "\\end{tabular}",
    "",
    if (!is.na(method_id)) {
      paste0("% Method: ", .bm_escape_latex(method_id))
    } else NULL,
    paste0("% Note: ", .bm_escape_latex(notes)))
}

.bm_escape_with_map <- function(x, replacements) {
  vapply(as.character(x), function(value) {
    if (is.na(value)) return("")
    characters <- strsplit(enc2utf8(value), "", fixed = TRUE)[[1L]]
    paste0(vapply(characters, function(character) {
      position <- match(character, names(replacements))
      if (is.na(position)) character else replacements[[position]]
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

.bm_escape_markdown <- function(x) {
  .bm_escape_with_map(x, c(
    "\\" = "\\\\", "|" = "\\|", "`" = "\\`", "*" = "\\*",
    "_" = "\\_", "{" = "\\{", "}" = "\\}", "[" = "\\[",
    "]" = "\\]", "#" = "\\#", "+" = "\\+", "!" = "\\!",
    "$" = "\\$", "^" = "\\^", "~" = "\\~", "&" = "&amp;",
    "<" = "&lt;", ">" = "&gt;",
    "\n" = "<br>", "\r" = ""
  ))
}

.bm_escape_latex <- function(x) {
  .bm_escape_with_map(x, c(
    "\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}",
    "$" = "\\$", "&" = "\\&", "#" = "\\#", "%" = "\\%",
    "_" = "\\_", "^" = "\\textasciicircum{}",
    "~" = "\\textasciitilde{}", "|" = "\\textbar{}",
    "<" = "\\textless{}", ">" = "\\textgreater{}",
    "\n" = "\\newline{}", "\r" = ""
  ))
}

.bm_trim_blank_lines <- function(lines) {
  while (length(lines) && !nzchar(trimws(lines[[1L]]))) lines <- lines[-1L]
  while (length(lines) && !nzchar(trimws(lines[[length(lines)]]))) {
    lines <- lines[-length(lines)]
  }
  lines
}
