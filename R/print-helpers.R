# =============================================================================
# Console output helpers
#
# Every print method in the package is built from these pieces, so headers,
# rules, number formats and the closing "See:" line look the same everywhere
# and are defined once.
#
# Two rules that shape all of it:
#   * the interpretation sentence comes from a constant, never retyped
#   * the last line always tells the reader what to call next
# =============================================================================

# UTF-8 box drawing where the console can show it, ASCII where it cannot.
.bm_rule_char <- function() {
  utf8 <- tryCatch(isTRUE(l10n_info()$`UTF-8`), error = function(e) FALSE)
  if (utf8) "\u2500" else "-"
}

# Keep source files ASCII-only while preserving the UTF-8 console grammar at
# runtime. This separator is shared by headers, sample lines and navigation.
.bm_dot_separator <- function() paste0(" ", "\u00b7", " ")

.bm_dot_join <- function(...) {
  paste(c(...), collapse = .bm_dot_separator())
}

.bm_width <- function() {
  w <- getOption("width", 80L)
  max(40L, min(as.integer(w), 100L))
}

# "-- Title -----------------------------------------------------"
.bm_header <- function(title) {
  ch <- .bm_rule_char()
  lead <- strrep(ch, 2L)
  pad <- .bm_width() - nchar(title) - nchar(lead) - 2L
  cat(lead, " ", title, " ", strrep(ch, max(0L, pad)), "\n", sep = "")
}

.bm_field <- function(label, ..., indent = 2L, label_width = 12L,
                      separator = " ") {
  cat(strrep(" ", indent), formatC(label, width = -label_width), separator,
      ..., "\n", sep = "")
}

.bm_blank <- function() cat("\n")

# Vectorised: printers format whole columns as well as single values.
.bm_num <- function(x, digits = 3L) {
  if (is.null(x) || !length(x)) return("NA")
  out <- formatC(as.numeric(x), digits = digits, format = "f")
  out[is.na(x)] <- "NA"
  out
}

.bm_ci <- function(lower, upper, digits = 3L) {
  paste0("[", .bm_num(lower, digits), ", ", .bm_num(upper, digits), "]")
}

# Wrap a paragraph to the console width, indented.
.bm_para <- function(text, indent = 2L) {
  wrapped <- strwrap(text, width = .bm_width() - indent)
  cat(paste0(strrep(" ", indent), wrapped, collapse = "\n"), "\n", sep = "")
}

# The closing line. Present on every print method; this is how a reader who
# only ever types the object name discovers the rest of the package.
.bm_see <- function(..., blank = TRUE) {
  calls <- c(...)
  if (isTRUE(blank)) cat("\n")
  cat(strrep(" ", 2L), "See: ", paste(calls, collapse = .bm_dot_separator()),
      "\n", sep = "")
}

# Scientific content shared by the base and cli printers.  Keep this as plain
# character data: typography belongs to the renderer, while the numerical
# status, validation scope, interpretation and navigation must not diverge.
.bm_interval_semantic_lines <- function(
    interval,
    navigation = c("bm_interval()", "?bayesmokken-intervals")) {
  if (is.null(interval)) {
    return(list(
      has_interval = FALSE,
      numerical_status = NULL,
      validation_status = NULL,
      validation_scope = NULL,
      validation_range = NULL,
      interpretation = NULL,
      navigation = navigation
    ))
  }

  validation_status <- interval$validation_status
  validation_scope <- interval$validation_scope
  validation_range <- NULL
  if (!is.null(validation_status)) {
    g <- interval$cluster_count
    lo <- interval$validated_cluster_min
    hi <- interval$validated_cluster_max
    if (!is.null(g) && !is.null(lo) && !is.null(hi)) {
      validation_range <- switch(
        validation_status,
        CONFIRMED_CLUSTER_COUNT = sprintf(
          "G = %d; confirmed range %d-%d", g, lo, hi
        ),
        EXTRAPOLATED_CLUSTER_COUNT = sprintf(
          "G = %d; outside confirmed range %d-%d", g, lo, hi
        ),
        BELOW_VALIDATED_CLUSTER_COUNT = sprintf(
          "G = %d; below confirmed range %d-%d", g, lo, hi
        ),
        NOT_APPLICABLE_EQUAL_TAIL = sprintf(
          "G = %d; confirmed range %d-%d does not apply to equal-tail intervals",
          g, lo, hi
        ),
        sprintf("G = %d; confirmed range %d-%d", g, lo, hi)
      )
    }
  }

  interpretation <- interval$interpretation
  if (is.null(interpretation) || !length(interpretation) ||
      is.na(interpretation[[1L]]) || !nzchar(interpretation[[1L]])) {
    interpretation <- paste(
      .bm_interpretation_note,
      "Not a posterior credible interval or posterior probability."
    )
  }

  list(
    has_interval = TRUE,
    numerical_status = interval$status,
    validation_status = validation_status,
    validation_scope = validation_scope,
    validation_range = validation_range,
    interpretation = as.character(interpretation[[1L]]),
    navigation = navigation
  )
}

.bm_fit_semantic_lines <- function(x) {
  empirical <- inherits(x, "bm_empirical_h")
  interval <- if (empirical) NULL else .bm_parts(x)$interval
  navigation <- if (empirical) {
    c("summary()", "bm_item_h()", "bm_plot_item_h()",
      "bb_mokken_h() for an interval")
  } else {
    c("summary()", "bm_item_h()", "bm_plot_draws()", "bm_diagnostics()")
  }
  .bm_interval_semantic_lines(interval, navigation = navigation)
}

# Item coefficients describe the declared item set.  Keep this sentence beside
# ordered item displays and opt-in raw intervals so inspection is not mistaken
# for a selection-and-refit procedure covered by the original scale interval.
.bm_fixed_scale_item_note <- paste(
  "Item order describes the declared fixed scale.",
  "Removing items and refitting defines a new analysis; scale-level interval",
  "claims do not carry over."
)

# Denominator statuses worth mentioning, formatted as one short line, or NULL
# when everything is positive.
.bm_status_line <- function(statuses) {
  bad <- statuses[statuses != "positive"]
  if (!length(bad)) return(NULL)
  tab <- table(bad)
  paste0(paste(sprintf("%d %s", as.integer(tab), names(tab)),
               collapse = ", "), " denominator(s)")
}
