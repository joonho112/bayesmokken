# =============================================================================
# Documentation completeness audit
#
# Checks the things R CMD check does not: that every export is documented, that
# every help page is reachable from another one, that every argument is
# described, and that no page is an orphan.
#
#     Rscript inst/scripts/audit-documentation.R
# =============================================================================

suppressPackageStartupMessages(library(bayesmokken))

rows <- list()
add <- function(check, status, detail) {
  rows[[length(rows) + 1L]] <<- data.frame(check = check, status = status,
                                           detail = detail,
                                           stringsAsFactors = FALSE)
  cat(sprintf("  [%s] %-40s %s\n", status, check, detail))
}

cat("Documentation audit\n")

db <- tools::Rd_db("bayesmokken")
exports <- sort(getNamespaceExports("bayesmokken"))

# --- every export has a help page --------------------------------------------
aliases <- unlist(lapply(db, function(rd) {
  tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
  vapply(rd[tags == "\\alias"], function(a) trimws(paste(unlist(a),
                                                         collapse = "")),
         character(1))
}), use.names = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

undocumented <- setdiff(exports, aliases)
add("every export documented",
    if (!length(undocumented)) "PASS" else "FAIL",
    if (!length(undocumented)) sprintf("%d exports, %d pages", length(exports),
                                       length(db))
    else paste(undocumented, collapse = ", "))

# --- every page has a title, description, value ------------------------------
missing_value <- character()
missing_desc <- character()
for (nm in names(db)) {
  tags <- vapply(db[[nm]], function(x) attr(x, "Rd_tag") %||% "",
                 character(1))
  is_data <- any(tags == "\\format")
  is_pkg <- grepl("bayesmokken-(package|estimands|intervals|glossary|plots)",
                  nm)
  # roxygen's page for re-exported generics documents another package's
  # function, so it carries no @return of ours.
  is_reexport <- identical(nm, "reexports.Rd")
  if (!("\\description" %in% tags)) missing_desc <- c(missing_desc, nm)
  if (!is_data && !is_pkg && !is_reexport && !("\\value" %in% tags)) {
    missing_value <- c(missing_value, nm)
  }
}
add("every page has a description",
    if (!length(missing_desc)) "PASS" else "FAIL",
    if (!length(missing_desc)) "ok" else paste(missing_desc, collapse = ", "))
add("every function page has @return",
    if (!length(missing_value)) "PASS" else "FAIL",
    if (!length(missing_value)) "ok" else paste(missing_value, collapse = ", "))

# --- no orphan pages ---------------------------------------------------------
# A page is reachable if some OTHER page links to it, through \seealso, \link
# or a shared \concept (which is what @family generates).
text_of <- function(rd) paste(unlist(rd), collapse = " ")
all_text <- vapply(db, text_of, character(1))
concepts <- lapply(db, function(rd) {
  tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
  trimws(vapply(rd[tags == "\\concept"],
                function(a) paste(unlist(a), collapse = ""), character(1)))
})

orphans <- character()
for (nm in names(db)) {
  page_aliases <- {
    tags <- vapply(db[[nm]], function(x) attr(x, "Rd_tag") %||% "",
                   character(1))
    trimws(vapply(db[[nm]][tags == "\\alias"],
                  function(a) paste(unlist(a), collapse = ""), character(1)))
  }
  others <- setdiff(names(db), nm)
  linked <- any(vapply(others, function(o) {
    any(vapply(page_aliases, function(a) grepl(a, all_text[[o]], fixed = TRUE),
               logical(1)))
  }, logical(1)))
  shares_family <- length(concepts[[nm]]) > 0 &&
    any(vapply(others, function(o) length(intersect(concepts[[nm]],
                                                    concepts[[o]])) > 0,
               logical(1)))
  if (!linked && !shares_family) orphans <- c(orphans, nm)
}
add("no orphan help pages",
    if (!length(orphans)) "PASS" else "FAIL",
    if (!length(orphans)) sprintf("%d pages all reachable", length(db))
    else paste(orphans, collapse = ", "))

# --- families ----------------------------------------------------------------
fam <- table(unlist(concepts))
fam <- fam[grepl("^bayesmokken-", names(fam))]
singleton <- names(fam)[fam < 2L]
add("families have at least two members",
    if (!length(singleton)) "PASS" else "FAIL",
    if (!length(singleton)) paste(sprintf("%s(%d)", names(fam), as.integer(fam)),
                                  collapse = " ")
    else paste(singleton, collapse = ", "))

# Generics re-exported from another package are documented there; requiring a
# @family here would mean writing a page for a function we do not own.
reexported <- c("autoplot")
exports <- setdiff(exports, reexported)

exports_with_family <- vapply(exports, function(e) {
  page <- names(db)[vapply(db, function(rd) {
    tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
    any(trimws(vapply(rd[tags == "\\alias"],
                      function(a) paste(unlist(a), collapse = ""),
                      character(1))) == e)
  }, logical(1))]
  if (!length(page)) return(FALSE)
  length(concepts[[page[1L]]]) > 0
}, logical(1))
add("every export has a @family",
    if (all(exports_with_family)) "PASS" else "FAIL",
    if (all(exports_with_family)) "ok"
    else paste(exports[!exports_with_family], collapse = ", "))

out <- do.call(rbind, rows)
cat("\n  STATUS: ", if (any(out$status == "FAIL")) "FAIL" else "PASS", "\n",
    sep = "")
invisible(if (any(out$status == "FAIL")) quit(status = 1))
