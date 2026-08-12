# =============================================================================
# G-7  Identity correctness
#
# INV-7: the author's name, email, ORCID, affiliation and repository must be
# right everywhere they appear. v0 shipped a placeholder address
# (joonho.lee@example.org); that class of error is what this gate exists to
# make impossible to repeat.
#
#     Rscript inst/scripts/check-identity.R
# =============================================================================

required <- c(
  name  = "JoonHo Lee",
  email = "jlee296@ua.edu",
  orcid = "0009-0006-4019-8703",
  repo  = "github.com/joonho112/bayesmokken"
)
placeholders <- c("example.org", "example.com", "Joonho Lee",
                  "your.email@", "TODO", "FIXME", "XXX-XXXX")

files <- Filter(file.exists, c(
  "DESCRIPTION", "LICENSE", "LICENSE.md", "inst/CITATION",
  "README.md", "README.Rmd", "NEWS.md", "_pkgdown.yml"
))
files <- c(files, list.files("R", pattern = "\\.R$", full.names = TRUE),
           list.files("vignettes", pattern = "\\.(Rmd|qmd)$",
                      full.names = TRUE))

rows <- list()
add <- function(check, status, detail) {
  rows[[length(rows) + 1L]] <<- data.frame(
    check = check, status = status, detail = detail, stringsAsFactors = FALSE
  )
  cat(sprintf("  [%s] %-42s %s\n", status, check, detail))
}

cat("G-7 identity check\n")

# --- 1. DESCRIPTION carries every required field ------------------------------
# The author name lives inside person("JoonHo", "Lee", ...), so it is checked
# against the PARSED person below (step 4), not against the raw file text.
d <- paste(readLines("DESCRIPTION", warn = FALSE), collapse = "\n")
for (k in c("email", "orcid", "repo")) {
  add(paste0("DESCRIPTION has ", k),
      if (grepl(required[[k]], d, fixed = TRUE)) "PASS" else "FAIL",
      required[[k]])
}

# --- 2. LICENSE copyright holder ---------------------------------------------
lic <- paste(readLines("LICENSE", warn = FALSE), collapse = "\n")
add("LICENSE copyright holder",
    if (grepl("JoonHo Lee", lic, fixed = TRUE)) "PASS" else "FAIL",
    trimws(gsub("\n", " | ", lic)))

# --- 3. no placeholders anywhere ---------------------------------------------
bad <- list()
for (f in files) {
  ln <- readLines(f, warn = FALSE)
  for (p in placeholders) {
    i <- grep(p, ln, fixed = TRUE)
    # "Joonho Lee" is only wrong when it is NOT part of "JoonHo Lee"
    if (identical(p, "Joonho Lee")) i <- i[!grepl("JoonHo Lee", ln[i])]
    if (length(i)) {
      bad[[length(bad) + 1L]] <- data.frame(file = f, line = i, pattern = p,
                                            stringsAsFactors = FALSE)
    }
  }
}
bad <- if (length(bad)) do.call(rbind, bad) else NULL
add("no placeholder strings",
    if (is.null(bad)) "PASS" else "FAIL",
    if (is.null(bad)) "0 hits" else paste(nrow(bad), "hits"))
if (!is.null(bad)) print(bad, row.names = FALSE)

# --- 4. Authors@R parses and carries the ORCID comment -----------------------
a <- tryCatch(eval(parse(text = read.dcf("DESCRIPTION")[1, "Authors@R"])),
              error = function(e) NULL)
add("Authors@R parses", if (is.null(a)) "FAIL" else "PASS",
    if (is.null(a)) "parse error" else format(a))
if (!is.null(a)) {
  add("Authors@R name",
      if (identical(paste(a$given, a$family), required[["name"]])) "PASS" else "FAIL",
      paste(a$given, a$family))
  add("Authors@R email",
      if (identical(a$email, required[["email"]])) "PASS" else "FAIL", a$email)
  cm <- unlist(a$comment)
  add("Authors@R ORCID",
      if (identical(unname(cm[["ORCID"]]), required[["orcid"]])) "PASS" else "FAIL",
      paste(cm, collapse = " "))
  add("Authors@R roles",
      if (all(c("aut", "cre") %in% a$role)) "PASS" else "FAIL",
      paste(a$role, collapse = ","))
}

out <- do.call(rbind, rows)
cat("\n  STATUS: ", if (any(out$status == "FAIL")) "FAIL" else "PASS", "\n",
    sep = "")
invisible(if (any(out$status == "FAIL")) quit(status = 1))
