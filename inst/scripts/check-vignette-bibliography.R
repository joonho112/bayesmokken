# Audit the small bibliography used by the ten vignettes. This is deliberately
# source-based: it verifies citation coverage and the two identifiers corrected
# during documentation review #2 without claiming a live metadata service.

args <- commandArgs(trailingOnly = TRUE)
root_arg <- grep("^--root=", args, value = TRUE)
root <- if (length(root_arg)) sub("^--root=", "", root_arg[[1]]) else "."
root <- normalizePath(root, mustWork = TRUE)
bib_path <- file.path(root, "vignettes", "references.bib")
if (!file.exists(bib_path)) stop("vignette bibliography not found")

bib <- readLines(bib_path, warn = FALSE)
entry_start <- grep("^@[[:alpha:]]+\\{[^,]+,", bib)
keys <- sub("^@[[:alpha:]]+\\{([^,]+),.*$", "\\1", bib[entry_start])
if (anyDuplicated(keys)) stop("duplicate bibliography key(s): ",
                              paste(unique(keys[duplicated(keys)]), collapse = ", "))

entry_block <- function(key) {
  start <- grep(paste0("^@[[:alpha:]]+\\{", key, ","), bib)
  if (length(start) != 1L) stop("expected exactly one entry for ", key)
  next_start <- entry_start[entry_start > start]
  end <- if (length(next_start)) next_start[[1]] - 1L else length(bib)
  paste(bib[start:end], collapse = "\n")
}

caliciuri <- entry_block("caliciuri_psychometric_2024")
if (!grepl("doi[[:space:]]*=[[:space:]]*\\{10\\.17605/OSF\\.IO/JK9DP\\}",
           caliciuri, ignore.case = TRUE)) {
  stop("Caliciuri OSF entry does not carry DOI 10.17605/OSF.IO/JK9DP")
}
if (!grepl("url[[:space:]]*=[[:space:]]*\\{https://osf\\.io/jk9dp/\\}",
           caliciuri, ignore.case = TRUE)) {
  stop("Caliciuri OSF entry does not point to https://osf.io/jk9dp/")
}
if (grepl("10\\.13140/RG", caliciuri, ignore.case = TRUE)) {
  stop("ResearchGate poster DOI is still mixed into the OSF entry")
}

provenance_sources <- c(
  file.path(root, "R", "data.R"),
  file.path(root, "data-raw", "srs.R"),
  file.path(root, "inst", "extdata", "DATA-LICENSES.md")
)
provenance_text <- paste(
  unlist(lapply(provenance_sources, readLines, warn = FALSE)),
  collapse = "\n"
)
if (grepl("10\\.13140/RG", provenance_text, ignore.case = TRUE)) {
  stop("ResearchGate poster DOI remains in an SRS provenance source")
}
if (!all(vapply(provenance_sources, function(path) {
  any(grepl("10.17605/OSF.IO/JK9DP", readLines(path, warn = FALSE),
            fixed = TRUE))
}, logical(1)))) {
  stop("SRS provenance sources do not consistently carry the OSF DOI")
}

lo <- entry_block("lo_large_1987")
if (!grepl("10.1214/aos/1176350271", lo, fixed = TRUE)) {
  stop("Lo (1987) primary DOI is missing")
}

sources <- list.files(file.path(root, "vignettes"),
                      pattern = "^[am][1-5].*\\.Rmd$", full.names = TRUE)
text <- paste(unlist(lapply(sources, readLines, warn = FALSE)), collapse = "\n")
matches <- regmatches(text, gregexpr("@[A-Za-z0-9_:-]+", text, perl = TRUE))[[1]]
cited <- sort(unique(sub("^@", "", matches)))
missing <- setdiff(cited, keys)
if (length(missing)) stop("missing cited bibliography key(s): ",
                          paste(missing, collapse = ", "))

cat("Vignette bibliography audit\n")
cat("  bibliography entries :", length(keys), "\n")
cat("  cited keys           :", length(cited), "\n")
cat("  missing citations    :", length(missing), "\n")
cat("  Caliciuri OSF DOI    : PASS\n")
cat("  SRS provenance DOI   : PASS\n")
cat("  Lo primary DOI       : PASS\n")
cat("  STATUS               : PASS\n")
