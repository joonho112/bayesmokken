# =============================================================================
# Alternative text for reference-page example figures
#
# pkgdown 2.2.0 renders figures produced by @examples as
#
#     <span class="r-plt img"><img src="topic-1.png" alt="" ...></span>
#
# with an empty alt attribute, and offers no per-figure mechanism to fill it.
# Those figures are informative -- they show what a function draws -- so an
# empty alt means a screen-reader user is told nothing at all about them.
#
# (The logo's empty alt is left alone. It sits beside the package name in text
# and conveys nothing further, which is exactly the case where an empty alt is
# the correct choice rather than a defect.)
#
# This script fills the example figures in from the page's own Rd title, which
# is the most specific description available without hand-writing 18 strings
# that would drift the moment an example changed. Run it after build_site():
#
#     Rscript inst/scripts/inject-reference-alt.R
# =============================================================================

ref_dir <- file.path("docs", "reference")
if (!dir.exists(ref_dir)) {
  stop("no built site at ", ref_dir, "; run pkgdown::build_site() first")
}

files <- list.files(ref_dir, pattern = "[.]html$", full.names = TRUE)
files <- setdiff(files, file.path(ref_dir, "index.html"))

pattern <- '(<span class="r-plt img"><img src="([^"]+)\\.png")( alt="")'

total <- 0L
touched <- character()

for (f in files) {
  page <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (regexpr(pattern, page, perl = TRUE) == -1L) next

  # "Plot the draws ... - bm_plot_draws * bayesmokken" -> "Plot the draws ..."
  title <- sub("^.*<title>(.*?)</title>.*$", "\\1", page)
  title <- trimws(strsplit(title, "—|•")[[1]][[1]])
  topic <- sub("[.]html$", "", basename(f))

  # One substitution at a time, so each figure gets its own index.
  n <- 0L
  repeat {
    m <- regexpr(pattern, page, perl = TRUE)
    if (m == -1L) break
    n <- n + 1L
    alt <- sprintf("Example figure %d from %s(): %s.", n, topic, title)
    alt <- gsub('"', "&quot;", alt, fixed = TRUE)
    matched <- regmatches(page, m)
    head_part <- sub(' alt=""$', "", matched)
    regmatches(page, m) <- sprintf('%s alt="%s"', head_part, alt)
  }

  writeLines(strsplit(page, "\n", fixed = TRUE)[[1]], f, useBytes = TRUE)
  total <- total + n
  touched <- c(touched, basename(f))
}

cat("\n== reference example alt text ================================\n\n")
cat("  pages touched  :", length(touched), "\n")
cat("  figures filled :", total, "\n")
if (length(touched)) {
  cat("  ", paste(touched, collapse = ", "), "\n", sep = "")
}
cat("\n")
