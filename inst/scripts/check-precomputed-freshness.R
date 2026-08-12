# =============================================================================
# G-10  Current-source, full-object precomputed cache freshness
#
# Installs this source tree into an isolated temporary library, rebuilds every
# cache with that installation, and compares each normalized wrapper and fit by
# `identical()`. Only `built_at` is volatile. Attributes, dimnames, provenance,
# grouping, summaries, diagnostics, and draw arrays are therefore all covered.
# =============================================================================

source_root <- normalizePath(".", mustWork = TRUE)
description <- read.dcf(file.path(source_root, "DESCRIPTION"))
if (!identical(unname(description[1, "Package"]), "bayesmokken")) {
  stop("run G-10 from the bayesmokken package root")
}
if (!requireNamespace("mokken", quietly = TRUE)) {
  stop("G-10 requires Suggests package 'mokken' to rebuild every shipped cache")
}

shipped_dir <- file.path(source_root, "inst", "extdata", "precomputed")
builder <- file.path(source_root, "data-raw", "precompute-fits.R")
contract <- file.path(source_root, "inst", "scripts", "cache-contract.R")
if (!dir.exists(shipped_dir)) stop("no shipped cache directory: ", shipped_dir)
if (!file.exists(builder)) stop("builder not found: ", builder)
if (!file.exists(contract)) stop("cache comparison contract not found")
source(contract, local = TRUE)

scratch <- tempfile("bayesmokken-cache-freshness-")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
isolated_lib <- file.path(scratch, "library")
rebuilt_dir <- file.path(scratch, "rebuilt")
dir.create(isolated_lib, recursive = TRUE, showWarnings = FALSE)
dir.create(rebuilt_dir, recursive = TRUE, showWarnings = FALSE)

install_output <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-multiarch", "--with-keep.source",
    paste0("--library=", shQuote(isolated_lib)), shQuote(source_root)),
  stdout = TRUE, stderr = TRUE
)
install_status <- attr(install_output, "status")
if (!is.null(install_status) && install_status != 0L) {
  cat(install_output, sep = "\n")
  stop("isolated installation of the current source tree failed")
}

rebuild_output <- system2(
  file.path(R.home("bin"), "Rscript"),
  c("--vanilla", shQuote(builder)),
  env = c(
    paste0("R_LIBS_USER=", shQuote(isolated_lib)),
    paste0("BM_SOURCE_ROOT=", shQuote(source_root)),
    paste0("BM_PRECOMPUTE_OUT=", shQuote(rebuilt_dir))
  ),
  stdout = TRUE, stderr = TRUE
)
rebuild_status <- attr(rebuild_output, "status")
if (!is.null(rebuild_status) && rebuild_status != 0L) {
  cat(rebuild_output, sep = "\n")
  stop("the isolated current-source cache builder failed")
}

shipped <- sort(list.files(shipped_dir, pattern = "\\.rds$"))
rebuilt <- sort(list.files(rebuilt_dir, pattern = "\\.rds$"))
problems <- character()
if (!identical(shipped, rebuilt)) {
  problems <- c(
    problems,
    paste("file set differs; shipped-only:",
          paste(setdiff(shipped, rebuilt), collapse = ", ")),
    paste("file set differs; rebuilt-only:",
          paste(setdiff(rebuilt, shipped), collapse = ", "))
  )
}

for (file in intersect(shipped, rebuilt)) {
  old <- readRDS(file.path(shipped_dir, file))
  new <- readRDS(file.path(rebuilt_dir, file))
  if (!bm_cache_identical(old, new)) {
    detail <- bm_cache_difference(old, new)
    problems <- c(problems, sprintf(
      "%s: %s", file, paste(utils::head(detail, 3L), collapse = " | ")
    ))
  }
}

# Comparator self-test against the exact branches the earlier shallow gate
# missed. A gate whose comparator cannot see these mutations must fail closed.
probe_file <- file.path(rebuilt_dir, "srs_2000.rds")
if (!file.exists(probe_file)) stop("mutation probe cache is missing")
probe <- readRDS(probe_file)
mutations <- list(
  empirical_provenance = function(x) {
    x$fit$empirical$provenance$n_complete <- 999L; x
  },
  grouping = function(x) {
    x$fit$grouping$n_respondents <- 999L; x
  },
  summaries = function(x) {
    x$fit$summaries$scale_draws$mean[1] <- 999; x
  },
  diagnostics = function(x) {
    x$fit$mc_diagnostics$defined_draws <- 999L; x
  },
  draw_dimnames = function(x) {
    colnames(x$fit$draws$item)[1] <- "mutated-item"; x
  }
)
mutation_caught <- vapply(mutations, function(mutate) {
  changed <- mutate(bm_cache_clone(probe))
  !bm_cache_identical(probe, changed)
}, logical(1))
if (!all(mutation_caught)) {
  problems <- c(
    problems,
    paste("comparator missed mutation(s):",
          paste(names(mutation_caught)[!mutation_caught], collapse = ", "))
  )
}

cat("\n== G-10  current-source full-cache freshness =================\n\n")
cat("  isolated install : PASS\n")
cat("  shipped files    :", length(shipped), "\n")
cat("  rebuilt files    :", length(rebuilt), "\n")
cat("  full deep diffs  :", length(problems), "\n")
cat("  mutation probes  :", sum(mutation_caught), "/",
    length(mutation_caught), "caught\n\n")

if (length(problems)) {
  cat(paste0("  - ", problems), sep = "\n")
  cat("\n\n  STATUS: FAIL\n")
  cat("  Rebuild shipped caches from an isolated current-source install.\n\n")
  quit(status = 1)
}

cat("  STATUS: PASS\n\n")
