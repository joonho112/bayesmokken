# =============================================================================
# data-raw/precompute-fits.R  --  build inst/extdata/precomputed/*.rds
#
# Vignettes and pkgdown must not re-run every Bayesian bootstrap on every
# build. These cached fits let a vignette show a real result instantly, and
# let the SWMDK sections render on machines where the mokken package is not
# installed.
#
# SIZE POLICY
#   CRAN asks that an installed package stay under about 5 MB, and the draw
#   arrays are what threaten that: pair draws grow as choose(J, 2) * B, so the
#   50,000-draw SRS fit alone would carry roughly 22 MB of pair draws.
#   Each entry therefore declares how much of its draw structure to keep:
#     "full"  keep scale, item and pair draws
#     "lite"  keep scale and item draws, drop pair draws
#     "scale" keep scale draws only
#   Whatever is dropped is replaced by a note saying so, so a cached object can
#   never be mistaken for a complete one.
#
# Run from the package root:
#     Rscript data-raw/precompute-fits.R
# =============================================================================

suppressPackageStartupMessages(library(bayesmokken))

source_root <- normalizePath(
  Sys.getenv("BM_SOURCE_ROOT", unset = "."), mustWork = TRUE
)
builder_path <- file.path(source_root, "data-raw", "precompute-fits.R")
if (!file.exists(builder_path)) stop("cannot locate cache builder in source tree")

source_files <- c(
  "DESCRIPTION", "NAMESPACE",
  sort(list.files(file.path(source_root, "R"), pattern = "\\.[Rr]$",
                  full.names = FALSE))
)
source_paths <- c(
  file.path(source_root, source_files[1:2]),
  file.path(source_root, "R", source_files[-c(1:2)])
)
source_manifest <- stats::setNames(
  vapply(source_paths, function(path) {
    digest::digest(file = path, algo = "sha256", serialize = FALSE)
  }, character(1)),
  source_files
)
source_tree_hash <- digest::digest(
  source_manifest, algo = "sha256", serialize = TRUE
)
builder_hash <- digest::digest(
  file = builder_path, algo = "sha256", serialize = FALSE
)

# The freshness gate (inst/scripts/check-precomputed-freshness.R) rebuilds into
# a temporary directory and diffs the result against the shipped files, so this
# script must be able to write somewhere other than the package.
out_dir <- Sys.getenv("BM_PRECOMPUTE_OUT",
                      unset = file.path("inst", "extdata", "precomputed"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

trim <- function(fit, keep = c("full", "lite", "scale")) {
  keep <- match.arg(keep)
  dropped <- character()
  if (keep != "full") {
    fit$draws$pair <- NULL
    dropped <- c(dropped, "pair")
  }
  if (keep == "scale") {
    fit$draws$item <- NULL
    dropped <- c(dropped, "item")
  }
  if (length(dropped)) {
    fit$draws$dropped_for_size <- dropped
    fit$summaries$pair_draws <- NULL
    if (keep == "scale") fit$summaries$item_draws <- NULL
  }
  fit
}

save_fit <- function(fit, key, keep, note) {
  obj <- list(
    fit = trim(fit, keep),
    key = key,
    keep = keep,
    note = note,
    built_at = format(Sys.time(), "%Y-%m-%d"),
    package_version = as.character(utils::packageVersion("bayesmokken")),
    r_version = R.version.string,
    analysis_input_hash = fit$empirical$provenance$analysis_input_hash,
    builder_hash = builder_hash,
    source_tree_hash = source_tree_hash
  )
  path <- file.path(out_dir, paste0(key, ".rds"))
  saveRDS(obj, path, version = 3, compress = "xz")
  message(sprintf("  %-18s %-6s %9s bytes", key, keep,
                  format(file.size(path), big.mark = ",")))
  invisible(path)
}

message("Building precomputed fits...")

# --- SRS ---------------------------------------------------------------------
data("srs", package = "bayesmokken", envir = environment())

save_fit(bb_mokken_h(srs, draws = 2000L, seed = 1L, warn = FALSE),
         "srs_2000", "lite",
         "SRS, 2,000 draws, seed 1. The everyday example fit.")

# The article's own setting. Scale draws only: this object exists so a vignette
# can show the published interval without a two-minute bootstrap, and pair and
# item draws at 50,000 would dominate the installed size.
save_fit(bb_mokken_h(srs, draws = 50000L, seed = 2026072611L, warn = FALSE),
         "srs_50000", "scale",
         paste("SRS at the article's draw count and seed. Note that the",
               "article's own draws came from the research harness, not from",
               "bb_mokken_h(), so the endpoints agree to Monte Carlo error",
               "rather than exactly. See inst/scripts/verify-published-numbers.R."))

# --- classroom_wb ------------------------------------------------------------
data("classroom_wb", package = "bayesmokken", envir = environment())
cwb_items <- paste0("wb", 1:6)
cwb_sup <- stats::setNames(rep(list(1:5), length(cwb_items)), cwb_items)
cwb_cl <- as.character(classroom_wb$class_id)

for (tg in c("respondent", "equal_cluster")) {
  save_fit(
    bb_mokken_h(classroom_wb[, cwb_items], item_type = "ordinal",
                supports = cwb_sup, cluster = cwb_cl, cluster_target = tg,
                weight_law = "hierarchical_cluster",
                draws = 2000L, seed = 1L, warn = FALSE),
    paste0("cwb_", tg), "lite",
    paste0("Synthetic classroom data, ", tg, " target, 2,000 draws, seed 1.")
  )
}

# --- SWMDK (only when mokken is available) -----------------------------------
if (requireNamespace("mokken", quietly = TRUE)) {
  utils::data("SWMDK", package = "mokken", envir = environment())
  swmdk <- get("SWMDK", envir = environment())
  scales <- list(teacher = paste0("Item", 1:6),
                 classmate = c("Item8", "Item9", "Item10", "Item11", "Item13"))
  for (sc in names(scales)) {
    it <- scales[[sc]]
    sup <- stats::setNames(rep(list(1:5), length(it)), it)
    for (tg in c("respondent", "equal_cluster")) {
      save_fit(
        bb_mokken_h(swmdk[, it], item_type = "ordinal", supports = sup,
                    cluster = as.character(swmdk$classId), cluster_target = tg,
                    weight_law = "hierarchical_cluster",
                    draws = 2000L, seed = 1L, warn = FALSE),
        paste0("swmdk_", sc, "_", tg), "lite",
        paste0("mokken::SWMDK ", sc, " scale, ", tg, " target, 2,000 draws, ",
               "seed 1. Cached so the vignette renders without mokken; the ",
               "published analysis used 99,999 draws from the research ",
               "harness, so these endpoints differ by Monte Carlo error.")
      )
    }
  }
} else {
  message("  mokken not installed; SWMDK fits skipped")
}

total <- sum(file.size(list.files(out_dir, full.names = TRUE)))
message(sprintf("\nprecomputed total: %s bytes (%.2f MB)",
                format(total, big.mark = ","), total / 1024^2))
if (total > 3 * 1024^2) {
  warning("Precomputed cache exceeds 3 MB; tighten the keep levels.")
}
