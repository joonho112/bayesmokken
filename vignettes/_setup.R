# =============================================================================
# vignettes/_setup.R  --  shared vignette configuration
#
# Sourced by every vignette in its first chunk. Three jobs:
#
#   1. Pin the knitr options so figures are sized and described consistently,
#      and so no vignette can ship a figure without alternative text (G-9).
#   2. Provide bm_cached(), which loads a precomputed fit. Vignettes must not
#      re-run a 50,000-draw bootstrap on every build (decision D-13), but a
#      cached object that has fallen behind the code is worse than a slow one,
#      so the ordinary gate runner's G-10 installs the current source in an
#      isolated library, rebuilds the caches, and deep-diffs them.
#   3. Expose has_mokken. The SWMDK data lives in the mokken package, which is
#      a Suggests. A vignette that needs it must degrade to the cached result
#      rather than fail to build.
#
# Nothing here is part of the public interface.
# =============================================================================

# Raster figures at screen resolution.
#
# Two constraints fix this. SVG would be smaller and sharper, but pkgdown 2.2.0
# rejects `dev = "svglite"` set from a vignette's chunk options, and a site that
# renders differently from the installed vignettes is a maintenance trap worth
# more than the bytes. And html_vignette self-contains its images as base64, so
# every extra pixel lands in the installed package: at fig.retina = 2 the
# visualization vignette alone reaches 1.8 MB against 772 KB here. The pkgdown
# site is unaffected either way, since it writes figures as separate files.
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.4,
  fig.retina = 1,
  dpi = 96,
  out.width = "100%",
  # Figures carry alternative text individually. Anything that reaches this
  # default has none of its own, which is a defect rather than a style choice;
  # the accessibility check reports it.
  fig.alt = "Figure without alternative text -- see inst/scripts/check-accessibility.R"
)

options(width = 82, digits = 7)

set.seed(1)

has_mokken <- requireNamespace("mokken", quietly = TRUE)
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)

# --- cached fits -------------------------------------------------------------

# Load a precomputed fit by key. Keys are the file stems in
# inst/extdata/precomputed: srs_2000, srs_50000, cwb_respondent,
# cwb_equal_cluster, swmdk_{teacher,classmate}_{respondent,equal_cluster}.
#
# Returns the fit itself. The wrapper's metadata (how it was built, what was
# dropped to keep the installed package small) is available through
# bm_cached_note().
bm_cached <- function(key) {
  path <- system.file("extdata", "precomputed", paste0(key, ".rds"),
                      package = "bayesmokken")
  if (!nzchar(path)) {
    stop("no cached fit named '", key, "'. Available: ",
         paste(bm_cached_keys(), collapse = ", "), call. = FALSE)
  }
  readRDS(path)$fit
}

bm_cached_note <- function(key) {
  path <- system.file("extdata", "precomputed", paste0(key, ".rds"),
                      package = "bayesmokken")
  obj <- readRDS(path)
  sprintf(paste0(
    "%s (%s draws kept: %s; built %s under bayesmokken %s; ",
    "analysis %s; source %s)"
  ),
          obj$note, obj$fit$interval$total_draws, obj$keep,
          obj$built_at, obj$package_version,
          substr(obj$analysis_input_hash, 1L, 12L),
          substr(obj$source_tree_hash, 1L, 12L))
}

bm_cached_keys <- function() {
  sub("\\.rds$", "",
      list.files(system.file("extdata", "precomputed", package = "bayesmokken"),
                 pattern = "\\.rds$"))
}

# --- small formatting helpers used in prose ----------------------------------

# Vignette prose states numbers inline rather than typing them out, so the text
# cannot drift from the objects it describes.
fmt_h <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

fmt_ci <- function(interval, digits = 3) {
  sprintf("[%s, %s]",
          fmt_h(interval$lower, digits), fmt_h(interval$upper, digits))
}

# Percentage change in interval width produced by the calibration.
fmt_width_change <- function(interval) {
  raw <- interval$raw_upper - interval$raw_lower
  cal <- interval$upper - interval$lower
  sprintf("%+.1f%%", 100 * (cal / raw - 1))
}
