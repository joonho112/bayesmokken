# Shared comparison contract for precomputed vignette fits.

bm_cache_normalize <- function(x) {
  if (!is.list(x)) stop("cache wrapper must be a list")
  # Calendar time is intentionally descriptive and is the only volatile field.
  x$built_at <- NULL
  x
}

bm_cache_identical <- function(x, y) {
  identical(bm_cache_normalize(x), bm_cache_normalize(y))
}

bm_cache_difference <- function(x, y) {
  if (bm_cache_identical(x, y)) return(character())
  as.character(all.equal(
    bm_cache_normalize(x),
    bm_cache_normalize(y),
    tolerance = 0,
    check.attributes = TRUE
  ))
}

bm_cache_clone <- function(x) unserialize(serialize(x, NULL, version = 3))
