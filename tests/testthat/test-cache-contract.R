test_that("cache comparison ignores only build time", {
  path <- system.file("scripts", "cache-contract.R", package = "bayesmokken")
  expect_true(nzchar(path))
  source(path, local = TRUE)

  base <- list(
    fit = structure(
      list(
        empirical = list(provenance = list(n_complete = 10L)),
        grouping = list(n_respondents = 10L),
        summaries = list(value = 1),
        diagnostics = list(value = 2),
        draws = matrix(1:4, 2, dimnames = list(NULL, c("a", "b")))
      ),
      class = "bm_bb_h"
    ),
    built_at = "2026-08-10",
    builder_hash = "builder",
    source_tree_hash = "source"
  )
  later <- bm_cache_clone(base)
  later$built_at <- "2026-08-11"
  expect_true(bm_cache_identical(base, later))

  mutations <- list(
    function(x) { x$fit$empirical$provenance$n_complete <- 999L; x },
    function(x) { x$fit$grouping$n_respondents <- 999L; x },
    function(x) { x$fit$summaries$value <- 999; x },
    function(x) { x$fit$diagnostics$value <- 999; x },
    function(x) { colnames(x$fit$draws)[1] <- "changed"; x },
    function(x) { x$builder_hash <- "changed"; x },
    function(x) { x$source_tree_hash <- "changed"; x }
  )
  for (mutate in mutations) {
    changed <- mutate(bm_cache_clone(base))
    expect_false(bm_cache_identical(base, changed))
    expect_true(length(bm_cache_difference(base, changed)) >= 1L)
  }
})
