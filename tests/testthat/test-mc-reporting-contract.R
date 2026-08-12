test_that("the copy-ready three-decimal sentence uses an MC-resolved cache", {
  cache <- function(key) {
    readRDS(system.file(
      "extdata", "precomputed", paste0(key, ".rds"),
      package = "bayesmokken"
    ))$fit
  }
  fit_2000 <- cache("srs_2000")
  fit_50000 <- cache("srs_50000")
  diagnostics <- bm_mc_diagnostics(fit_50000)
  reporting_unit <- 1e-3

  expect_identical(fit_50000$interval$total_draws, 50000L)
  expect_lt(
    max(diagnostics$lower_endpoint_mcse,
        diagnostics$upper_endpoint_mcse),
    0.5 * reporting_unit
  )
  expect_identical(
    round(fit_50000$interval$lower, 3),
    0.103
  )
  # This is the reviewed counterexample: 2,000 draws advertise another last
  # digit, which is why the copy-ready sentence does not use that fit.
  expect_identical(round(fit_2000$interval$lower, 3), 0.104)
})
