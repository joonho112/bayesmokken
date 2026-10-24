test_that("complete-record RNG rejects unsupported caller generators", {
  validate <- bayesmokken:::.bm_validate_archive_rng_kind
  expect_error(validate(c("user-supplied", "Inversion", "Rejection")),
               "User-supplied", class = "bm_input_error")
  expect_error(validate(c("Mersenne-Twister", "user-supplied", "Rejection")),
               "User-supplied", class = "bm_input_error")
  expect_error(validate(c("Mersenne-Twister", "Box-Muller", "Rejection")),
               "Box-Muller", class = "bm_input_error")
  expect_true(validate(c("Mersenne-Twister", "Inversion", "Rejection")))
})

test_that("a subprocess preserves future normal output on rejected and valid calls", {
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    'root <- commandArgs(TRUE)[1L]',
    'e <- new.env(parent = baseenv())',
    'for (f in c("validate-data.R","conditions.R","rng-streams.R","cluster-weights.R"))',
    '  sys.source(file.path(root, "R", f), envir = e)',
    'setup <- function(normal) { RNGkind("Mersenne-Twister",normal,"Rejection"); set.seed(123); invisible(rnorm(1)) }',
    'setup("Box-Muller"); expected <- rnorm(3)',
    'setup("Box-Muller"); before <- .Random.seed; kind <- RNGkind(); evaluated <- FALSE',
    'error <- tryCatch({e$.bm_with_archive_rng(7, {evaluated <- TRUE; rexp(1)}); NULL}, error = identity)',
    'stopifnot(inherits(error,"bm_input_error"), grepl("Box-Muller",conditionMessage(error)),',
    '          !evaluated, identical(before,.Random.seed), identical(kind,RNGkind()),',
    '          identical(expected,rnorm(3)))',
    'setup("Inversion"); expected <- rnorm(3)',
    'setup("Inversion"); before <- .Random.seed; kind <- RNGkind()',
    'invisible(e$.bm_with_archive_rng(7, rexp(10)))',
    'stopifnot(identical(before,.Random.seed), identical(kind,RNGkind()), identical(expected,rnorm(3)))',
    'rm(".Random.seed", envir = .GlobalEnv)',
    'invisible(e$.bm_with_archive_rng(7, rexp(10)))',
    'stopifnot(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))',
    'cat("FUTURE_RNG_STATE_PASS\\n")'
  ), script)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), shQuote(.bm_test_source_root())),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L, info = paste(output, collapse = "\n"))
  expect_true(any(output == "FUTURE_RNG_STATE_PASS"))
})
