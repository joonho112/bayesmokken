.language_package_root <- .bm_test_source_root()

.language_scanner <- local({
  environment <- new.env(parent = globalenv())
  sys.source(
    .bm_test_inst_file("scripts", "scan-forbidden-language.R"),
    envir = environment
  )
  environment
})

.scan_language_package <- function() {
  targets <- .language_scanner$bm_language_targets(.language_package_root)
  whitelist <- .language_scanner$bm_read_language_whitelist(file.path(
    .language_package_root, "inst", "extdata", "language-whitelist.txt"
  ))
  # R CMD build intentionally omits source-only authoring files such as
  # README.Rmd. The source-tree gate remains strict; installed-tarball tests
  # validate exactly the targets that survived into 00_pkg_src.
  whitelist <- whitelist[whitelist$path %in% targets, , drop = FALSE]
  .language_scanner$bm_scan_forbidden_language(
    root = .language_package_root,
    targets = targets,
    whitelist = whitelist
  )
}

.single_whitelisted_file <- function() {
  root <- tempfile("language-firewall-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  file.copy(
    file.path(.language_package_root, "R", "bb-interval.R"),
    file.path(root, "R", "bb-interval.R")
  )
  whitelist <- .language_scanner$bm_read_language_whitelist(
    file.path(
      .language_package_root,
      "inst", "extdata", "language-whitelist.txt"
    )
  )
  whitelist <- whitelist[
    whitelist$path == "R/bb-interval.R",
    ,
    drop = FALSE
  ]
  list(
    root = root,
    targets = "R/bb-interval.R",
    whitelist = whitelist
  )
}

test_that("genuine negations pass the exact-sentence firewall", {
  result <- .scan_language_package()

  expect_identical(result$status, 0L)
  expect_identical(nrow(result$issues), 0L)
  expect_true("DESCRIPTION" %in% result$targets)
  expect_true(any(result$hits$path == "DESCRIPTION"))
  expect_identical(nrow(result$hits), nrow(result$whitelist))
})

test_that("a new endorsement in a whitelisted file is not immunized", {
  fixture <- .single_whitelisted_file()
  baseline <- .language_scanner$bm_scan_forbidden_language(
    root = fixture$root,
    targets = fixture$targets,
    whitelist = fixture$whitelist
  )
  expect_identical(baseline$status, 0L)

  path <- file.path(fixture$root, "R", "bb-interval.R")
  lines <- readLines(path, warn = FALSE)
  writeLines(
    c(lines, "# The credible interval proves the scale is acceptable."),
    path
  )
  mutated <- .language_scanner$bm_scan_forbidden_language(
    root = fixture$root,
    targets = fixture$targets,
    whitelist = fixture$whitelist
  )

  expect_identical(mutated$status, 1L)
  expect_true("UNWHITELISTED_HIT" %in% mutated$issues$type)
  endorsement <- mutated$hits[
    grepl("proves the scale", mutated$hits$text, fixed = TRUE),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(endorsement), 0L)
  expect_false(any(endorsement$key %in% paste(
    fixture$whitelist$path,
    fixture$whitelist$pattern,
    fixture$whitelist$sentence_hash,
    sep = "::"
  )))
})

test_that("duplicate, unused, stale, hash, and count defects all fail", {
  fixture <- .single_whitelisted_file()
  scan <- function(whitelist) {
    .language_scanner$bm_scan_forbidden_language(
      root = fixture$root,
      targets = fixture$targets,
      whitelist = whitelist
    )
  }

  duplicated <- rbind(fixture$whitelist, fixture$whitelist[1L, ])
  expect_true(
    "DUPLICATE_WHITELIST_KEY" %in% scan(duplicated)$issues$type
  )

  unused_row <- fixture$whitelist[1L, ]
  unused_row$pattern <- "posterior_predictive"
  unused_row$sentence_hash <- paste(rep("a", 64L), collapse = "")
  unused <- rbind(fixture$whitelist, unused_row)
  expect_true("UNUSED_WHITELIST_ENTRY" %in% scan(unused)$issues$type)

  stale <- fixture$whitelist
  stale$path[1L] <- "R/removed-source.R"
  expect_true("STALE_WHITELIST_ENTRY" %in% scan(stale)$issues$type)

  mismatched <- fixture$whitelist
  mismatched$sentence_hash[1L] <- paste(rep("b", 64L), collapse = "")
  expect_true("HASH_MISMATCH" %in% scan(mismatched)$issues$type)

  wrong_count <- fixture$whitelist
  wrong_count$expected_count[1L] <- "2"
  expect_true(
    "EXPECTED_COUNT_MISMATCH" %in% scan(wrong_count)$issues$type
  )
})

test_that("generated Rd exemptions trace to live whitelisted source", {
  result <- .scan_language_package()
  rd <- startsWith(result$whitelist$path, "man/")

  expect_true(all(nzchar(result$whitelist$source_path[rd])))
  expect_true(all(startsWith(result$whitelist$source_path[rd], "R/")))
  expect_true(all(nzchar(result$whitelist$source_hash[rd])))

  broken <- result$whitelist
  row <- which(rd)[1L]
  broken$source_hash[row] <- paste(rep("c", 64L), collapse = "")
  rescanned <- .language_scanner$bm_scan_forbidden_language(
    root = .language_package_root,
    whitelist = broken
  )
  expect_identical(rescanned$status, 1L)
  expect_true("INVALID_RD_SOURCE_TRACE" %in% rescanned$issues$type)
})

test_that("selection and benchmark regressions are source-scanned", {
  root <- tempfile("language-semantics-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c(
      "# Show the weakest item first.",
      "# The interval is entirely above the benchmark.",
      "# Which items carry the scale?",
      "# Which pairs are doing the work?",
      "# The interval lies below all three landmarks.",
      "# The position is not in question."
    ),
    file.path(root, "R", "regression.R")
  )
  result <- .language_scanner$bm_scan_forbidden_language(
    root = root,
    targets = "R/regression.R",
    whitelist = .language_scanner$bm_empty_language_whitelist()
  )

  expect_identical(result$status, 1L)
  expect_true("selection_affordance" %in% result$hits$pattern)
  expect_true("benchmark_classification" %in% result$hits$pattern)
  expect_true("interval_landmark_classification" %in% result$hits$pattern)
  expect_true(all(result$issues$type == "UNWHITELISTED_HIT"))
})

test_that("sentence hashes normalize case and whitespace only", {
  hash <- .language_scanner$bm_sentence_hash
  expect_identical(
    hash("  Not a posterior credible interval. "),
    hash("not  a POSTERIOR credible interval.")
  )
  expect_false(identical(
    hash("Not a posterior credible interval."),
    hash("A posterior credible interval.")
  ))
})
