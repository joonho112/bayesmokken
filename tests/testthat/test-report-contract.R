.report_boundary_fit <- function() {
  mokken_h(
    data.frame(a = rep(0, 6), b = c(0, 1, 0, 1, 1, 0)),
    warn = FALSE
  )
}

.report_adversarial_fit <- function() {
  x <- as.data.frame(expand.grid(rep(list(0:1), 6L)))
  names(x) <- c("a|b", "c&d", "e%f", "g_h", "i#j", "k\\l{m}")
  mokken_h(x, warn = FALSE)
}

test_that("benchmarks expose point arithmetic without interval inversion", {
  fit <- mokken_h(srs, warn = FALSE)
  out <- bm_benchmark(fit, benchmarks = c(0.50, fit$scale$H, 0.30))

  expect_identical(
    names(out),
    c("benchmark", "H", "difference", "point_relation", "note")
  )
  expect_identical(
    vapply(out, class, character(1)),
    c(benchmark = "numeric", H = "numeric", difference = "numeric",
      point_relation = "character", note = "character")
  )
  expect_equal(out$difference, out$H - out$benchmark, tolerance = 0)
  expect_identical(out$point_relation[out$difference == 0], "at")
  expect_false("interval_relation" %in% names(out))
  expect_false(any(grepl(
    "entirely below|entirely above|spans|data place H",
    paste(unlist(out), collapse = " "), ignore.case = TRUE
  )))
})

test_that("undefined benchmark output keeps a stable typed schema", {
  out <- bm_benchmark(.report_boundary_fit())
  expect_true(all(is.na(out$H)))
  expect_true(all(is.na(out$difference)))
  expect_identical(out$point_relation, rep("undefined", nrow(out)))
  expect_true(all(nzchar(out$note)))
  expect_true(all(grepl("^H is undefined; .*unavailable\\.$", out$note)))
  expect_false(anyNA(out$point_relation))
  expect_error(bm_benchmark(.report_boundary_fit(), Inf),
               class = "bm_input_error")
  integer_landmarks <- bm_benchmark(.report_boundary_fit(), c(0L, 1L))
  expect_identical(class(integer_landmarks$benchmark), "numeric")
})

test_that("bm_compare enforces item and row hash comparability", {
  first <- mokken_h(srs[, 1:5], warn = FALSE)
  second <- mokken_h(srs[, 6:10], warn = FALSE)

  expect_error(
    bm_compare(first = first, second = second),
    class = "bm_comparability_error"
  )
  override <- bm_compare(
    first = first, second = second, allow_different = TRUE
  )
  metadata <- attr(override, "comparability")
  expect_identical(metadata$state, "different_items_override_same_rows")
  expect_identical(metadata$item_set, "different_override")
  expect_identical(metadata$rows, "same")
  expect_identical(names(metadata$item_set_hash), c("first", "second"))

  same_items_other_rows <- mokken_h(srs[-1L, 1:5], warn = FALSE)
  expect_warning(
    row_mismatch <- bm_compare(
      first = first, other_rows = same_items_other_rows
    ),
    class = "bm_row_comparability_warning"
  )
  row_metadata <- attr(row_mismatch, "comparability")
  expect_identical(row_metadata$state, "same_items_different_rows")
  expect_identical(row_metadata$item_set, "same")
  expect_identical(row_metadata$rows, "different")
  expect_error(
    bm_compare(first = first, second = first, allow_different = NA),
    class = "bm_input_error"
  )
})

test_that("report escaping covers legal Markdown and LaTeX item labels", {
  values <- c("a|b", "c&d", "e%f", "g_h", "i#j", "k\\l{m}", "$^~<>")
  expect_identical(
    bayesmokken:::.bm_escape_markdown(values),
    c("a\\|b", "c&amp;d", "e%f", "g\\_h", "i\\#j",
      "k\\\\l\\{m\\}", "\\$\\^\\~&lt;&gt;")
  )
  expect_identical(
    bayesmokken:::.bm_escape_latex(values),
    c("a\\textbar{}b", "c\\&d", "e\\%f", "g\\_h", "i\\#j",
      "k\\textbackslash{}l\\{m\\}",
      "\\$\\textasciicircum{}\\textasciitilde{}\\textless{}\\textgreater{}")
  )

  fit <- .report_adversarial_fit()
  markdown <- bm_report(fit, what = "item", format = "markdown")
  markdown_text <- paste(markdown$rendered, collapse = "\n")
  for (label in c("a\\|b", "c&amp;d", "e%f", "g\\_h", "i\\#j",
                  "k\\\\l\\{m\\}")) {
    expect_true(grepl(label, markdown_text, fixed = TRUE), info = label)
  }

  latex <- bm_report(fit, what = "item", format = "latex")
  latex_text <- paste(latex$rendered, collapse = "\n")
  for (label in c("a\\textbar{}b", "c\\&d", "e\\%f", "g\\_h",
                  "i\\#j", "k\\textbackslash{}l\\{m\\}")) {
    expect_true(grepl(label, latex_text, fixed = TRUE), info = label)
  }
})

test_that("deterministic reports never claim an interval or bootstrap", {
  fit <- .report_adversarial_fit()
  for (format in c("data.frame", "markdown", "latex")) {
    report <- bm_report(fit, what = "item", format = format)
    expect_length(report$notes, 0L)
    text <- paste(c(report$notes, report$rendered), collapse = " ")
    expect_false(grepl("Bayesian-bootstrap|confidence interval", text,
                       ignore.case = TRUE))
  }
})

test_that("BibTeX citations contain only paste-ready entries", {
  fit <- mokken_h(srs, warn = FALSE)
  bibtex <- suppressWarnings(bm_cite(fit, format = "bibtex"))
  guidance <- attr(bibtex, "guidance")
  content <- paste(bibtex, collapse = "\n")

  expect_identical(bibtex[[1L]], "@Manual{bayesmokken,")
  expect_identical(tail(bibtex, 1L), "}")
  expect_identical(sum(startsWith(bibtex, "@")), 2L)
  expect_true(grepl("lee2026binary", content, fixed = TRUE))
  expect_false(grepl("lee2026clustered", content, fixed = TRUE))
  expect_true(is.character(guidance) && nzchar(guidance))
  expect_false(grepl(guidance, content, fixed = TRUE))

  text <- suppressWarnings(bm_cite(fit, format = "text"))
  expect_identical(tail(text, 1L), guidance)
  all_bibtex <- suppressWarnings(bm_cite(format = "bibtex"))
  expect_true(any(grepl("lee2026binary", all_bibtex, fixed = TRUE)))
  expect_true(any(grepl("lee2026clustered", all_bibtex, fixed = TRUE)))
})
