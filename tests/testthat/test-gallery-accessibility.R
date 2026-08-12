.gallery_package_root <- tryCatch(
  .bm_test_source_root(),
  error = function(e) system.file(package = "bayesmokken")
)

.gallery_png_dimensions <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  header <- readBin(con, what = "raw", n = 24L)
  expect_identical(
    as.integer(header[1:8]),
    c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)
  )
  decode_u32 <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  c(
    width = decode_u32(header[17:20]),
    height = decode_u32(header[21:24])
  )
}

test_that("default captions wrap for the intended output width", {
  widths <- vapply(
    c(5, 7, 9),
    function(width) .bm_caption_wrap_width(device_width = width),
    integer(1)
  )
  expect_identical(widths, c(60L, 88L, 105L))
  expect_true(all(diff(widths) > 0L))

  fit <- bb_mokken_h(srs, draws = 120L, seed = 9102L, warn = FALSE)
  old_options <- options(bayesmokken.figure_width = 5)
  on.exit(options(old_options), add = TRUE)
  five_inch <- bm_plot_item_h(fit, show_interval = TRUE)
  options(bayesmokken.figure_width = 9)
  nine_inch <- bm_plot_item_h(fit, show_interval = TRUE)

  five_lines <- strsplit(
    five_inch$labels$caption, "\n", fixed = TRUE
  )[[1L]]
  nine_lines <- strsplit(
    nine_inch$labels$caption, "\n", fixed = TRUE
  )[[1L]]
  expect_gte(length(five_lines), length(nine_lines))
  expect_lte(max(nchar(five_lines)), widths[[1L]])
  expect_lte(max(nchar(nine_lines)), widths[[3L]])
})

test_that("the accessibility gate executes package checks before vignettes", {
  gate <- .bm_test_inst_file("scripts", "check-accessibility.R")
  gate_text <- readLines(gate, warn = FALSE)
  expect_false(any(grepl("PASS (vacuous)", gate_text, fixed = TRUE)))

  output <- withr::with_dir(
    .gallery_package_root,
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(gate)),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L, info = paste(output, collapse = "\n"))
  expect_true(any(grepl("package plot construction.*PASS", output)))
  expect_true(any(grepl("5-inch caption fit.*PASS", output)))
  expect_true(any(grepl("STATUS: PASS", output, fixed = TRUE)))
})

test_that("checked-in responsive gallery renders carry a passing receipt", {
  gallery <- file.path(.gallery_package_root, "dev", "gallery")
  receipt_path <- file.path(gallery, "accessibility-receipt.txt")
  skip_if_not(file.exists(receipt_path), "source-only gallery is unavailable")

  receipt <- readLines(receipt_path, warn = FALSE)
  expect_true(any(receipt == "STATUS: PASS"))
  expect_true(any(receipt == "caption_contrast_wcag_aa: PASS"))
  expect_identical(
    sum(grepl("^responsive_[579]in:.*horizontal_fit=PASS$", receipt)),
    3L
  )

  png_dir <- file.path(gallery, "png")
  dimensions <- lapply(c(5, 7, 9), function(width) {
    .gallery_png_dimensions(
      file.path(png_dir, sprintf("resp-item-h-%din.png", width))
    )
  })
  expect_identical(
    vapply(dimensions, `[[`, numeric(1), "width"),
    c(550, 770, 990)
  )
  expect_identical(
    vapply(dimensions, `[[`, numeric(1), "height"),
    c(572, 572, 572)
  )
  expect_true(file.exists(file.path(png_dir, "11-headroom-binary.png")))
  expect_false(file.exists(file.path(png_dir, "11-gap-binary.png")))
})
