.bm_test_source_root <- function() {
  test_root <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = TRUE
  )
  candidates <- unique(c(
    Sys.getenv("R_PACKAGE_SOURCE", unset = ""),
    test_root,
    file.path(test_root, "00_pkg_src", "bayesmokken")
  ))
  candidates <- candidates[nzchar(candidates)]
  is_source <- vapply(candidates, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) &&
      dir.exists(file.path(path, "R")) &&
      dir.exists(file.path(path, "inst"))
  }, logical(1))
  if (!any(is_source)) {
    stop("The bayesmokken source tree is unavailable to this test.")
  }
  normalizePath(candidates[which(is_source)[[1L]]], mustWork = TRUE)
}

.bm_test_inst_file <- function(...) {
  source_root <- tryCatch(
    .bm_test_source_root(),
    error = function(e) NULL
  )
  if (!is.null(source_root)) {
    source_path <- file.path(source_root, "inst", ...)
    if (file.exists(source_path)) return(source_path)
  }
  installed_path <- system.file(..., package = "bayesmokken")
  if (!nzchar(installed_path) || !file.exists(installed_path)) {
    stop("The requested bayesmokken installed resource is unavailable.")
  }
  installed_path
}
