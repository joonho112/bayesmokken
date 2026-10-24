# =============================================================================
# Gate runner -- G-1 through G-10
#
# Run at the end of every phase. Paste the printed table into that phase's log
# (section 3). A gate that cannot run reports SKIP with a reason; it never
# silently passes.
#
#     Rscript inst/scripts/run-all-gates.R
#     Rscript inst/scripts/run-all-gates.R --published   # + the slow gate
#     Rscript inst/scripts/run-all-gates.R --published --lib <lib>
#                     # the slow gate against an installation of this source
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
with_published <- "--published" %in% args
source(file.path("inst", "scripts", "published-check-contract.R"))

gates <- list()
add <- function(id, name, status, detail) {
  gates[[length(gates) + 1L]] <<- data.frame(
    gate = id, name = name, status = status, detail = detail,
    stringsAsFactors = FALSE
  )
}

run_script <- function(path, extra = character()) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(path), extra),
    stdout = TRUE, stderr = TRUE
  ))
  list(status = attr(out, "status") %||% 0L, out = out)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

run_tests <- function(filter, label) {
  res <- tryCatch(
    as.data.frame(testthat::test_local(filter = filter, reporter = "silent",
                                       stop_on_failure = FALSE)),
    error = function(e) NULL
  )
  if (is.null(res) || !nrow(res)) {
    return(list(status = "SKIP", detail = paste0("no tests matched '", filter, "'")))
  }
  f <- sum(res$failed); e <- sum(res$error); s <- sum(res$skipped)
  list(
    status = if (f + e > 0) "FAIL" else if (s == nrow(res)) "SKIP" else "PASS",
    detail = sprintf("%d tests, %d expectations, %d failed, %d skipped",
                     nrow(res), sum(res$passed), f + e, s)
  )
}

cat("\n=== bayesmokken gate run ", format(Sys.time(), "%Y-%m-%d %H:%M"),
    " ===\n\n", sep = "")

## G-1 / G-2 -----------------------------------------------------------------
r <- run_tests("zzz-parity-v0", "parity")
add("G-1", "numerical parity vs v0", r$status, r$detail)
r <- run_tests("zzz-api-contract", "api")
add("G-2", "API contract (additive-only)", r$status, r$detail)

## G-3 ------------------------------------------------------------------------
r <- run_script("inst/scripts/scan-forbidden-language.R")
add("G-3", "forbidden interpretation language",
    if (r$status == 0) "PASS" else "FAIL",
    tail(grep("hits", r$out, value = TRUE), 1))

## G-4 ------------------------------------------------------------------------
r <- run_tests("zzz-frozen-calibration", "calibration")
add("G-4", "frozen calibration", r$status, r$detail)

## G-5 ------------------------------------------------------------------------
r <- run_tests("zzz-uncalibrated-labels", "labels")
add("G-5", "uncalibrated item/pair labels", r$status, r$detail)

## G-6 ------------------------------------------------------------------------
r <- run_tests("rng-determinism|rng-caller-state|public-api-deterministic", "rng")
add("G-6", "reproducibility (seed / RNG)", r$status, r$detail)

## G-7 ------------------------------------------------------------------------
r <- run_script("inst/scripts/check-identity.R")
add("G-7", "identity correctness",
    if (r$status == 0) "PASS" else "FAIL",
    if (r$status == 0) "all fields correct" else "see check-identity output")

## G-8 ------------------------------------------------------------------------
sug <- strsplit(read.dcf("DESCRIPTION")[1, "Suggests"], ",\\s*")[[1]]
sug <- trimws(sub("\\s*\\(.*", "", sug))
imp <- strsplit(read.dcf("DESCRIPTION")[1, "Imports"], ",\\s*")[[1]]
imp <- trimws(sub("\\s*\\(.*", "", imp))
leaked <- character()
for (p in setdiff(sug, c("knitr", "rmarkdown", "testthat", "spelling"))) {
  hit <- unlist(lapply(
    list.files("R", pattern = "\\.R$", full.names = TRUE),
    function(f) {
      ln <- readLines(f, warn = FALSE)
      # Only executable lines count. A Suggests package named in a roxygen
      # block or an ordinary comment is documentation, not a dependency, and
      # flagging it would push authors to stop naming packages in prose.
      code <- ln[!grepl("^\\s*#", ln)]
      i <- grep(paste0("\\b", p, "::"), code)
      guarded <- any(grepl(paste0("requireNamespace\\(\\s*[\"']", p),
                           paste(code, collapse = "\n")))
      if (length(i) && !guarded) f else NULL
    }
  ))
  if (length(hit)) leaked <- c(leaked, paste0(p, " in ", basename(hit)))
}
add("G-8", "dependency discipline",
    if (!length(leaked)) "PASS" else "FAIL",
    if (!length(leaked)) sprintf("Imports %d, Suggests %d, unguarded 0",
                                 length(imp), length(sug))
    else paste(leaked, collapse = "; "))

## G-9 ------------------------------------------------------------------------
r <- run_script("inst/scripts/check-accessibility.R")
add("G-9", "accessibility (fig.alt / seeds)",
    if (r$status == 0) "PASS" else "FAIL",
    tail(grep("STATUS|missing", r$out, value = TRUE), 1))

## G-10 -----------------------------------------------------------------------
r <- run_script("inst/scripts/check-precomputed-freshness.R")
add("G-10", "precomputed cache freshness",
    if (r$status == 0) "PASS" else "FAIL",
    tail(grep("STATUS|full deep", r$out, value = TRUE), 1))

## Published numbers (opt-in; reads external sources) -------------------------
if (with_published) {
  pub_lib <- bm_pub_option(args, "--lib")
  pub_receipt <- bm_pub_option(
    args, "--receipt", tempfile("bayesmokken-published-", fileext = ".rds")
  )
  pub_args <- c("--receipt", shQuote(pub_receipt))
  if (!is.null(pub_lib)) pub_args <- c(pub_args, "--lib", shQuote(pub_lib))
  # A failed child must not be certified with an earlier successful receipt.
  if (file.exists(pub_receipt)) unlink(pub_receipt)
  r <- run_script("inst/scripts/verify-published-numbers.R", pub_args)
  parsed <- tryCatch(
    bm_pub_read_receipt(pub_receipt, bm_pub_contract("."), r$status),
    error = function(e) list(
      status = "FAIL",
      detail = paste(conditionMessage(e), paste(tail(r$out, 3L), collapse = " "))
    )
  )
  add("PUB", "published numbers (both papers)", parsed$status, parsed$detail)
}

out <- do.call(rbind, gates)
w <- max(nchar(out$name))
for (i in seq_len(nrow(out))) {
  cat(sprintf("%-5s %-*s %-6s %s\n", out$gate[i], w, out$name[i],
              out$status[i], out$detail[i]))
}
n_fail <- sum(out$status == "FAIL")
cat("\n", nrow(out), " gates: ", sum(out$status == "PASS"), " PASS, ",
    n_fail, " FAIL, ", sum(out$status == "SKIP"), " SKIP\n", sep = "")
if (!with_published) {
  cat("(PUB was NOT RUN; release receipts require --published)\n")
}
release_incomplete <- with_published && any(out$status != "PASS")
invisible(if (n_fail || release_incomplete) quit(status = 1))
