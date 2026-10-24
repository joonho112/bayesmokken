# Stable scientific checks and run receipts. Sourcing performs no checks.
# The scientific target table is read only; run receipts are separate files.
bm_pub_option <- function(args, name, default = NULL) {
  at <- which(args == name)
  if (!length(at)) return(default)
  if (length(at) != 1L || at == length(args) ||
      startsWith(args[at + 1L], "--")) {
    stop("Supply exactly one value after ", name, ".", call. = FALSE)
  }
  args[[at + 1L]]
}

bm_pub_contract <- function(root) {
  spec <- utils::read.csv(file.path(root, "inst", "extdata", "spec",
                                    "published-checks.csv"),
                          stringsAsFactors = FALSE, check.names = FALSE)
  targets <- utils::read.csv(file.path(root, "inst", "extdata",
                                       "published-targets.csv"),
                             stringsAsFactors = FALSE, check.names = FALSE)
  required <- sprintf("PUB-%03d", seq_len(43L))
  key <- function(x) paste(x$section, x$check, sep = "\r")
  if (!identical(names(spec),
                 c("check_id", "section", "check", "required_inputs")) ||
      !identical(spec$check_id, required) || anyDuplicated(key(spec)) ||
      !all(c("section", "check", "expected", "tolerance") %in% names(targets)) ||
      nrow(targets) != length(required) || anyDuplicated(key(targets)) ||
      !setequal(key(targets), key(spec))) {
    stop("Published check registry has missing, duplicate or unexpected IDs/keys.",
         call. = FALSE)
  }
  targets <- targets[match(key(spec), key(targets)), , drop = FALSE]
  e <- new.env(parent = emptyenv())
  e$source_version <- unname(read.dcf(file.path(root, "DESCRIPTION"),
                                     fields = "Version")[1, 1])
  e$spec <- spec
  e$seen <- rep(FALSE, length(required))
  e$checks <- data.frame(
    check_id = spec$check_id, section = spec$section, check = spec$check,
    expected = targets$expected, observed = "",
    max_abs_diff = NA_real_, tolerance = targets$tolerance,
    status = "SKIP", reason = "Check was not reached.",
    stringsAsFactors = FALSE
  )
  e
}

bm_pub_record <- function(state, section, check, expected, observed, tol,
                          status = NULL) {
  i <- which(state$checks$section == section & state$checks$check == check)
  if (length(i) != 1L) stop("Unexpected published check: ", section, " / ", check)
  if (state$seen[i]) stop("Duplicate published check: ", state$checks$check_id[i])
  formatted <- paste(format(expected, digits = 15), collapse = " , ")
  if (!identical(formatted, state$checks$expected[i]) ||
      !isTRUE(all.equal(as.numeric(tol), state$checks$tolerance[i],
                        tolerance = 0))) {
    stop("Scientific target or tolerance changed for ", state$checks$check_id[i])
  }
  diff <- if (is.numeric(expected) && is.numeric(observed)) {
    max(abs(expected - observed))
  } else NA_real_
  if (is.null(status)) {
    status <- if (is.finite(diff) && diff <= tol) "PASS" else "FAIL"
  }
  if (length(status) != 1L || !status %in% c("PASS", "FAIL")) {
    stop("A computed published check must be PASS or FAIL.")
  }
  state$seen[i] <- TRUE
  state$checks$observed[i] <- paste(format(observed, digits = 15), collapse = " , ")
  state$checks$max_abs_diff[i] <- diff
  state$checks$status[i] <- status
  state$checks$reason[i] <- if (status == "PASS") "" else "Observed value failed its fixed target."
  cat(sprintf("  [%s] %s %-46s max|diff| = %s\n",
              status, state$checks$check_id[i], check, format(diff, digits = 3)))
  invisible(NULL)
}

bm_pub_skip <- function(state, input, reason, prefix = NULL) {
  requires <- strsplit(state$spec$required_inputs, ";", fixed = TRUE)
  selected <- vapply(requires, function(x) input %in% x, logical(1))
  if (!is.null(prefix)) selected <- selected & startsWith(state$checks$check, prefix)
  selected <- selected & !state$seen
  state$checks$reason[selected] <- reason
  for (i in which(selected)) {
    cat(sprintf("  [SKIP] %s %s: %s\n", state$checks$check_id[i],
                state$checks$check[i], reason))
  }
  invisible(NULL)
}

bm_pub_validate_checks <- function(checks, contract) {
  expected <- contract$checks
  required <- names(expected)
  if (!is.data.frame(checks) || !identical(names(checks), required) ||
      nrow(checks) != nrow(expected) || anyNA(checks$check_id) ||
      anyDuplicated(checks$check_id) ||
      !setequal(checks$check_id, expected$check_id)) {
    stop("Published receipt has missing, duplicate or unexpected check IDs/schema.")
  }
  checks <- checks[match(expected$check_id, checks$check_id), , drop = FALSE]
  for (field in c("section", "check", "expected", "tolerance")) {
    if (!identical(checks[[field]], expected[[field]])) {
      stop("Published receipt changed fixed check field: ", field)
    }
  }
  if (anyNA(checks$status) || !all(checks$status %in% c("PASS", "FAIL", "SKIP")) ||
      any(checks$status == "SKIP" & (!nzchar(checks$reason) | is.na(checks$reason)))) {
    stop("Published receipt has invalid status or an unexplained skip.")
  }
  invisible(TRUE)
}

bm_pub_finish <- function(state, path, source_version, loaded_version,
                          loaded_from, runtime_error = NULL) {
  bm_pub_validate_checks(state$checks, state)
  status <- if (!is.null(runtime_error) || any(state$checks$status == "FAIL")) {
    "FAIL"
  } else if (any(state$checks$status == "SKIP")) "INCOMPLETE" else "PASS"
  receipt <- list(
    schema_version = "bayesmokken-published-checks-v1", status = status,
    source_version = source_version, loaded_version = loaded_version,
    loaded_from = loaded_from, runtime_error = runtime_error,
    checks = state$checks
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".published-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(receipt, temporary)
  if (!file.rename(temporary, path)) stop("Could not save published receipt: ", path)
  receipt
}

bm_pub_read_receipt <- function(path, contract, exit_status) {
  receipt <- readRDS(path)
  if (!identical(receipt$schema_version, "bayesmokken-published-checks-v1") ||
      !identical(receipt$source_version, contract$source_version) ||
      !identical(receipt$source_version, receipt$loaded_version)) {
    stop("Published receipt identity does not match.")
  }
  bm_pub_validate_checks(receipt$checks, contract)
  counts <- table(factor(receipt$checks$status, levels = c("PASS", "FAIL", "SKIP")))
  derived <- if (!is.null(receipt$runtime_error) || counts[["FAIL"]] > 0L) {
    "FAIL"
  } else if (counts[["SKIP"]] > 0L) "INCOMPLETE" else "PASS"
  expected_exit <- switch(derived, PASS = 0L, INCOMPLETE = 2L, 1L)
  if (!identical(receipt$status, derived) || exit_status != expected_exit) {
    stop("Published receipt and process status disagree.")
  }
  list(
    status = if (derived == "INCOMPLETE") "SKIP" else derived,
    detail = sprintf("%d required checks: %d PASS, %d FAIL, %d SKIP%s",
                     nrow(contract$checks), counts[["PASS"]], counts[["FAIL"]],
                     counts[["SKIP"]],
                     if (is.null(receipt$runtime_error)) "" else
                       paste0("; ", receipt$runtime_error))
  )
}
