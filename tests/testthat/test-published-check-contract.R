.pub_tools <- new.env(parent = baseenv())
sys.source(.bm_test_inst_file("scripts", "published-check-contract.R"),
           envir = .pub_tools)

test_that("published checks have a fixed identity set and reject duplicate computation", {
  s <- .pub_tools$bm_pub_contract(.bm_test_source_root())
  expect_identical(s$checks$check_id, sprintf("PUB-%03d", seq_len(43L)))
  expect_true(all(s$checks$status == "SKIP"))
  .pub_tools$bm_pub_record(s, "A/SRS", "source file sha256", 0, 0, 0)
  expect_error(.pub_tools$bm_pub_record(s, "A/SRS", "source file sha256", 0, 0, 0),
               "Duplicate")
  expect_error(.pub_tools$bm_pub_record(s, "A/SRS", "unknown", 0, 0, 0),
               "Unexpected")
  expect_error(.pub_tools$bm_pub_record(s, "A/SRS", "complete-scale n", 338, 337, 0),
               "Scientific target or tolerance changed")
})

test_that("receipt validation rejects missing duplicate and forged check identities", {
  s <- .pub_tools$bm_pub_contract(.bm_test_source_root())
  expect_error(.pub_tools$bm_pub_validate_checks(s$checks[-1, ], s), "missing")
  duplicate <- s$checks; duplicate$check_id[2] <- duplicate$check_id[1]
  expect_error(.pub_tools$bm_pub_validate_checks(duplicate, s), "duplicate")
  unexpected <- s$checks; unexpected$check_id[2] <- "PUB-999"
  expect_error(.pub_tools$bm_pub_validate_checks(unexpected, s), "unexpected")
  changed <- s$checks; changed$tolerance[2] <- 1
  expect_error(.pub_tools$bm_pub_validate_checks(changed, s), "fixed check field")
})

test_that("incomplete or numerically failing receipts cannot become a pass", {
  s <- .pub_tools$bm_pub_contract(.bm_test_source_root())
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  r <- .pub_tools$bm_pub_finish(s, path, "0.10.0", "0.10.0", "fixture")
  expect_identical(r$status, "INCOMPLETE")
  expect_identical(.pub_tools$bm_pub_read_receipt(path, s, 2L)$status, "SKIP")
  expect_error(.pub_tools$bm_pub_read_receipt(path, s, 0L), "status disagree")
  wrong_version <- r
  wrong_version$source_version <- wrong_version$loaded_version <- "0.9.0"
  saveRDS(wrong_version, path)
  expect_error(.pub_tools$bm_pub_read_receipt(path, s, 2L), "identity")
  r$status <- "PASS"; saveRDS(r, path)
  expect_error(.pub_tools$bm_pub_read_receipt(path, s, 0L), "status disagree")
  .pub_tools$bm_pub_record(s, "A/SRS", "source file sha256", 0, 1, 0)
  failed <- .pub_tools$bm_pub_finish(s, path, "0.10.0", "0.10.0", "fixture")
  expect_identical(failed$status, "FAIL")
  expect_identical(.pub_tools$bm_pub_read_receipt(path, s, 1L)$status, "FAIL")
})
