test_that("frozen random mokken public-API oracle matches deterministic core", {
  fixture <- readRDS(testthat::test_path(
    "fixtures", "oracles", "binary-random-oracle.rds"
  ))
  expect_identical(fixture$schema_version, "mokken-random-binary-oracle-v1")
  expect_identical(fixture$mokken_version, "3.1.2")
  expect_equal(length(fixture$cases), fixture$n_cases)

  for (case in fixture$cases) {
    fit <- mokken_h(as.data.frame(case$data), warn = FALSE)
    pair <- matrix(
      NA_real_,
      ncol(case$data),
      ncol(case$data),
      dimnames = list(colnames(case$data), colnames(case$data))
    )
    for (k in seq_len(nrow(fit$pair))) {
      pair[fit$pair$item_i[k], fit$pair$item_j[k]] <- fit$pair$H_ij[k]
      pair[fit$pair$item_j[k], fit$pair$item_i[k]] <- fit$pair$H_ij[k]
    }
    expect_true(
      max(abs(pair - case$expected_displayed$pair), na.rm = TRUE) <=
        fixture$comparison_tolerance,
      info = case$id
    )
    expect_true(
      max(abs(fit$item$H_i - case$expected_displayed$item)) <=
        fixture$comparison_tolerance,
      info = case$id
    )
    expect_true(
      abs(fit$scale$H - case$expected_displayed$scale) <=
        fixture$comparison_tolerance,
      info = case$id
    )
  }
})
