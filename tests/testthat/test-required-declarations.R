required_declarations_fixture <- function() {
  list(
    data = data.frame(
      a = c(0, 1, 2, 3, 0, 1, 3, 2),
      b = c(0, 1, 1, 3, 0, 2, 3, 2),
      c = c(0, 0, 2, 3, 1, 2, 3, 1)
    ),
    supports = list(a = 0:3, b = 0:3, c = 0:3),
    cluster = rep(c("g1", "g2", "g3", "g4"), each = 2L)
  )
}

test_that("clustered public calls require an explicit population target", {
  fixture <- required_declarations_fixture()

  expect_error(
    mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster = fixture$cluster
    ),
    "must be declared whenever `cluster` is supplied",
    class = "bm_input_error"
  )
  expect_error(
    bb_mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster = fixture$cluster,
      draws = 2L
    ),
    "must be declared whenever `cluster` is supplied",
    class = "bm_input_error"
  )
  expect_error(
    bayes_mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster = fixture$cluster,
      draws = 2L
    ),
    "must be declared whenever `cluster` is supplied",
    class = "bm_input_error"
  )
})

test_that("ordinal public calls never infer observed supports", {
  fixture <- required_declarations_fixture()

  expect_error(
    mokken_h(fixture$data, item_type = "ordinal"),
    "requires explicit `supports`",
    class = "bm_input_error"
  )
  expect_error(
    mokken_h(
      fixture$data,
      cluster = fixture$cluster,
      cluster_target = "respondent"
    ),
    "requires explicit `supports`",
    class = "bm_input_error"
  )
  expect_error(
    bb_mokken_h(
      fixture$data,
      item_type = "ordinal",
      cluster = fixture$cluster,
      cluster_target = "respondent",
      draws = 2L
    ),
    "requires explicit `supports`",
    class = "bm_input_error"
  )
  expect_error(
    suppressWarnings(bayes_mokken_h(
      fixture$data,
      item_type = "ordinal",
      cluster = fixture$cluster,
      cluster_target = "respondent",
      draws = 2L
    )),
    "requires explicit `supports`",
    class = "bm_input_error"
  )
  expect_error(
    bayesmokken:::bm_validate_ordinal_data(fixture$data),
    "must declare one score support for every selected item",
    class = "bm_input_error"
  )
})

test_that("ordinal supports cover every selected item", {
  fixture <- required_declarations_fixture()

  expect_error(
    mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports[c("a", "b")]
    ),
    "every selected item",
    class = "bm_input_error"
  )
  expect_no_error(mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    warn = FALSE
  ))
})

test_that("declaration guardrails expose the same recovery hints", {
  guardrails <- bm_guardrails()

  target_row <- grepl("without `cluster_target`", guardrails$situation)
  support_row <- grepl("without `supports`", guardrails$situation)
  expect_identical(sum(target_row), 1L)
  expect_identical(sum(support_row), 1L)
  expect_match(guardrails$what_to_do[target_row], "will not choose for you")
  expect_match(guardrails$what_to_do[support_row], "bm_supports")
})
