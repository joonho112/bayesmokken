.preflight_ordinal <- data.frame(
  a = rep(0:2, 8L),
  b = rep(c(0, 1, 2, 1), 6L),
  c = rep(c(0, 2, 1), 8L)
)

.preflight_clustered <- local({
  cluster_number <- rep(seq_len(22L), each = 3L)
  row_number <- seq_along(cluster_number)
  data.frame(
    class_id = sprintf("g%02d", cluster_number),
    a = (row_number + cluster_number) %% 3L,
    b = (2L * row_number + cluster_number) %% 4L,
    c = (row_number + 2L * cluster_number) %% 3L
  )
})

test_that("binary preflight call captures data and spells out every item", {
  checked <- bm_check_data(srs)

  expect_true(checked$next_call_runnable)
  expect_identical(checked$next_call_method, "bb_mokken_h")
  expect_identical(checked$call_expressions$data, "srs")
  expect_null(checked$call_expressions$items)
  expect_null(checked$call_expressions$cluster)
  expect_no_error(parse(text = checked$next_call))
  expect_false(grepl("\\.\\.\\.", checked$next_call))
  expect_false(grepl("bb_mokken_h\\(data[,)]", checked$next_call))
  for (item in names(srs)) {
    expect_match(checked$next_call, item, fixed = TRUE)
  }

  execution <- new.env(parent = environment())
  execution$srs <- srs
  eval(parse(text = checked$next_call), envir = execution)
  expect_s3_class(execution$fit, "bm_bb_h")
})

test_that("unclustered ordinal preflight produces a runnable point call", {
  selected_items <- names(.preflight_ordinal)
  checked <- bm_check_data(
    .preflight_ordinal,
    items = selected_items,
    item_type = "ordinal"
  )

  expect_true(checked$next_call_runnable)
  expect_identical(checked$next_call_method, "mokken_h")
  expect_identical(checked$call_expressions$data, ".preflight_ordinal")
  expect_identical(checked$call_expressions$items, "selected_items")
  expect_no_error(parse(text = checked$next_call))
  expect_false(grepl("\\.\\.\\.", checked$next_call))

  execution <- new.env(parent = environment())
  execution$.preflight_ordinal <- .preflight_ordinal
  eval(parse(text = checked$next_call), envir = execution)
  expect_s3_class(execution$fit, "bm_empirical_h")
})

test_that("clustered preflight uses only the supplied target and runs", {
  selected_items <- c("a", "b", "c")
  chosen_target <- "respondent"
  checked <- bm_check_data(
    .preflight_clustered,
    items = selected_items,
    cluster = "class_id",
    cluster_target = chosen_target
  )

  expect_true(checked$next_call_runnable)
  expect_identical(checked$next_call_method, "bb_mokken_h")
  expect_identical(checked$call_expressions$items, "selected_items")
  expect_identical(checked$call_expressions$cluster, "\"class_id\"")
  expect_match(
    checked$next_call,
    ".preflight_clustered)[[\"class_id\"]]",
    fixed = TRUE
  )
  expect_match(
    checked$next_call,
    "cluster_target = \"respondent\"",
    fixed = TRUE
  )
  expect_false(grepl("<choose", checked$next_call, fixed = TRUE))
  expect_false(grepl("\\.\\.\\.", checked$next_call))
  expect_no_error(parse(text = checked$next_call))

  execution <- new.env(parent = environment())
  execution$.preflight_clustered <- .preflight_clustered
  eval(parse(text = checked$next_call), envir = execution)
  expect_s3_class(execution$fit, "bm_bb_h")
  expect_identical(
    execution$fit$empirical$estimand$cluster_target,
    "respondent"
  )
})

test_that("omitted cluster target remains an explicit non-runnable choice", {
  selected_items <- c("a", "b", "c")
  checked <- bm_check_data(
    .preflight_clustered,
    items = selected_items,
    cluster = "class_id"
  )

  expect_false(checked$next_call_runnable)
  expect_match(checked$next_call_reason, "Choose `cluster_target")
  expect_match(
    checked$next_call,
    "cluster_target = \"<choose respondent or equal_cluster>\"",
    fixed = TRUE
  )
  expect_no_error(parse(text = checked$next_call))
  expect_false(grepl(
    "cluster_target = \"respondent\"",
    checked$next_call,
    fixed = TRUE
  ))
})

test_that("few-cluster screens get a runnable interval call with the range stated", {
  small_clustered <- .preflight_clustered[seq_len(9L), , drop = FALSE]
  selected_items <- c("a", "b", "c")
  checked <- bm_check_data(
    small_clustered,
    items = selected_items,
    cluster = "class_id",
    cluster_target = "equal_cluster"
  )

  # The complete-record interval is computable for G >= 2, so the next call is
  # still the interval call; the reason line names the confirmed range the
  # three clusters fall outside of.
  expect_true(checked$next_call_runnable)
  expect_identical(checked$next_call_method, "bb_mokken_h")
  expect_match(checked$next_call_reason, "only 3 complete-scale clusters")
  expect_match(checked$next_call_reason, "22-320")
  expect_match(checked$next_call, "weight_law = \"complete_record\"",
               fixed = TRUE)
  expect_no_error(parse(text = checked$next_call))

  execution <- new.env(parent = environment())
  execution$small_clustered <- small_clustered
  eval(parse(text = checked$next_call), envir = execution)
  expect_s3_class(execution$fit, "bm_bb_h")
  expect_identical(execution$fit$empirical$estimand$cluster_target,
                   "equal_cluster")
  expect_identical(execution$fit$interval$validation_status,
                   "OUTSIDE_CONFIRMED_CLUSTER_COUNT")
})

test_that("invalid preflight targets are typed errors", {
  expect_error(
    bm_check_data(
      .preflight_clustered,
      items = c("a", "b", "c"),
      cluster = "class_id",
      cluster_target = "default"
    ),
    class = "bm_input_error"
  )
  expect_error(
    bm_check_data(srs, cluster_target = "respondent"),
    class = "bm_input_error"
  )
})
