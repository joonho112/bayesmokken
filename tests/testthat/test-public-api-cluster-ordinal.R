cluster_ordinal_fixture <- function() {
  list(
    data = data.frame(
      a = c(0, 1, 2, 3, 0, 1, 3, 2, 1, 0, 2, 3),
      b = c(0, 1, 1, 3, 0, 2, 3, 2, 1, 0, 2, 3),
      c = c(0, 0, 2, 3, 1, 2, 3, 1, 1, 0, 2, 3)
    ),
    cluster = rep(paste0("g", 1:4), c(2, 3, 4, 3)),
    supports = list(a = 0:3, b = 0:3, c = 0:3)
  )
}

supported_cluster_ordinal_fixture <- function(G = 22L) {
  base <- cluster_ordinal_fixture()
  rows <- rep(seq_len(nrow(base$data)), times = G)
  data <- base$data[rows, , drop = FALSE]
  rownames(data) <- NULL
  list(
    data = data,
    cluster = rep(sprintf("g%03d", seq_len(G)), each = nrow(base$data)),
    supports = base$supports
  )
}

test_that("deterministic ordinal API exposes both frozen cluster targets", {
  fixture <- cluster_ordinal_fixture()
  respondent <- mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "respondent",
    warn = FALSE
  )
  equal_cluster <- mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "equal_cluster",
    warn = FALSE
  )
  expect_s3_class(respondent, "bm_empirical_h")
  expect_identical(
    respondent$estimand$target_id,
    "V4-TARGET-RESPONDENT-WEIGHTED-v1"
  )
  expect_identical(
    equal_cluster$estimand$target_id,
    "V4-TARGET-EQUAL-CLUSTER-v1"
  )
  expect_false(identical(respondent$scale$H, equal_cluster$scale$H))
  expect_identical(
    respondent$provenance$cluster_id_hash,
    equal_cluster$provenance$cluster_id_hash
  )
})

test_that("matched row and cluster permutation leaves point H invariant", {
  fixture <- cluster_ordinal_fixture()
  order <- c(12, 11, 5, 4, 10, 9, 3, 2, 8, 7, 6, 1)
  before <- mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "equal_cluster",
    warn = FALSE
  )
  after <- mokken_h(
    fixture$data[order, ],
    supports = fixture$supports,
    cluster = fixture$cluster[order],
    cluster_target = "equal_cluster",
    warn = FALSE
  )
  expect_equal(before$scale$H, after$scale$H, tolerance = 2e-15)
  expect_equal(before$item$H_i, after$item$H_i, tolerance = 2e-15)
})

test_that("cluster vector is aligned before complete-scale filtering", {
  fixture <- cluster_ordinal_fixture()
  fixture$data$a[c(2, 8)] <- NA
  fit <- mokken_h(
    fixture$data,
    item_type = "ordinal",
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "equal_cluster",
    warn = FALSE
  )
  expected <- table(fixture$cluster[-c(2, 8)])
  expect_identical(
    unname(fit$provenance$cluster_sizes),
    as.integer(expected[unique(fixture$cluster)])
  )
  expect_identical(fit$provenance$excluded_index, c(2L, 8L))
})

test_that("clustered ordinal BB locks law, gamma, method, and draw identity", {
  fixture <- supported_cluster_ordinal_fixture()
  fit <- bb_mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "respondent",
    draws = 120,
    seed = 18023,
    batch_size = 17,
    workers = 9,
    warn = FALSE
  )
  expect_s3_class(fit, "bm_bb_h")
  expect_identical(
    fit$sampling$law_id,
    "V4-PRI-TWO-STAGE-HBB-RESP-v1"
  )
  expect_identical(
    fit$interval$method_id,
    "V4-P15-HBB-RESP-GN150-v1"
  )
  expect_identical(fit$interval$gamma, -1.5)
  expect_identical(fit$interval$n_complete, 264L)
  expect_identical(fit$interval$cluster_count, 22L)
  expect_identical(fit$interval$calibration_n, 22L)
  expect_identical(fit$interval$calibration_unit, "clusters")
  expect_identical(fit$interval$validation_status,
                   "CONFIRMED_CLUSTER_COUNT")
  expect_identical(fit$interval$validation_scope,
                   "POLYTOMOUS_CLUSTERED_HIERARCHICAL_BB")
  expect_identical(fit$interval$validated_cluster_min, 22L)
  expect_identical(fit$interval$validated_cluster_max, 80L)
  expect_identical(fit$computation$workers_requested, 9L)
  expect_true(nzchar(fit$rng$draw_identity_hash))
  expect_identical(fit$draws$draw_id, seq_len(120))
  # v1 note: the console layout was redesigned in Phase 5 (log 006).
  # The STRUCTURE of the printed information is unchanged and the
  # method identifier is still shown; only the wording and layout moved.
  # The original assertion is kept beside the new one so the change is
  # visible in the diff rather than silently dropped.
  # was: expect_output(print(fit), "rows/clusters/draws")
  expect_output(print(fit), "clusters =")
  expect_output(print(fit), "draws =")
  expect_output(print(fit), "V4-PRI-TWO-STAGE-HBB-RESP-v1")
  expect_s3_class(summary(fit), "summary_bm_bb_h")
})

test_that("cluster BB is invariant to batch size and requested workers", {
  fixture <- supported_cluster_ordinal_fixture()
  arguments <- list(
    data = fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "equal_cluster",
    draws = 125,
    seed = 9021,
    warn = FALSE
  )
  a <- do.call(bb_mokken_h, c(arguments, list(batch_size = 11, workers = 1)))
  b <- do.call(bb_mokken_h, c(arguments, list(batch_size = 80, workers = 32)))
  # The batch partition changes the order in which the weighted sums
  # accumulate, and floating-point addition is not associative. The draws are
  # bit-identical on some platforms and ~1e-15 apart on others; what the
  # contract guarantees is that the same weights are drawn, which is what the
  # draw-identity hash and this tolerance together pin down.
  expect_equal(a$draws, b$draws, tolerance = 1e-12)
  expect_equal(a$interval, b$interval, tolerance = 1e-12)
  expect_identical(a$rng$draw_identity_hash, b$rng$draw_identity_hash)
})

test_that("equal-cluster BB exposes its distinct frozen IDs", {
  fixture <- cluster_ordinal_fixture()
  fit <- bb_mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "equal_cluster",
    draws = 80,
    seed = 43,
    interval = "equal_tail",
    warn = FALSE
  )
  expect_identical(
    fit$sampling$law_id,
    "V4-SENS-TWO-STAGE-HBB-CLUSTER-v1"
  )
  expect_identical(
    fit$interval$base_method_id,
    "V4-SENS-TWO-STAGE-HBB-CLUSTER-v1"
  )
  expect_match(fit$interval$method_id, "EQUAL-TAIL-CLUSTER")
  expect_identical(fit$interval$gamma, 0)
})

test_that("public objects survive base R serialization exactly", {
  fixture <- supported_cluster_ordinal_fixture()
  empirical <- mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "respondent",
    warn = FALSE
  )
  random_weight <- bb_mokken_h(
    fixture$data,
    supports = fixture$supports,
    cluster = fixture$cluster,
    cluster_target = "respondent",
    draws = 60,
    seed = 91,
    warn = FALSE
  )
  empirical_file <- tempfile(fileext = ".rds")
  random_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(empirical_file, random_file)), add = TRUE)
  saveRDS(empirical, empirical_file)
  saveRDS(random_weight, random_file)
  expect_identical(readRDS(empirical_file), empirical)
  expect_identical(readRDS(random_file), random_weight)
})

test_that("calibrated cluster intervals fail closed outside their scope", {
  too_few <- cluster_ordinal_fixture()
  expect_error(
    bb_mokken_h(
      too_few$data,
      supports = too_few$supports,
      cluster = too_few$cluster,
      cluster_target = "respondent",
      draws = 20,
      warn = FALSE
    ),
    class = "bm_cluster_count_below_validation_range_error"
  )

  two_category <- supported_cluster_ordinal_fixture()
  two_category$data[] <- lapply(
    two_category$data,
    function(x) as.numeric(x > 1)
  )
  two_category$supports <- stats::setNames(
    rep(list(0:1), ncol(two_category$data)),
    names(two_category$data)
  )
  expect_error(
    bb_mokken_h(
      two_category$data,
      supports = two_category$supports,
      cluster = two_category$cluster,
      cluster_target = "respondent",
      draws = 20,
      warn = FALSE
    ),
    class = "bm_cluster_calibration_scope_error"
  )

  # The public range applies after complete-scale filtering, not to the raw
  # number of labels supplied by the caller.
  effective <- supported_cluster_ordinal_fixture()
  effective$data$a[effective$cluster == "g022"] <- NA_real_
  expect_error(
    bb_mokken_h(
      effective$data,
      supports = effective$supports,
      cluster = effective$cluster,
      cluster_target = "respondent",
      draws = 20,
      warn = FALSE
    ),
    class = "bm_cluster_count_below_validation_range_error"
  )

  # Dichotomous items under a cluster argument are refused on every route,
  # including the uncorrected comparator.
  #
  # This test previously asserted that the comparator still returned, on the
  # ground that it claims no calibration. That reasoning is sound as far as it
  # goes, but it leaves the package's stated guard rail false: both articles
  # say dichotomous data with a `cluster` argument produces a validation error,
  # without qualifying it by interval type. The route that made the sentence
  # false was not this one -- it was `item_type = "auto"`, which resolves to
  # "ordinal" as soon as a cluster is supplied and so never reaches the binary
  # rules at all. Guarding the resolved two-point case closes that route, and
  # it necessarily closes this one too. A user with dichotomous items keeps the
  # whole binary path; what they may not do is carry those items into the
  # clustered machinery, which was developed and confirmed on polytomous data.
  comparator <- cluster_ordinal_fixture()
  comparator$data[] <- lapply(
    comparator$data,
    function(x) as.numeric(x > 1)
  )
  comparator$supports <- stats::setNames(
    rep(list(0:1), ncol(comparator$data)),
    names(comparator$data)
  )
  expect_error(
    bb_mokken_h(
      comparator$data,
      supports = comparator$supports,
      cluster = comparator$cluster,
      cluster_target = "respondent",
      draws = 20,
      interval = "equal_tail",
      warn = FALSE
    ),
    class = "bm_path_crossing_error"
  )
})

test_that("cluster counts above 80 are marked and warn under user control", {
  upper_boundary <- supported_cluster_ordinal_fixture(80L)
  expect_no_warning(
    confirmed <- bb_mokken_h(
      upper_boundary$data,
      supports = upper_boundary$supports,
      cluster = upper_boundary$cluster,
      cluster_target = "respondent",
      draws = 20,
      seed = 80,
      warn = TRUE
    )
  )
  expect_identical(confirmed$interval$validation_status,
                   "CONFIRMED_CLUSTER_COUNT")
  expect_identical(confirmed$interval$cluster_count, 80L)

  fixture <- supported_cluster_ordinal_fixture(81L)
  expect_warning(
    fit <- bb_mokken_h(
      fixture$data,
      supports = fixture$supports,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      draws = 20,
      seed = 81,
      warn = TRUE
    ),
    class = "bm_cluster_count_extrapolation_warning"
  )
  expect_identical(fit$interval$validation_status,
                   "EXTRAPOLATED_CLUSTER_COUNT")
  expect_identical(fit$interval$cluster_count, 81L)
  expect_identical(fit$interval$calibration_n, 81L)

  expect_no_warning(
    silent <- bb_mokken_h(
      fixture$data,
      supports = fixture$supports,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      draws = 20,
      seed = 81,
      warn = FALSE
    )
  )
  expect_identical(silent$interval$validation_status,
                   "EXTRAPOLATED_CLUSTER_COUNT")
})

test_that("classroom interval keeps respondents and clusters distinct", {
  items <- paste0("wb", 1:6)
  fit <- bb_mokken_h(
    classroom_wb[, items],
    item_type = "ordinal",
    supports = bm_supports(classroom_wb, items = items),
    cluster = classroom_wb$class_id,
    cluster_target = "respondent",
    weight_law = "hierarchical_cluster",
    draws = 20,
    seed = 30,
    warn = FALSE
  )
  expect_identical(fit$interval$n_complete, 614L)
  expect_identical(fit$interval$cluster_count, 30L)
  expect_identical(fit$interval$calibration_n, 30L)
  expect_identical(fit$interval$calibration_unit, "clusters")
})

test_that("unsupported combinations fail closed", {
  fixture <- cluster_ordinal_fixture()
  binary <- as.data.frame(lapply(fixture$data, function(x) as.numeric(x > 1)))
  expect_error(
    mokken_h(
      binary,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      item_type = "binary"
    ),
    "Binary scalability with `cluster`"
  )
  expect_error(
    mokken_h(binary, weight_law = "hierarchical_cluster"),
    "Binary scalability"
  )
  expect_error(
    mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster_target = "equal_cluster"
    ),
    "requires `cluster`"
  )
  expect_error(
    mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      weights = rep(1, nrow(fixture$data))
    ),
    "cannot be supplied together"
  )
  expect_error(
    bb_mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      draws = 30
    ),
    "requires `cluster`"
  )
  expect_error(
    bb_mokken_h(
      fixture$data,
      item_type = "ordinal",
      supports = fixture$supports,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      weight_law = "iid_respondent",
      draws = 30
    ),
    "iid-respondent"
  )
  expect_error(
    bb_mokken_h(
      binary,
      cluster = fixture$cluster,
      cluster_target = "respondent",
      item_type = "binary",
      draws = 30
    ),
    "Binary scalability with `cluster`"
  )
})
