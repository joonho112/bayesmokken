test_that("analysis input hash changes when a retained response changes", {
  x <- srs
  before <- mokken_h(x, warn = FALSE)
  x[1, 1] <- 1 - x[1, 1]
  after <- mokken_h(x, warn = FALSE)

  expect_false(identical(
    before$provenance$analysis_input_hash,
    after$provenance$analysis_input_hash
  ))
  # These component hashes deliberately diagnose schema/selection, not values.
  expect_identical(before$provenance$row_hash, after$provenance$row_hash)
  expect_identical(
    before$provenance$item_set_hash,
    after$provenance$item_set_hash
  )
})

test_that("analysis input hash includes item order and normalized weights", {
  x <- srs[seq_len(20), 1:3]
  base <- mokken_h(x, warn = FALSE)
  reordered <- mokken_h(x[, c(2, 1, 3)], warn = FALSE)
  weighted <- mokken_h(x, weights = seq_len(nrow(x)), warn = FALSE)

  expect_false(identical(
    base$provenance$analysis_input_hash,
    reordered$provenance$analysis_input_hash
  ))
  expect_false(identical(
    base$provenance$analysis_input_hash,
    weighted$provenance$analysis_input_hash
  ))
})

test_that("ordinal analysis hash includes supports, clusters, and target", {
  x <- data.frame(
    a = rep(c(0, 1, 2), 4),
    b = rep(c(0, 2, 1), 4),
    c = rep(c(1, 0, 2), 4)
  )
  supports <- list(a = 0:2, b = 0:2, c = 0:2)
  expanded <- supports
  expanded$c <- 0:3
  cluster <- rep(c("g1", "g2", "g3"), each = 4)
  renamed <- sub("g", "class-", cluster, fixed = TRUE)

  respondent <- mokken_h(
    x, item_type = "ordinal", supports = supports,
    cluster = cluster, cluster_target = "respondent",
    weight_law = "complete_record", warn = FALSE
  )
  equal_cluster <- mokken_h(
    x, item_type = "ordinal", supports = supports,
    cluster = cluster, cluster_target = "equal_cluster",
    weight_law = "complete_record", warn = FALSE
  )
  relabeled <- mokken_h(
    x, item_type = "ordinal", supports = supports,
    cluster = renamed, cluster_target = "respondent",
    weight_law = "complete_record", warn = FALSE
  )
  support_changed <- mokken_h(
    x, item_type = "ordinal", supports = expanded, warn = FALSE
  )
  unclustered <- mokken_h(
    x, item_type = "ordinal", supports = supports, warn = FALSE
  )

  hashes <- vapply(
    list(respondent, equal_cluster, relabeled, support_changed, unclustered),
    function(fit) fit$provenance$analysis_input_hash,
    character(1)
  )
  expect_length(unique(hashes), length(hashes))
  expect_identical(
    bm_provenance(respondent)$analysis_input_hash,
    respondent$provenance$analysis_input_hash
  )
})
