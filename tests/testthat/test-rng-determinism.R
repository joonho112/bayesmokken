.rng_test_data <- data.frame(
  a = c(0, 0, 1, 1, 1, 0, 1, 0, 1, 0),
  b = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1),
  c = c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1)
)

test_that("same seed has exact draw identity across repeated calls", {
  a <- bb_mokken_h(
    .rng_test_data, draws = 80, seed = 300,
    batch_size = 13, warn = FALSE
  )
  b <- bb_mokken_h(
    .rng_test_data, draws = 80, seed = 300,
    batch_size = 13, warn = FALSE
  )
  expect_identical(a$draws$pair, b$draws$pair)
  expect_identical(a$draws$item, b$draws$item)
  expect_identical(a$draws$scale, b$draws$scale)
  expect_identical(a$rng$stream_map_hash, b$rng$stream_map_hash)
})

test_that("batch size and requested worker count do not alter draw identity", {
  reference <- bb_mokken_h(
    .rng_test_data, draws = 75, seed = 301,
    batch_size = 75, workers = 1, warn = FALSE
  )
  scheduled <- bb_mokken_h(
    .rng_test_data, draws = 75, seed = 301,
    batch_size = 7, workers = 4, warn = FALSE
  )
  expect_identical(reference$draws$pair, scheduled$draws$pair)
  expect_identical(reference$draws$item, scheduled$draws$item)
  expect_identical(reference$draws$scale, scheduled$draws$scale)
  expect_equal(scheduled$computation$workers_requested, 4)
  expect_equal(
    scheduled$computation$execution,
    "deterministic_serial_reference"
  )
})

test_that("draw doubling preserves the exact prefix stream map", {
  short <- bb_mokken_h(
    .rng_test_data, draws = 60, seed = 302,
    batch_size = 11, warn = FALSE
  )
  doubled <- bb_mokken_h(
    .rng_test_data, draws = 120, seed = 302,
    batch_size = 19, warn = FALSE
  )
  expect_identical(short$draws$pair, doubled$draws$pair[1:60, , drop = FALSE])
  expect_identical(short$draws$item, doubled$draws$item[1:60, , drop = FALSE])
  expect_identical(short$draws$scale, doubled$draws$scale[1:60])
})

test_that("independent validation seed changes BB draws", {
  a <- bb_mokken_h(.rng_test_data, draws = 50, seed = 401, warn = FALSE)
  b <- bb_mokken_h(.rng_test_data, draws = 50, seed = 402, warn = FALSE)
  expect_false(identical(a$draws$scale, b$draws$scale))
  expect_false(identical(a$rng$stream_map_hash, b$rng$stream_map_hash))
})

test_that("fractional, NaN, and boundary seeds are policed", {
  expect_error(bm_rng_streams(1.1, 2L), class = "bm_input_error")
  expect_error(bm_rng_streams(1.9, 2L), class = "bm_input_error")
  expect_error(bm_rng_streams(NaN, 2L), class = "bm_input_error")
  expect_error(bm_rng_streams(-1, 2L), class = "bm_input_error")
  expect_error(
    bm_rng_streams(.Machine$integer.max + 1, 2L),
    class = "bm_input_error"
  )
  # Integer-valued doubles and true integers produce identical streams.
  expect_identical(bm_rng_streams(7, 3L), bm_rng_streams(7L, 3L))
  expect_identical(
    bm_rng_streams(.Machine$integer.max, 2L),
    bm_rng_streams(as.integer(.Machine$integer.max), 2L)
  )
})
