# =============================================================================
# G-5  Scale-level-only calibration
#
# INV-5: the width calibration was validated for the SCALE-level coefficient
# only. Paper A: "The selected calibration has been validated only for the
# frozen fixed-binary scale-level H problem." Item- and pair-level draws exist
# in the fitted object and are genuinely useful, but an item-level interval has
# never had its coverage checked. Exposing one without saying so would invite
# readers to treat it as equal in standing to the scale interval.
#
# Rule: item- and pair-level intervals are opt-in; whenever surfaced they must
# (1) use raw equal-tail quantiles with no calibration applied and (2) carry an
# explicit "uncalibrated" label in the output and any figure that shows it.
#
# ACTIVATION: the extraction functions arrive in Phase 4 and the figures in
# Phase 6. Until then these tests skip with an explicit reason rather than
# passing vacuously.
# =============================================================================

has_fn <- function(nm) nm %in% getNamespaceExports("bayesmokken")

skip_until <- function(nm, phase) {
  testthat::skip_if_not(has_fn(nm),
                        paste0("`", nm, "()` arrives in Phase ", phase))
}

bb_fit <- function() {
  x <- data.frame(
    a = c(0, 0, 1, 1, 1, 0, 1, 0, 1, 1), b = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0),
    c = c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1), d = c(1, 0, 1, 1, 1, 0, 1, 0, 1, 1)
  )
  bb_mokken_h(x, draws = 300, seed = 1, warn = FALSE)
}

test_that("item-level output is labelled uncalibrated", {
  skip_until("bm_item_h", 4)
  tab <- bm_item_h(bb_fit(), include_interval = TRUE)
  expect_true("interval_calibration" %in% names(tab))
  expect_true(all(tab$interval_calibration == "uncalibrated"))
})

test_that("pair-level output is labelled uncalibrated when it has intervals", {
  skip_until("bm_pair_h", 4)
  tab <- bm_pair_h(bb_fit(), include_interval = TRUE)
  expect_true("interval_calibration" %in% names(tab))
  expect_true(all(tab$interval_calibration == "uncalibrated"))
})

test_that("item-level endpoints are raw quantiles, not widened", {
  skip_until("bm_item_h", 4)
  fit <- bb_fit()
  tab <- bm_item_h(fit, include_interval = TRUE)
  for (i in seq_len(nrow(tab))) {
    d <- fit$draws$item[, tab$item[i]]
    d <- d[is.finite(d)]
    q <- stats::quantile(d, c(0.025, 0.975), names = FALSE, type = 8)
    expect_equal(tab$bb_lower[i], q[1], tolerance = 1e-12)
    expect_equal(tab$bb_upper[i], q[2], tolerance = 1e-12)
  }
})

test_that("the scale-level table is NOT labelled uncalibrated", {
  skip_until("bm_scale_h", 4)
  tab <- bm_scale_h(bb_fit())
  expect_identical(tab$method_id, "P7-FS-WIDTH-RAW-G025-v1")
  # Absence of the column is the requirement. Writing this as
  # `all(tab$interval_calibration == "uncalibrated")` would be vacuously TRUE
  # when the column is missing, which is exactly the case we want to allow --
  # so assert absence directly.
  expect_false("interval_calibration" %in% names(tab))
})

test_that("tidy() carries the same label as bm_item_h()", {
  skip_until("bm_item_h", 4)
  testthat::skip_if_not(
    "tidy.bm_bb_h" %in% unlist(lapply(
      ls(asNamespace("bayesmokken"), all.names = TRUE), identity)),
    "tidy method arrives in Phase 4")
  tid <- generics::tidy(bb_fit(), include_interval = TRUE)
  expect_true("interval_calibration" %in% names(tid))
})

test_that("item- and pair-level figures declare the label in their caption", {
  skip_until("bm_plot_item_h", 6)
  p <- bm_plot_item_h(bb_fit(), show_interval = TRUE)
  cap <- p$labels$caption
  expect_true(is.character(cap) && nzchar(cap))
  expect_match(cap, "uncalibrated", ignore.case = TRUE)
})
