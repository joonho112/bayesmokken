# =============================================================================
# The clustered article's application, reproduced by a package call
#
# mokken::SWMDK as loaded (639 pupils, 30 classrooms, scores 1-5), the teacher
# scale Items 1-6 and the classmate scale Items 8, 9, 10, 11, 13, both targets,
# B = 9,999 draws, and the article's archive seeds for its main stream (S01):
# 1883301060 for the teacher scale and 1863497163 for the classmate scale.
# The values are the article's key-number ledger (v4.app-*). The point
# estimates agree to floating-point summation order; the package's draws are
# the archive's draws evaluated by a kernel whose BLAS summation order differs,
# so the endpoints agree to about 3e-13, not bit for bit. The 1e-9 tolerance
# is a thousand times the observed difference and a million times below the
# archive's own 20-stream endpoint standard deviation (<= 5.7e-4).
# =============================================================================

swmdk_published <- list(
  teacher = list(
    seed = 1883301060L,
    items = paste0("Item", 1:6),
    respondent = c(point = 0.619919910686637,
                   lower = 0.576348622837636, upper = 0.662965039512330),
    equal_cluster = c(point = 0.622319152142876,
                      lower = 0.579849088771199, upper = 0.665975663041322)
  ),
  classmate = list(
    seed = 1863497163L,
    items = paste0("Item", c(8, 9, 10, 11, 13)),
    respondent = c(point = 0.592308560081349,
                   lower = 0.556233936039344, upper = 0.631182130540829),
    equal_cluster = c(point = 0.582943580481479,
                      lower = 0.542865250689451, upper = 0.621934026654128)
  )
)

test_that("the default clustered lane reproduces the article's SWMDK intervals", {
  skip_if_not_installed("mokken")
  skip_on_cran()
  env <- new.env()
  utils::data("SWMDK", package = "mokken", envir = env)
  swmdk <- get("SWMDK", envir = env)
  expect_identical(nrow(swmdk), 639L)
  cluster <- as.character(swmdk$classId)
  expect_identical(length(unique(cluster)), 30L)
  for (scale in names(swmdk_published)) {
    spec <- swmdk_published[[scale]]
    items <- spec$items
    supports <- stats::setNames(rep(list(1:5), length(items)), items)
    for (target in c("respondent", "equal_cluster")) {
      fit <- bb_mokken_h(
        swmdk[, items], item_type = "ordinal", supports = supports,
        cluster = cluster, cluster_target = target,
        draws = 9999L, seed = spec$seed, warn = FALSE
      )
      published <- spec[[target]]
      itv <- fit$interval
      expect_identical(itv$status, "OK")
      expect_identical(itv$defined_draws, 9999L)
      expect_identical(itv$cluster_count, 30L)
      expect_identical(itv$n_complete, 639L)
      expect_equal(itv$point_estimate, published[["point"]], tolerance = 1e-12)
      expect_equal(itv$lower, published[["lower"]], tolerance = 1e-9)
      expect_equal(itv$upper, published[["upper"]], tolerance = 1e-9)
      expect_equal(itv$width_multiplier, 1.07888669019652, tolerance = 1e-14)
      expect_identical(itv$validation_status, "CONFIRMED_CLUSTER_COUNT")
      expect_false(itv$upper_intersection)
      expect_identical(fit$rng$rng_mapping, "V4R-DRAW-MAJOR-EXP-GPLUSN-CMRG-v1")
      # The same draws under the percentile comparator give the article's
      # percentile interval construction on the identical stream.
      comparator <- bb_mokken_h(
        swmdk[, items], item_type = "ordinal", supports = supports,
        cluster = cluster, cluster_target = target, interval = "equal_tail",
        draws = 9999L, seed = spec$seed, warn = FALSE
      )
      expect_identical(comparator$draws$scale, fit$draws$scale)
      expect_identical(comparator$interval$lower, fit$interval$raw_lower)
      expect_identical(comparator$interval$upper, fit$interval$raw_upper)
    }
  }
})

test_that("the archive's stored main draws reproduce its endpoints through the package arithmetic", {
  # Optional: runs only where the clustered article's archive is available
  # next to the package. It is the same check the published-number gate
  # performs; here it documents the exact arithmetic path.
  archive <- Sys.getenv(
    "BAYESMOKKEN_PAPER_B_ARCHIVE",
    normalizePath(file.path(
      "..", "..", "..", "paper-B-clustered-polytomous-SWMDK", "manuscript",
      "manuscript-clustered-v4-arXiv", "analysis", "application",
      "phase08-swmdk-20261007"
    ), mustWork = FALSE)
  )
  skip_if_not(dir.exists(archive), "clustered article archive not available")
  skip_if_not_installed("mokken")
  for (scale in names(swmdk_published)) {
    stored <- readRDS(file.path(archive, "execution",
                                sprintf("APP8-%s-S01.rds", scale)))
    for (target in c("respondent", "equal_cluster")) {
      archive_target <- if (identical(target, "respondent")) {
        "respondent"
      } else {
        "cluster"
      }
      published <- swmdk_published[[scale]][[target]]
      itv <- bayesmokken:::.bm_crbb_interval_from_draws(
        bb_draws = stored$draws[[archive_target]],
        point_estimate = published[["point"]], n_complete = 639L,
        cluster_count = 30L, cluster_target = target
      )
      expect_equal(itv$lower, published[["lower"]], tolerance = 1e-12)
      expect_equal(itv$upper, published[["upper"]], tolerance = 1e-12)
    }
  }
})
