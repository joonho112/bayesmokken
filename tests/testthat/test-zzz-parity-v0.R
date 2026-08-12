# =============================================================================
# G-1  Numerical parity against bayesmokken v0 (0.0.0.9001)
#
# INV-1 of the v1 upgrade plan: the estimation engine must be bit-for-bit
# unchanged. Every user-facing surface added in v1 is additive; if any of it
# perturbs a number, this file fails.
#
# The reference was produced by dev/parity/10-build-v0-reference.R, which runs
# the same eight fixtures against a private install of the v0 source in a
# separate R process. Regenerate it only when deliberately re-baselining, and
# say so in the phase log.
# =============================================================================

parity_dir <- system.file("extdata", "parity", package = "bayesmokken")

skip_if_no_reference <- function() {
  testthat::skip_if(
    !nzchar(parity_dir) ||
      !file.exists(file.path(parity_dir, "v0-reference.rds")),
    "v0 parity reference not installed"
  )
}

test_that("v0 parity reference is present and self-describing", {
  skip_if_no_reference()
  ref <- readRDS(file.path(parity_dir, "v0-reference.rds"))
  expect_length(ref, 8L)
  expect_identical(attr(ref, "v0_version"), "0.0.0.9001")
  expect_identical(
    sort(names(ref)),
    sort(c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8"))
  )
})

test_that("v1 reproduces v0 exactly on all eight parity fixtures", {
  skip_if_no_reference()
  source(file.path(parity_dir, "parity-fixtures.R"), local = TRUE)
  ref <- readRDS(file.path(parity_dir, "v0-reference.rds"))
  fixtures <- bm_parity_fixtures()

  # Numeric fields are compared at 1e-12, not at tolerance = 0.
  #
  # The reference was computed on one machine. Summation order in the engine is
  # fixed, but the platform's floating-point arithmetic is not: the same code
  # on Linux and Windows lands ~1e-16 away from the macOS reference. Demanding
  # bit equality therefore made this gate pass only where the reference was
  # built, which is the one place it proves least.
  #
  # 1e-12 is roughly ten thousand times the observed platform noise and far
  # below anything an actual engine change could produce -- a different
  # denominator, a different weight law or a different quantile type moves
  # these numbers in the third or fourth decimal, not the twelfth. Hashes and
  # policy strings are still compared exactly, below, because those are
  # bit-comparable everywhere.
  exact <- function(new, old, what) {
    testthat::expect_equal(new, old, tolerance = 1e-12, info = what)
  }

  for (id in names(ref)) {
    old <- ref[[id]]
    new <- if (identical(id, "F7")) {
      # F7 is the immutable G = 12 numerical receipt. Public calibrated calls
      # now fail closed below G = 22, so bypass only the public scope helper in
      # this test; no production option or engine override is introduced.
      testthat::with_mocked_bindings(
        bm_parity_evaluate(fixtures[[id]]),
        .bm_validate_cluster_calibration_scope =
          function(...) invisible(NULL),
        .package = "bayesmokken"
      )
    } else {
      bm_parity_evaluate(fixtures[[id]])
    }

    # --- deterministic engine ---------------------------------------------
    exact(new$pair, old$pair, paste(id, "pair"))
    exact(new$item, old$item, paste(id, "item"))
    exact(new$scale, old$scale, paste(id, "scale"))
    if ("weight_law" %in% names(new$estimand) &&
        !"weight_law" %in% names(old$estimand)) {
      # Step 2.4 records the declared ordinal weighting law additively. Keep
      # every v0 estimand field exact, then verify the new declaration.
      exact(
        new$estimand[names(old$estimand)],
        old$estimand,
        paste(id, "legacy estimand fields")
      )
      expected_law <- if (identical(fixtures[[id]]$mode,
                                    "clustered_ordinal")) {
        "hierarchical_cluster"
      } else {
        "iid_respondent"
      }
      expect_identical(new$estimand$weight_law, expected_law,
                       info = paste(id, "declared weight law"))
    } else {
      exact(new$estimand, old$estimand, paste(id, "estimand"))
    }
    exact(new$n_complete, old$n_complete, paste(id, "n_complete"))

    # --- preprocessing provenance hashes -----------------------------------
    expect_identical(new$row_hash, old$row_hash, info = paste(id, "row_hash"))
    expect_identical(new$score_map_hash, old$score_map_hash,
                     info = paste(id, "score_map_hash"))
    expect_identical(new$item_set_hash, old$item_set_hash,
                     info = paste(id, "item_set_hash"))

    # --- Bayesian-bootstrap path (fixtures that have one) ------------------
    if (!is.null(old$interval)) {
      if (id %in% c("F7", "F8")) {
        # Step 1.3 corrects clustered n_complete (respondents, not G) and adds
        # validation provenance. Compare every pre-existing numerical/policy
        # field except the formerly mislabelled n_complete receipt.
        legacy_fields <- setdiff(names(old$interval), "n_complete")
        exact(
          new$interval[legacy_fields],
          old$interval[legacy_fields],
          paste(id, "legacy interval fields")
        )
        expect_identical(new$interval$n_complete, new$n_complete,
                         info = paste(id, "corrected interval n_complete"))
      } else {
        exact(new$interval, old$interval, paste(id, "interval"))
      }
      exact(new$draws_scale, old$draws_scale, paste(id, "draws$scale"))
      exact(new$draws_item, old$draws_item, paste(id, "draws$item"))
      # Step 1.1 replaced delimiter-concatenated pair column labels with an
      # injective key. The draw values, dimensions and pair order remain frozen.
      exact(unname(new$draws_pair), unname(old$draws_pair),
            paste(id, "draws$pair values/order"))
      expect_identical(dim(new$draws_pair), dim(old$draws_pair),
                       info = paste(id, "draws$pair dimensions"))
      # The clustered engine's stream_map_hash and draw_identity_hash are
      # SHA-256 digests of the weight *matrix* (R/cluster-weights.R), not of
      # the integer stream states the binary path digests. A digest of doubles
      # inherits their platform dependence in full: the ~1e-16 accumulation
      # difference that the tolerance above absorbs changes the hash entirely.
      # Compare the seed and generator identity exactly -- those are the parts
      # that pin the RNG contract -- and let the float-derived digests be what
      # they are, a within-platform fingerprint of the draws.
      float_digest <- c("stream_map_hash", "draw_identity_hash")
      rng_fields <- setdiff(names(old$rng), float_digest)
      exact(new$rng[rng_fields], old$rng[rng_fields],
            paste(id, "rng provenance"))
      for (h in intersect(float_digest, names(old$rng))) {
        expect_true(is.character(new$rng[[h]]) && nzchar(new$rng[[h]]),
                    info = paste(id, h, "present"))
      }
      # MC diagnostics now add a typed status while retaining the complete v0
      # numerical receipt. Compare every legacy field exactly and assert the
      # additive state separately.
      exact(new$mc[names(old$mc)], old$mc,
            paste(id, "legacy mc diagnostics"))
      expect_identical(new$mc$status, "OK",
                       info = paste(id, "mc diagnostic status"))
    } else {
      expect_null(new$interval, info = paste(id, "must stay deterministic"))
    }
  }
})

test_that("the frozen method identifiers are unchanged", {
  skip_if_no_reference()
  ref <- readRDS(file.path(parity_dir, "v0-reference.rds"))
  ids <- vapply(
    ref[c("F1", "F2", "F3", "F7", "F8")],
    function(x) x$interval$method_id, character(1)
  )
  expect_identical(unname(ids[c("F1", "F2", "F3")]),
                   rep("P7-FS-WIDTH-RAW-G025-v1", 3L))
  expect_identical(unname(ids["F7"]), "V4-P15-HBB-RESP-GN150-v1")
  expect_identical(unname(ids["F8"]), "V4-P15-HBB-CLUSTER-GN150-v1")
})
