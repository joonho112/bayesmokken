# =============================================================================
# Published-numbers gate
#
# Proves that bayesmokken reproduces the numbers printed in the two articles.
#
# WHAT THIS GATE CAN AND CANNOT CLAIM
# -----------------------------------
# Neither empirical application was produced by a single bb_mokken_h() call.
# Both used the research harness, which draws random weights under its own
# sharded scheme:
#
#   * Paper A (SRS)   research/v3/empirical-ppsrs/analysis/run_ppsrs_...R
#                     set.seed(2026072611) + rexp(), 50,000 draws, one process.
#   * Paper B (SWMDK) research/v4/.../phase17, 12 tasks x 99,999 draws,
#                     batch_size 2,000, seed = 2026075701 + 10000*task + batch.
#
# The package's own bb_mokken_h() uses L'Ecuyer-CMRG streams, so its DRAWS are
# a different (equally valid) Monte Carlo sample. Draw-for-draw reproduction of
# the published intervals from a package call is therefore impossible by
# construction, and any gate claiming otherwise would be false.
#
# What the articles actually claim, and what this script verifies, is:
#
#   A. POINT ESTIMATES are exact. H is a deterministic functional of the data;
#      no RNG is involved. Checked at tolerance 1e-12.
#
#   B. THE INTERVAL ALGORITHM is exact. Feeding the archived draw vectors into
#      the package's interval construction reproduces the published endpoints.
#      This is the claim Paper A states as "reproduced the empirical
#      application's interval endpoints from saved draws to a maximum absolute
#      difference of 1.1e-15". Checked at tolerance 1e-12.
#
#   C. A PLAIN PACKAGE CALL AGREES within Monte Carlo error. This is a sanity
#      band, not an exactness claim. The band is stated explicitly below.
#
# Usage (from the package root):
#     Rscript inst/scripts/verify-published-numbers.R
#
# Requires the two source datasets, which are NOT bundled:
#     * SRS  : IRW local database (sha256-checked below)
#     * SWMDK: mokken::SWMDK
# Missing sources cause the affected section to be reported as SKIPPED, never
# silently passed.
# =============================================================================

suppressPackageStartupMessages(library(bayesmokken))

TOL_EXACT <- 1e-12
results <- list()

record <- function(section, check, expected, observed, tol, status = NULL) {
  diff <- if (is.numeric(expected) && is.numeric(observed)) {
    max(abs(expected - observed))
  } else {
    NA_real_
  }
  if (is.null(status)) {
    status <- if (is.na(diff)) "SKIP" else if (diff <= tol) "PASS" else "FAIL"
  }
  results[[length(results) + 1L]] <<- data.frame(
    section = section, check = check,
    expected = paste(format(expected, digits = 15), collapse = " , "),
    observed = paste(format(observed, digits = 15), collapse = " , "),
    max_abs_diff = diff, tolerance = tol, status = status,
    stringsAsFactors = FALSE
  )
  cat(sprintf("  [%s] %-46s  max|diff| = %s\n", status, check,
              format(diff, digits = 3)))
  invisible(NULL)
}

research_root <- Sys.getenv(
  "BAYESMOKKEN_RESEARCH_ROOT",
  normalizePath(file.path("..", "paper-A-bayesian-scalability", "codebase",
                          "research"), mustWork = FALSE)
)

# =============================================================================
# A. Paper A -- Scientific Reasoning Scale (n = 337, 11 binary items)
# =============================================================================
cat("\n== Paper A / SRS =============================================\n")

# Outside this package by design: the gate reads the authors' archived source
# rather than a copy vendored here. Set the variable to your own copy.
srs_rds <- Sys.getenv("BAYESMOKKEN_SRS_RDS", "")
SRS_SHA256 <- "72ca8785e01bcc78f26ff616f7a2249161d6072676cd65e0d0eb96ba84def6fc"
SRS_ITEMS <- paste0("SRS", 1:11)

published_srs <- list(
  H          = 0.130205929962575,
  raw        = c(0.102910243347665, 0.161443058765301),
  calibrated = c(0.102511682644250, 0.161841619468715),
  multiplier = 1.01361836776757,
  seed       = 2026072611L,
  draws      = 50000L
)

srs_wide <- NULL
if (file.exists(srs_rds)) {
  obs_sha <- digest::digest(file = srs_rds, algo = "sha256")
  record("A/SRS", "source file sha256", 0, 0, 0,
         status = if (identical(obs_sha, SRS_SHA256)) "PASS" else "FAIL")
  long <- readRDS(srs_rds)
  srs_wide <- stats::reshape(
    long[, c("id", "item", "resp")], idvar = "id", timevar = "item",
    direction = "wide"
  )
  names(srs_wide) <- sub("^resp\\.", "", names(srs_wide))
  rownames(srs_wide) <- srs_wide$id
  srs_wide <- srs_wide[order(as.integer(as.character(srs_wide$id))),
                       SRS_ITEMS, drop = FALSE]
  srs_wide[] <- lapply(srs_wide, as.numeric)
} else {
  cat("  [SKIP] SRS source not found at ", srs_rds, "\n", sep = "")
}

if (!is.null(srs_wide)) {
  # --- A1. deterministic point estimate ------------------------------------
  det <- mokken_h(srs_wide, warn = FALSE)
  record("A/SRS", "scale H (deterministic)", published_srs$H,
         det$scale$H, TOL_EXACT)
  record("A/SRS", "complete-scale n", 337, det$provenance$n_complete, 0)

  # --- A2. interval algorithm on the ARCHIVED draws ------------------------
  draw_rds <- file.path(research_root, "v3", "empirical-ppsrs", "artifacts",
                        "ppsrs-bb-scale-h-draws-v1.rds")
  if (file.exists(draw_rds)) {
    archived <- readRDS(draw_rds)
    stopifnot(nrow(archived) == published_srs$draws,
              all(archived$seed == published_srs$seed))
    itv <- bayesmokken:::.bm_interval_from_draws(
      bb_draws = archived$scale_H, point_estimate = det$scale$H,
      n_complete = det$provenance$n_complete,
      interval = "calibrated", level = 0.95
    )
    raw <- bayesmokken:::.bm_interval_from_draws(
      bb_draws = archived$scale_H, point_estimate = det$scale$H,
      n_complete = det$provenance$n_complete,
      interval = "equal_tail", level = 0.95
    )
    record("A/SRS", "calibrated endpoints from archived draws",
           published_srs$calibrated, c(itv$lower, itv$upper), TOL_EXACT)
    record("A/SRS", "raw endpoints from archived draws",
           published_srs$raw, c(raw$lower, raw$upper), TOL_EXACT)
    record("A/SRS", "width multiplier", published_srs$multiplier,
           itv$width_multiplier, TOL_EXACT)
    record("A/SRS", "defined draws", published_srs$draws,
           itv$defined_draws, 0)
  } else {
    cat("  [SKIP] archived SRS draws not found\n")
  }

  # --- A3. Monte Carlo agreement of a plain package call -------------------
  # Band: the article reports an endpoint MCSE-scale disagreement well under
  # 0.002 at 50,000 draws. 0.005 is a deliberately loose sanity band; it is a
  # smoke check on the whole pipeline, not evidence about calibration.
  fit <- bb_mokken_h(srs_wide, draws = 50000L, seed = 1L, warn = FALSE)
  record("A/SRS", "plain call endpoints (MC band 0.005)",
         published_srs$calibrated, c(fit$interval$lower, fit$interval$upper),
         0.005)
  record("A/SRS", "plain call point estimate", published_srs$H,
         fit$interval$point_estimate, TOL_EXACT)
}

# =============================================================================
# B. Paper B -- SWMDK (639 pupils, 30 classrooms, 5-point items)
# =============================================================================
cat("\n== Paper B / SWMDK ===========================================\n")

SWMDK_SCALES <- list(
  teacher   = paste0("Item", 1:6),
  classmate = c("Item8", "Item9", "Item10", "Item11", "Item13")
)
SWMDK_TASK <- list(
  teacher_respondent    = "P17-T01", teacher_equal_cluster   = "P17-T02",
  classmate_respondent  = "P17-T07", classmate_equal_cluster = "P17-T08"
)
published_swmdk <- list(
  teacher_respondent = list(
    H = 0.619919910686637, ci = c(0.572846061948732, 0.669202776257687)),
  teacher_equal_cluster = list(
    H = 0.622319152142876, ci = c(0.574284671698886, 0.669839481460374)),
  classmate_respondent = list(
    H = 0.592308560081349, ci = c(0.543055771947031, 0.640839138707681)),
  classmate_equal_cluster = list(
    H = 0.582943580481479, ci = c(0.534723913209442, 0.634478839596969))
)
PUBLISHED_MULTIPLIER_G30 <- 0.7261387212474169

if (!requireNamespace("mokken", quietly = TRUE)) {
  cat("  [SKIP] package 'mokken' not installed; SWMDK section skipped\n")
} else {
  utils::data("SWMDK", package = "mokken", envir = environment())
  swmdk <- get("SWMDK", envir = environment())
  cluster <- as.character(swmdk$classId)
  record("B/SWMDK", "respondents", 639, nrow(swmdk), 0)
  record("B/SWMDK", "clusters", 30, length(unique(cluster)), 0)

  for (sc in names(SWMDK_SCALES)) {
    items <- SWMDK_SCALES[[sc]]
    dat <- swmdk[, items, drop = FALSE]
    # Observed scores are 1..5 (the documented 0..4 coding is a shift; the
    # package object is already scored, so no reverse keying is applied).
    sup <- stats::setNames(rep(list(1:5), length(items)), items)

    for (tg in c("respondent", "equal_cluster")) {
      key <- paste0(sc, "_", tg)
      pub <- published_swmdk[[key]]

      # --- B1. deterministic point estimate --------------------------------
      det <- mokken_h(dat, item_type = "ordinal", supports = sup,
                      cluster = cluster, cluster_target = tg,
                      weight_law = "hierarchical_cluster", warn = FALSE)
      record("B/SWMDK", paste(key, "point H (deterministic)"),
             pub$H, det$scale$H, TOL_EXACT)

      # --- B2. interval algorithm on the ARCHIVED draws ---------------------
      gz <- file.path(research_root, "v4", "cluster-polytomous",
                      "empirical-swmdk", "phase17", "aws-raw",
                      paste0(SWMDK_TASK[[key]], "-draws-v1.csv.gz"))
      if (file.exists(gz)) {
        con <- gzfile(gz, "rt")            # read-only; never decompress in place
        arch <- utils::read.csv(con, stringsAsFactors = FALSE)
        try(close(con), silent = TRUE)
        itv <- bayesmokken:::.bm_cluster_interval_from_draws(
          bb_draws = arch$H, point_estimate = det$scale$H,
          n_complete = det$provenance$n_complete,
          cluster_count = 30L, cluster_target = tg,
          interval = "calibrated", level = 0.95
        )
        record("B/SWMDK", paste(key, "endpoints from archived draws"),
               pub$ci, c(itv$lower, itv$upper), TOL_EXACT)
        record("B/SWMDK", paste(key, "width multiplier"),
               PUBLISHED_MULTIPLIER_G30, itv$width_multiplier, TOL_EXACT)
      } else {
        cat("  [SKIP] archived draws not found for ", key, "\n", sep = "")
      }

      # --- B3. Monte Carlo agreement of a plain package call ---------------
      # Band 0.02: the published run used 99,999 draws; this call uses 4,000,
      # and the article's own split-half endpoint spread was 8.8e-4 at the
      # published draw count. This is a pipeline smoke check only.
      fit <- bb_mokken_h(dat, item_type = "ordinal", supports = sup,
                         cluster = cluster, cluster_target = tg,
                         weight_law = "hierarchical_cluster",
                         draws = 4000L, seed = 1L, warn = FALSE)
      record("B/SWMDK", paste(key, "plain call endpoints (MC band 0.02)"),
             pub$ci, c(fit$interval$lower, fit$interval$upper), 0.02)
    }
  }
}

# =============================================================================
# Summary
# =============================================================================
out <- do.call(rbind, results)
cat("\n== SUMMARY ===================================================\n")
print(table(out$status))
failed <- out[out$status == "FAIL", , drop = FALSE]
if (nrow(failed)) {
  cat("\nFAILURES:\n"); print(failed[, c("section", "check", "max_abs_diff")])
}
target_csv <- file.path("inst", "extdata", "published-targets.csv")
if (dir.exists(dirname(target_csv))) {
  utils::write.csv(out, target_csv, row.names = FALSE)
  cat("\nWritten: ", target_csv, "\n", sep = "")
}
invisible(if (nrow(failed)) quit(status = 1) else out)
