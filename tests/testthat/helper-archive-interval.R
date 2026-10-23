# =============================================================================
# Reference arithmetic of the adjusted complete-record interval
#
# A verbatim copy of `v4p6_candidate_interval()` from the clustered article's
# replication archive (analysis/R/method-candidates.R, PHASE06-CANDIDATES-v1),
# the implementation the article calls its reference. It is vendored here,
# unchanged apart from this header, so that the package's interval constructor
# can be tested for exact agreement with it on identical draw vectors without
# depending on the archive's location. Do not edit it: the point of the test is
# that this is the archive's arithmetic, not a restatement of ours.
# =============================================================================

.archive_candidate_ids <- function() {
  c("one_stage_basic", "one_stage_basic_G_adjusted")
}

.archive_candidate_interval <- function(values, point, G, method_id) {
  if (length(method_id) != 1L || !method_id %in% .archive_candidate_ids())
    stop("Unknown Phase 6 candidate method_id.")
  if (length(G) != 1L || !is.finite(G) || G < 2L || G != as.integer(G))
    stop("Candidate G must be a finite integer >= 2.")
  if (!is.numeric(values) || !is.null(dim(values)) || length(point) != 1L || !is.numeric(point))
    stop("Supply a numeric H draw vector and scalar point.")
  n <- length(values); finite <- is.finite(values); kept <- sum(finite)
  source_variance <- if (kept > 1L) stats::var(values[finite]) else NA_real_
  k <- if (method_id == "one_stage_basic") 1 else
    sqrt((G+1)/(G-1)) * stats::qt(.975, df = G-1) / stats::qnorm(.975)
  ans <- list(method_id = method_id, candidate_version = "PHASE06-CANDIDATES-v1",
    source_law = "one_stage_bb", construction_scale = "H", root_center = point,
    root_multiplier = k, lower = NA_real_, upper = NA_real_, lower_raw = NA_real_, upper_raw = NA_real_,
    center = NA_real_, interval_length = NA_real_, set_measure = NA_real_,
    quantile_lower = NA_real_, quantile_upper = NA_real_,
    draw_variance = source_variance, scaled_error_root_variance = k^2*source_variance,
    defined_draws = as.integer(kept), total_draws = as.integer(n),
    invalid_fraction = if (n) mean(!finite) else NA_real_,
    boundary_draws = as.integer(sum(values[finite] == 1)),
    bound_violation_draws = as.integer(sum(values[finite] > 1)),
    point_boundary = is.finite(point) && point == 1,
    point_bound_violation = is.finite(point) && point > 1,
    computed = FALSE, set_empty = FALSE, upper_intersection = FALSE,
    zero_width = FALSE, numerical_failure = FALSE, computational_failure = FALSE,
    operational_failure = TRUE, success = FALSE, status = "UNINITIALIZED")
  fail <- function(status, numerical = TRUE) {
    ans$status <- status; ans$numerical_failure <- numerical; ans$computational_failure <- numerical; ans
  }
  if (!is.finite(point)) return(fail("POINT_UNDEFINED"))
  if (point > 1) return(fail("POINT_UPPER_BOUND_VIOLATION"))
  if (kept < max(20L, ceiling(.99*n))) return(fail("INSUFFICIENT_DEFINED_DRAWS"))
  if (ans$bound_violation_draws > 0L) return(fail("DRAW_UPPER_BOUND_VIOLATION"))
  if (!is.finite(k) || k <= 0) return(fail("INVALID_ROOT_MULTIPLIER"))
  q <- as.numeric(stats::quantile(values[finite], c(.025, .975), type = 8L, names = FALSE))
  ans$quantile_lower <- q[1]; ans$quantile_upper <- q[2]
  # This is expansion of h* - h about the point, not midpoint width scaling.
  ans$lower_raw <- point - k*(q[2]-point)
  ans$upper_raw <- point - k*(q[1]-point)
  if (any(!is.finite(c(ans$lower_raw, ans$upper_raw)))) return(fail("NONFINITE_ENDPOINT"))
  if (ans$lower_raw > ans$upper_raw) return(fail("REVERSED_RAW_ENDPOINTS"))
  ans$computed <- TRUE
  ans$upper_intersection <- ans$upper_raw > 1
  if (ans$lower_raw > 1) {
    ans$set_empty <- TRUE; ans$set_measure <- 0
    # Legitimate empty confidence set: noncoverage, not a computation error.
    return(fail("EMPTY_PARAMETER_INTERSECTION", numerical = FALSE))
  }
  ans$lower <- ans$lower_raw; ans$upper <- min(ans$upper_raw, 1)
  ans$center <- ans$lower/2 + ans$upper/2
  ans$interval_length <- ans$upper-ans$lower
  if (!is.finite(ans$interval_length)) {
    ans$lower <- ans$upper <- ans$center <- ans$interval_length <- NA_real_
    return(fail("NONFINITE_INTERVAL_LENGTH"))
  }
  ans$set_measure <- ans$interval_length; ans$zero_width <- ans$interval_length == 0
  ans$success <- TRUE; ans$operational_failure <- FALSE; ans$status <- "OK"
  ans
}

# The archive's percentile comparator (`v4r_percentile()` in comparators.R),
# likewise copied verbatim apart from the name.
.archive_percentile <- function(values, level = .95, minimum_defined_fraction = .99) {
  finite <- is.finite(values); n <- length(values); kept <- sum(finite)
  okay <- kept >= ceiling(minimum_defined_fraction * n) && kept >= 20L
  endpoints <- if (okay) as.numeric(stats::quantile(values[finite], c((1-level)/2, 1-(1-level)/2), type = 8L)) else c(NA_real_, NA_real_)
  list(lower = endpoints[1], upper = endpoints[2], center = mean(endpoints),
    interval_length = diff(endpoints), draw_variance = if (kept > 1L) stats::var(values[finite]) else NA_real_,
    defined_draws = kept, total_draws = n, invalid_fraction = mean(!finite),
    status = if (okay) "OK" else "INSUFFICIENT_DEFINED_DRAWS")
}

# The archive's draw-major weight stream (`v4r_weight_stream()` in
# weight-stream.R), reduced to the complete-record ("one_stage_bb") outputs.
# Independent of the package's generator: it draws the archive's G + N
# exponentials per draw in blocks of `block`, which the package does not.
.archive_crbb_weights <- function(cluster, draws, seed, block = 128L) {
  index <- match(cluster, unique(cluster)); sizes <- tabulate(index)
  G <- length(sizes); N <- length(index)
  old_kind <- RNGkind()
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  respondent <- matrix(NA_real_, N, draws)
  equal_cluster <- matrix(NA_real_, N, draws)
  generated <- 0L
  while (generated < draws) {
    n <- min(block, draws - generated)
    E <- matrix(stats::rexp((G + N) * n), G + N, n)
    A <- sweep(E[seq_len(G), , drop = FALSE], 2L,
               colSums(E[seq_len(G), , drop = FALSE]), "/")
    OC <- A[index, , drop = FALSE] / sizes[index]
    denominator <- colSums(sizes * A)
    OR <- sweep(A[index, , drop = FALSE], 2L, denominator, "/")
    columns <- generated + seq_len(n)
    respondent[, columns] <- OR
    equal_cluster[, columns] <- OC
    generated <- generated + n
  }
  list(respondent = respondent, equal_cluster = equal_cluster,
       G = G, N = N, sizes = sizes, index = index)
}
