#' Diagnose Monte Carlo error in interval endpoints
#'
#' Estimates the numerical variability of the reported interval endpoints by
#' dividing the finite scale-level bootstrap draws into nonoverlapping batches.
#' It also compares the endpoints from the first half of the finite draws with
#' the endpoints from all finite draws. These quantities diagnose whether the
#' requested number of draws was enough for the endpoints to settle; they do
#' not validate interval calibration or provide threshold evidence.
#'
#' @param object A completed `bm_bb_h` object returned by [bb_mokken_h()].
#' @param level Confidence level used for the endpoint quantiles. It must be
#'   exactly `0.95`, matching the package's frozen interval contract.
#' @param batches Number of nonoverlapping batches. Must be one finite integer
#'   of at least 2. If fewer finite draws than batches are available, the
#'   function returns the explicit `"TOO_FEW_DEFINED_DRAWS"` state rather than
#'   silently reducing the requested batch count.
#' @param ... Must be empty. It is accepted only so misspelled arguments can
#'   produce a typed `bm_input_error` instead of an unclassified base error.
#'
#' @return A named list with the same fields and field types in every state:
#'
#' - `defined_draws`, `total_draws`: integer draw counts.
#' - `defined_fraction`, `level`: numeric scalars.
#' - `lower_endpoint_mcse`, `upper_endpoint_mcse`: numeric endpoint Monte Carlo
#'   standard errors, or `NA_real_` when they cannot be computed.
#' - `interval_batch_max_deviation`: numeric maximum absolute difference
#'   between a batch endpoint and its corresponding full-draw endpoint.
#' - `prefix_half_endpoint_max_difference`: numeric maximum absolute endpoint
#'   difference between the first half and all finite draws; `NA_real_` when
#'   fewer than four finite draws are available.
#' - `batches`: the requested integer batch count. It is never silently
#'   reduced.
#' - `quantile_type`: integer `8`, the quantile convention used throughout the
#'   package.
#' - `method_id`, `contract`, `status`: character scalars. `status` is one of
#'   `"OK"`, `"OK_WITH_UNDEFINED_DRAWS"`, `"ALL_DRAWS_UNDEFINED"`, or
#'   `"TOO_FEW_DEFINED_DRAWS"`.
#'
#' @section What the diagnostics describe:
#' The batch and prefix endpoints are rebuilt with the construction the
#' reported interval used: midpoint-and-half-width scaling on the binary and
#' the historical clustered paths, and reflection about the point estimate
#' with the expansion by \eqn{k_G} on the default clustered path, where the
#' diagnostics describe the endpoints before their intersection with
#' \eqn{(-\infty, 1]}.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_mc_diagnostics(fit)
#'
#' @family bayesmokken-diagnostics
#' @seealso [bm_diagnostics()], [bm_plot_mc_stability()]
#' @export
bm_mc_diagnostics <- function(object, level = 0.95, batches = 4L, ...) {
  dots <- list(...)
  if (length(dots)) {
    dot_names <- names(dots)
    if (is.null(dot_names)) {
      dot_names <- rep("", length(dots))
    }
    labels <- ifelse(
      nzchar(dot_names),
      paste0("`", dot_names, "`"),
      "<unnamed>"
    )
    .bm_abort(sprintf(
      "Unused argument(s) in `bm_mc_diagnostics()`: %s.",
      paste(labels, collapse = ", ")
    ))
  }
  if (!inherits(object, "bm_bb_h") || !is.list(object)) {
    .bm_abort("Expected a `bm_bb_h` object.")
  }
  level <- .bm_validate_interval_level(level)
  if (!is.numeric(batches) || length(batches) != 1L ||
      !is.finite(batches) || batches < 2 ||
      batches > .Machine$integer.max || batches != floor(batches)) {
    .bm_abort("`batches` must be one finite integer of at least 2.")
  }
  batches <- as.integer(batches)

  if (!is.list(object$draws) ||
      !is.numeric(object$draws$scale) ||
      is.matrix(object$draws$scale) ||
      is.data.frame(object$draws$scale) ||
      !length(object$draws$scale)) {
    .bm_abort(
      "`object$draws$scale` must be one nonempty numeric draw vector."
    )
  }
  if (!is.list(object$interval) ||
      !is.numeric(object$interval$width_multiplier) ||
      length(object$interval$width_multiplier) != 1L ||
      !is.finite(object$interval$width_multiplier) ||
      object$interval$width_multiplier <= 0 ||
      !is.character(object$interval$method_id) ||
      length(object$interval$method_id) != 1L ||
      is.na(object$interval$method_id) ||
      !nzchar(object$interval$method_id)) {
    .bm_abort(
      "`object$interval` must contain a valid width multiplier and method ID."
    )
  }

  x <- object$draws$scale
  defined <- is.finite(x)
  values <- x[defined]
  common <- list(
    defined_draws = length(values),
    total_draws = length(x),
    defined_fraction = mean(defined),
    level = level,
    batches = batches,
    method_id = object$interval$method_id
  )
  if (!length(values)) {
    return(do.call(
      .bm_new_mc_diagnostics,
      c(common, list(status = "ALL_DRAWS_UNDEFINED"))
    ))
  }
  if (length(values) < batches) {
    return(do.call(
      .bm_new_mc_diagnostics,
      c(common, list(status = "TOO_FEW_DEFINED_DRAWS"))
    ))
  }

  batch_id <- cut(
    seq_along(values),
    breaks = batches,
    labels = FALSE,
    include.lowest = TRUE
  )
  probabilities <- c((1 - level) / 2, (1 + level) / 2)
  full_quantile <- stats::quantile(
    values, probabilities, names = FALSE, type = 8L
  )
  batch_quantile <- vapply(
    split(values, batch_id),
    function(v) stats::quantile(
      v, probabilities, names = FALSE, type = 8L
    ),
    numeric(2L)
  )

  # The endpoints are rebuilt the way the interval was built. The binary and
  # the historical clustered constructions scale the half-width about the
  # percentile midpoint; the complete-record construction reflects the
  # quantiles about the point estimate and expands the reflected errors. For
  # the latter the diagnostics describe the endpoints before their
  # intersection with (-Inf, 1], which is a deterministic cap.
  reflected <- identical(
    object$interval$construction, "reflected_error_expansion_about_point"
  )
  point <- object$interval$point_estimate
  calibrate <- function(endpoint, multiplier) {
    if (reflected) {
      return(c(
        lower = point - multiplier * (endpoint[2L] - point),
        upper = point - multiplier * (endpoint[1L] - point)
      ))
    }
    midpoint <- (endpoint[1L] + endpoint[2L]) / 2
    halfwidth <- (endpoint[2L] - endpoint[1L]) / 2
    c(
      lower = midpoint - multiplier * halfwidth,
      upper = midpoint + multiplier * halfwidth
    )
  }
  multiplier <- object$interval$width_multiplier
  full_endpoint <- calibrate(full_quantile, multiplier)
  batch_endpoint <- apply(
    batch_quantile,
    2L,
    calibrate,
    multiplier = multiplier
  )

  half_n <- floor(length(values) / 2)
  half_endpoint <- if (half_n >= 2L) {
    calibrate(
      stats::quantile(
        values[seq_len(half_n)],
        probabilities,
        names = FALSE,
        type = 8L
      ),
      multiplier
    )
  } else {
    c(lower = NA_real_, upper = NA_real_)
  }
  endpoint_mcse <- apply(
    batch_endpoint,
    1L,
    stats::sd
  ) / sqrt(ncol(batch_endpoint))
  prefix_difference <- if (all(is.finite(half_endpoint))) {
    max(abs(half_endpoint - full_endpoint))
  } else {
    NA_real_
  }

  do.call(
    .bm_new_mc_diagnostics,
    c(common, list(
      status = if (all(defined)) {
        "OK"
      } else {
        "OK_WITH_UNDEFINED_DRAWS"
      },
      lower_endpoint_mcse = endpoint_mcse[1L],
      upper_endpoint_mcse = endpoint_mcse[2L],
      interval_batch_max_deviation = max(abs(
        sweep(batch_endpoint, 1L, full_endpoint, "-")
      )),
      prefix_half_endpoint_max_difference = prefix_difference
    ))
  )
}

.bm_new_mc_diagnostics <- function(
    defined_draws, total_draws, defined_fraction, level, batches,
    method_id, status, lower_endpoint_mcse = NA_real_,
    upper_endpoint_mcse = NA_real_,
    interval_batch_max_deviation = NA_real_,
    prefix_half_endpoint_max_difference = NA_real_) {
  list(
    defined_draws = as.integer(defined_draws),
    total_draws = as.integer(total_draws),
    defined_fraction = unname(as.double(defined_fraction)[1L]),
    level = unname(as.double(level)[1L]),
    # Preserve the v0 scalar names on the two endpoint fields. The names are
    # useful in direct inspection and are part of the frozen numerical receipt.
    lower_endpoint_mcse = stats::setNames(
      as.double(lower_endpoint_mcse)[1L], "lower"
    ),
    upper_endpoint_mcse = stats::setNames(
      as.double(upper_endpoint_mcse)[1L], "upper"
    ),
    interval_batch_max_deviation = unname(as.double(
      interval_batch_max_deviation
    )[1L]),
    prefix_half_endpoint_max_difference = unname(as.double(
      prefix_half_endpoint_max_difference
    )[1L]),
    batches = as.integer(batches),
    quantile_type = 8L,
    method_id = unname(as.character(method_id)[1L]),
    contract = paste(
      "Endpoint Monte Carlo diagnostics only;",
      "not interval-calibration or threshold evidence."
    ),
    status = unname(as.character(status)[1L])
  )
}
