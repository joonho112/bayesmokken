# =============================================================================
# Diagnostics
#
# Four questions a reader is entitled to ask of any reported coefficient:
#   * are any denominators sitting on the boundary, where H is undefined or
#     unstable?
#   * for polytomous items, how did the transport that computes the maximum
#     covariance behave?
#   * are the item margins spread out enough for that maximum to be far from
#     zero?
#   * were enough bootstrap draws taken for the endpoints to settle?
#
# bm_diagnostics() answers all four in one call. The individual functions
# remain available for people who want just one.
# =============================================================================

#' Diagnose a fit
#'
#' Collects the denominator, regularity, transport and Monte Carlo diagnostics
#' into one report, and summarizes them in a sentence.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#' @param ... Passed to [bm_mc_diagnostics()].
#'
#' @return An object of class `bm_diagnostic_report` with elements
#'   `denominator`, `regularity`, `knots` (polytomous fits only), `mc`
#'   (bootstrap fits only) and `overall`.
#'
#' @section What the overall line does and does not say:
#' It reports whether anything numerically awkward was found: boundary
#' denominators, undefined draws, endpoints that have not settled. It is a
#' statement about the arithmetic, not about whether the scale is any good.
#' No verdict on the scale is offered, here or anywhere else in this package.
#'
#' @examples
#' fit <- bb_mokken_h(srs, draws = 200, seed = 1)
#' bm_diagnostics(fit)
#'
#' @family bayesmokken-diagnostics
#' @seealso [bm_regularity()], [bm_mc_diagnostics()], [bm_guardrails()]
#' @export
bm_diagnostics <- function(x, ...) {
  parts <- .bm_parts(x)
  emp <- parts$empirical
  dots <- list(...)

  pair <- bm_pair_h(x)
  item <- bm_item_h(x, include_interval = FALSE)
  denom <- data.frame(
    level = c("pair", "item", "scale"),
    n = c(nrow(pair), nrow(item), 1L),
    positive = c(sum(pair$denominator_status == "positive"),
                 sum(item$denominator_status == "positive"),
                 as.integer(emp$scale$denominator_status == "positive")),
    near_zero = c(sum(pair$denominator_status == "near_zero"),
                  sum(item$denominator_status == "near_zero"),
                  as.integer(emp$scale$denominator_status == "near_zero")),
    zero = c(sum(pair$denominator_status == "zero"),
             sum(item$denominator_status == "zero"),
             as.integer(emp$scale$denominator_status == "zero")),
    stringsAsFactors = FALSE
  )

  reg <- bm_regularity(x)
  knots <- if (identical(emp$estimand$item_type, "ordinal")) {
    bm_knot_diagnostics(x)
  } else {
    NULL
  }
  mc <- if (!is.null(parts$interval)) {
    if ("object" %in% names(dots)) {
      .bm_abort(
        "`object` cannot be supplied through `...` in `bm_diagnostics()`."
      )
    }
    do.call(bm_mc_diagnostics, c(list(object = x), dots))
  } else {
    if (length(dots)) {
      .bm_abort(
        "Monte Carlo diagnostic arguments apply to bootstrap fits only.",
        hint = paste(
          "Use bb_mokken_h() when endpoint Monte Carlo diagnostics",
          "are needed."
        )
      )
    }
    NULL
  }

  structure(
    list(denominator = denom, regularity = reg, knots = knots, mc = mc,
         item_type = emp$estimand$item_type,
         overall = .bm_overall_line(denom, mc, parts$interval)),
    class = "bm_diagnostic_report"
  )
}


#' Inspect item margins and denominator headroom
#'
#' Reports, for each item, its target-weighted mean and declared score range,
#' together with the smallest pair maximum covariance involving that item.
#' This last quantity is denominator headroom: values near zero identify pairs
#' at or near the denominator boundary.
#'
#' @param x A `bm_bb_h` or `bm_empirical_h` object.
#'
#' @return A data frame with one row per item: `item`, `mean_score`,
#'   `min_score`, `max_score`, `denominator`, `denominator_headroom`, the
#'   backward-compatible alias `min_pair_denominator`, and `worst_pair`, naming
#'   the partner item in the pair with the least headroom.
#'
#' @examples
#' bm_regularity(mokken_h(srs))
#'
#' @family bayesmokken-diagnostics
#' @seealso [bm_diagnostics()], [bm_plot_margins()]
#' @export
bm_regularity <- function(x) {
  parts <- .bm_parts(x)
  item <- bm_item_h(x, include_interval = FALSE)
  pair <- bm_pair_h(x)
  emp <- parts$empirical

  item_names <- as.character(item$item)
  ordinal <- identical(emp$estimand$item_type, "ordinal")
  if (ordinal) {
    supports <- emp$provenance$supports[item_names]
    if (length(supports) != length(item_names) ||
        any(vapply(supports, is.null, logical(1)))) {
      .bm_abort(
        "The ordinal empirical object has incomplete support provenance.",
        "bm_internal_error"
      )
    }
    min_score <- vapply(supports, min, numeric(1))
    max_score <- vapply(supports, max, numeric(1))
    if (!is.null(emp$category_mass)) {
      margins <- vapply(item_names, function(it) {
        mass <- .bm_category_mass_for_support(emp, it)
        sum(emp$provenance$supports[[it]] * mass)
      }, numeric(1))
    } else {
      margins <- .bm_item_margin_from_pairs(
        emp$pair, item_names, "mean_i", "mean_j"
      )
    }
  } else {
    margins <- .bm_item_margin_from_pairs(
      emp$pair, item_names, "p_i", "p_j"
    )
    min_score <- stats::setNames(rep(0, length(item_names)), item_names)
    max_score <- stats::setNames(rep(1, length(item_names)), item_names)
  }

  worst <- vapply(item_names, function(it) {
    rows <- pair[pair$item_i == it | pair$item_j == it, , drop = FALSE]
    usable <- which(is.finite(rows$max_covariance))
    if (!length(usable)) return(NA_character_)
    k <- usable[which.min(rows$max_covariance[usable])]
    partner <- if (rows$item_i[k] == it) rows$item_j[k] else rows$item_i[k]
    partner
  }, character(1))

  headroom <- vapply(item_names, function(it) {
    rows <- pair[pair$item_i == it | pair$item_j == it, , drop = FALSE]
    values <- rows$max_covariance[is.finite(rows$max_covariance)]
    if (!length(values)) return(NA_real_)
    min(values)
  }, numeric(1))

  out <- data.frame(
    item = item_names,
    mean_score = unname(margins),
    min_score = unname(min_score),
    max_score = unname(max_score),
    denominator = item$denominator,
    denominator_headroom = unname(headroom),
    min_pair_denominator = unname(headroom),
    worst_pair = unname(worst),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$denominator_headroom, out$item, method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

.bm_item_margin_from_pairs <- function(pair, items, left, right) {
  if (!all(c(left, right) %in% names(pair))) {
    .bm_abort(
      "The empirical object does not carry the required item margins.",
      "bm_internal_error"
    )
  }
  lookup <- c(
    stats::setNames(pair[[left]], pair$item_i),
    stats::setNames(pair[[right]], pair$item_j)
  )
  out <- unname(lookup[items])
  if (length(out) != length(items) || any(!is.finite(out))) {
    .bm_abort(
      "The empirical object has incomplete or nonfinite item margins.",
      "bm_internal_error"
    )
  }
  stats::setNames(out, items)
}

.bm_category_mass_for_support <- function(emp, item) {
  support <- emp$provenance$supports[[item]]
  mass <- emp$category_mass[[item]]
  if (is.null(support) || is.null(mass) || !is.numeric(mass)) {
    .bm_abort(
      "The ordinal empirical object does not carry item category masses.",
      "bm_internal_error"
    )
  }
  if (!is.null(names(mass))) {
    index <- match(as.character(support), names(mass))
    if (anyNA(index)) {
      .bm_abort(
        "The stored category masses do not match the declared support.",
        "bm_internal_error"
      )
    }
    mass <- unname(mass[index])
  }
  if (length(mass) != length(support) || any(!is.finite(mass)) ||
      any(mass < 0) || abs(sum(mass) - 1) > 1e-12) {
    .bm_abort(
      "The stored category masses are invalid for the declared support.",
      "bm_internal_error"
    )
  }
  stats::setNames(as.numeric(mass), as.character(support))
}


#' Transport knot diagnostics for polytomous fits
#'
#' Reports the structure of the mass transport that computes the maximum
#' covariance for polytomous items. The knots are the points at which the
#' transport path changes direction; they are an empirical diagnostic of the
#' denominator computation, not a quantity to interpret substantively.
#'
#' @param x A polytomous `bm_bb_h` or `bm_empirical_h` object.
#'
#' @return A data frame of knot counts and related quantities, as recorded by
#'   the estimation engine.
#'
#' @examplesIf FALSE
#' items <- paste0("wb", 1:6)
#' fit <- mokken_h(classroom_wb[, items], item_type = "ordinal",
#'                 supports = bm_supports(classroom_wb, items = items))
#' bm_knot_diagnostics(fit)
#'
#' @family bayesmokken-diagnostics
#' @seealso [bm_diagnostics()]
#' @export
bm_knot_diagnostics <- function(x) {
  parts <- .bm_parts(x)
  kd <- parts$empirical$knot_diagnostics
  if (is.null(kd)) {
    .bm_abort(
      "Knot diagnostics are recorded for polytomous fits only.",
      hint = "This fit used the binary path, which has a closed-form maximum."
    )
  }
  as.data.frame(kd, stringsAsFactors = FALSE)
}


# --- internals ---------------------------------------------------------------

# A factual sentence, never a verdict. It says what was found, and what to look
# at next when something was found.
.bm_overall_line <- function(denom, mc, interval) {
  issues <- character()
  bad <- sum(denom$near_zero) + sum(denom$zero)
  if (bad > 0) {
    issues <- c(issues, sprintf(
      "%d denominator(s) at or near zero", bad))
  }
  if (!is.null(interval) &&
      interval$defined_draws < interval$total_draws) {
    issues <- c(issues, sprintf(
      "%d of %d draws undefined",
      interval$total_draws - interval$defined_draws, interval$total_draws))
  }
  if (!is.null(interval) && is.null(mc)) {
    issues <- c(issues, "Monte Carlo diagnostics are unavailable")
  }
  if (!is.null(mc) && identical(mc$status, "TOO_FEW_DEFINED_DRAWS")) {
    issues <- c(issues, sprintf(
      "only %d finite draw(s) for %d requested Monte Carlo batches",
      mc$defined_draws, mc$batches
    ))
  }
  if (!is.null(mc) && identical(mc$status, "ALL_DRAWS_UNDEFINED") &&
      (is.null(interval) || interval$defined_draws == interval$total_draws)) {
    issues <- c(issues, "all bootstrap draws are undefined")
  }
  if (!is.null(mc) && !is.null(interval) &&
      is.finite(mc$lower_endpoint_mcse) && is.finite(interval$upper)) {
    width <- interval$upper - interval$lower
    rel <- max(mc$lower_endpoint_mcse, mc$upper_endpoint_mcse) / width
    if (is.finite(rel) && rel > 0.05) {
      issues <- c(issues, sprintf(
        "endpoint Monte Carlo error is %.1f%% of the interval width", 100 * rel))
    }
  }
  if (!length(issues)) {
    return("Nothing numerically awkward found.")
  }
  paste0("Worth a look: ", paste(issues, collapse = "; "), ".")
}
