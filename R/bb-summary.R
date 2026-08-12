.bm_draw_summary <- function(x, label) {
  defined <- !is.na(x)
  values <- x[defined]
  if (!length(values)) {
    return(data.frame(
      variable = label, mean = NA_real_, median = NA_real_, sd = NA_real_,
      q025 = NA_real_, q975 = NA_real_, defined_draws = 0L,
      total_draws = length(x), defined_fraction = 0,
      stringsAsFactors = FALSE
    ))
  }
  quantiles <- stats::quantile(
    values, probs = c(0.025, 0.50, 0.975),
    names = FALSE, type = 8
  )
  data.frame(
    variable = label,
    mean = mean(values),
    median = quantiles[2L],
    sd = stats::sd(values),
    q025 = quantiles[1L],
    q975 = quantiles[3L],
    defined_draws = length(values),
    total_draws = length(x),
    defined_fraction = mean(defined),
    stringsAsFactors = FALSE
  )
}

.bm_summarize_matrix <- function(x) {
  do.call(rbind, lapply(seq_len(ncol(x)), function(j) {
    .bm_draw_summary(x[, j], colnames(x)[j])
  }))
}

bm_bb_summary <- function(object) {
  if (!inherits(object, "bm_bb_h")) {
    .bm_abort("Expected a `bm_bb_h` object.")
  }
  pair <- object$empirical$pair
  aligned_pair <- .bm_align_pair_draws(
    object$draws$pair,
    as.character(pair$item_i),
    as.character(pair$item_j)
  )
  pair_draws <- .bm_summarize_matrix(aligned_pair$draws)
  # Keep all nine legacy summary columns in place. Pair coordinates are
  # additive display fields; `variable` remains an opaque compatibility key.
  pair_draws$item_i <- as.character(pair$item_i)
  pair_draws$item_j <- as.character(pair$item_j)
  structure(
    list(
      bb_type = object$bb_type,
      interval = object$interval,
      scale_draws = .bm_draw_summary(object$draws$scale, "scale_H"),
      item_draws = .bm_summarize_matrix(object$draws$item),
      pair_draws = pair_draws,
      interpretation = object$interpretation
    ),
    class = "summary_bm_bb_h"
  )
}
