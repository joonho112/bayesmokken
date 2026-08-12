.bm_ratio_with_state <- function(numerator, denominator, zero_tol, eta) {
  state <- .bm_denominator_state(denominator, zero_tol, eta)
  list(
    numerator = numerator,
    denominator_raw = state$raw,
    denominator = state$value,
    H = if (state$status == "zero") NA_real_ else numerator / state$value,
    denominator_status = state$status
  )
}

bm_h_aggregate <- function(pair, item_names,
                           zero_tol = 64 * .Machine$double.eps,
                           eta = sqrt(.Machine$double.eps)) {
  item <- vector("list", length(item_names))
  for (i in seq_along(item_names)) {
    keep <- pair$item_i == item_names[i] | pair$item_j == item_names[i]
    ratio <- .bm_ratio_with_state(
      sum(pair$cov[keep]),
      sum(pair$cov_max[keep]),
      zero_tol,
      eta
    )
    item[[i]] <- data.frame(
      item = item_names[i],
      cov_sum = ratio$numerator,
      cov_max_sum = ratio$denominator,
      H_i = ratio$H,
      denominator_status = ratio$denominator_status,
      stringsAsFactors = FALSE
    )
  }
  scale <- .bm_ratio_with_state(
    sum(pair$cov),
    sum(pair$cov_max),
    zero_tol,
    eta
  )
  scale <- list(
    cov_sum = scale$numerator,
    cov_max_sum = scale$denominator,
    H = scale$H,
    denominator_status = scale$denominator_status
  )
  list(item = do.call(rbind, item), scale = scale)
}
