# Pair identities must not depend on a delimiter that can also occur in an item
# name. The byte-length prefixes make this encoding injective without reserving
# any otherwise-valid character. Item order is preserved deliberately: callers
# supply the canonical i < j order used by the empirical pair table.
.bm_pair_key <- function(item_i, item_j) {
  if (!is.character(item_i) || !is.character(item_j) ||
      length(item_i) != length(item_j) || anyNA(item_i) || anyNA(item_j) ||
      any(!nzchar(item_i)) || any(!nzchar(item_j))) {
    .bm_abort(
      "Internal pair keys require equally sized vectors of nonempty item names.",
      "bm_invariant_error"
    )
  }

  paste0(
    "bm-pair-v1:",
    nchar(item_i, type = "bytes"), ":", item_i,
    "|",
    nchar(item_j, type = "bytes"), ":", item_j
  )
}
