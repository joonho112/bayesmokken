# =============================================================================
# data-raw/classroom_wb.R  --  build data/classroom_wb.rda
#
# A FULLY SYNTHETIC clustered polytomous dataset, produced by bm_simulate().
# No real respondent contributed to it.
#
# Why it exists
#   The clustered path of this package needs a dataset that every example,
#   test and vignette can use without an external dependency. The real
#   clustered application of Paper B is mokken::SWMDK, which cannot be bundled:
#   the article's data manifest sets redistribution policy
#   "no_vendoring_load_from_installed_mokken", and mokken is GPL while this
#   package is MIT.
#
#   So SWMDK is reached through Suggests when it is installed, and this
#   synthetic set carries everything else.
#
# Design size only
#   The number of pupils, classrooms, categories and the intraclass
#   correlation are of the same ORDER as SWMDK so the workflow feels realistic.
#   It is not a model of SWMDK, does not reproduce its item structure, and its
#   coefficient should not be compared with the published one.
#
# Run from the package root:
#     Rscript data-raw/classroom_wb.R
# =============================================================================

stopifnot(requireNamespace("bayesmokken", quietly = TRUE))

SEED <- 20260810L

raw <- bayesmokken::bm_simulate(
  n = 600L,
  n_items = 6L,
  type = "ordinal",
  n_categories = 5L,
  discrimination = 1.5,
  clusters = 30L,
  icc = 0.17,
  informative_size = TRUE,      # the two targets must actually separate
  seed = SEED
)

classroom_wb <- data.frame(
  class_id = factor(sub("^c", "class", as.character(raw$cluster))),
  stats::setNames(raw[, paste0("item", 1:6)], paste0("wb", 1:6)),
  stringsAsFactors = FALSE
)
classroom_wb[paste0("wb", 1:6)] <- lapply(
  classroom_wb[paste0("wb", 1:6)], as.integer
)
rownames(classroom_wb) <- NULL

# --- checks ------------------------------------------------------------------
items <- paste0("wb", 1:6)
stopifnot(
  nlevels(classroom_wb$class_id) == 30L,
  all(vapply(classroom_wb[items], function(x) all(x %in% 1:5), logical(1))),
  !anyNA(classroom_wb),
  min(table(classroom_wb$class_id)) >= 2L
)

sup <- stats::setNames(rep(list(1:5), length(items)), items)
cl <- as.character(classroom_wb$class_id)
h <- vapply(c("respondent", "equal_cluster"), function(tg) {
  bayesmokken::mokken_h(
    classroom_wb[, items], item_type = "ordinal", supports = sup,
    cluster = cl, cluster_target = tg,
    weight_law = "hierarchical_cluster", warn = FALSE
  )$scale$H
}, numeric(1))

sizes <- as.integer(table(classroom_wb$class_id))
means <- tapply(rowSums(classroom_wb[, items]), classroom_wb$class_id, mean)
rho <- stats::cor(sizes, as.numeric(means), method = "spearman")

message(sprintf("n = %d, G = %d, sizes %d-%d",
                nrow(classroom_wb), nlevels(classroom_wb$class_id),
                min(sizes), max(sizes)))
message(sprintf("H respondent = %.4f   H equal-cluster = %.4f   diff = %+.4f",
                h[["respondent"]], h[["equal_cluster"]],
                h[["equal_cluster"]] - h[["respondent"]]))
message(sprintf("informative-size index (Spearman size vs cluster mean) = %.3f",
                rho))

# The whole point of this dataset is that the two targets are not the same
# number. If they ever coincide, the vignette that uses it stops teaching
# anything, so fail loudly here rather than shipping a silent dud.
if (abs(h[["equal_cluster"]] - h[["respondent"]]) < 0.002) {
  stop("The two cluster targets are too close; regenerate with a stronger ",
       "informative-size mechanism.")
}
if (rho < 0.4) {
  stop("Cluster size and cluster level are not related strongly enough.")
}

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(classroom_wb, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(classroom_wb, file = "data/classroom_wb.rda", compress = "xz",
       version = 3)
}
message("data/classroom_wb.rda written (",
        format(file.size("data/classroom_wb.rda"), big.mark = ","), " bytes)")
