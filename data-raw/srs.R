# =============================================================================
# data-raw/srs.R  --  build data/srs.rda
#
# Source
#   Caliciuri, R., & Lanz, M. (2024). Psychometric properties of the Scientific
#   Reasoning Scale: Application to the Italian context. OSF project jk9dp.
#   https://osf.io/jk9dp/  doi:10.17605/OSF.IO/JK9DP
#   Licence: CC BY 4.0  (redistribution permitted with attribution)
#
# The instrument itself is the 11-item Scientific Reasoning Scale of
# Drummond & Fischhoff (2017), doi:10.1002/bdm.1906.
#
# Path taken here
#   The long-format table distributed through the Item Response Warehouse is
#   read, checked against the sha256 recorded in the article's source lock, and
#   pivoted to one row per respondent. Nothing else is changed: no items are
#   dropped, no scores are reversed, no respondents are filtered. The 337 rows
#   are the complete-case analysis file as published.
#
# Run from the package root:
#     Rscript data-raw/srs.R
# =============================================================================

# The source file lives outside this package -- it is the archived copy of the
# authors' data release, not something redistributed here. Point at your own
# copy; the SHA-256 below is what makes any copy verifiable.
SRS_RDS <- Sys.getenv("BAYESMOKKEN_SRS_RDS", "")
SRS_SHA256 <- "72ca8785e01bcc78f26ff616f7a2249161d6072676cd65e0d0eb96ba84def6fc"

if (!nzchar(SRS_RDS)) {
  stop("Set BAYESMOKKEN_SRS_RDS to the ppsrs_caliciuri_2024 .rds file.\n",
       "  Its SHA-256 must be ", SRS_SHA256, ".\n",
       "  See ?srs and inst/extdata/DATA-LICENSES.md for provenance.",
       call. = FALSE)
}

# Fixed item order, from the article's analysis contract
# (research/v3/empirical-ppsrs/protocol/ppsrs-empirical-analysis-contract-v1.yml).
# Item selection, deletion, reversal and reselection are all forbidden there,
# so this vector is a contract and not a preference.
SRS_ITEMS <- paste0("SRS", 1:11)

stopifnot(file.exists(SRS_RDS))
observed <- digest::digest(file = SRS_RDS, algo = "sha256")
if (!identical(observed, SRS_SHA256)) {
  stop("SRS source sha256 mismatch.\n  expected: ", SRS_SHA256,
       "\n  observed: ", observed)
}

long <- readRDS(SRS_RDS)
stopifnot(identical(sort(names(long)), c("id", "item", "resp")),
          nrow(long) == 3707L,
          length(unique(long$id)) == 337L,
          setequal(unique(as.character(long$item)), SRS_ITEMS),
          all(long$resp %in% c(0, 1)))

wide <- stats::reshape(
  long[, c("id", "item", "resp")],
  idvar = "id", timevar = "item", direction = "wide"
)
names(wide) <- sub("^resp\\.", "", names(wide))
wide <- wide[order(as.integer(as.character(wide$id))), , drop = FALSE]

srs <- wide[, SRS_ITEMS, drop = FALSE]
srs[] <- lapply(srs, as.integer)
rownames(srs) <- as.character(wide$id)

# --- verification against the published analysis -----------------------------
stopifnot(dim(srs) == c(337L, 11L),
          !anyNA(srs),
          all(vapply(srs, function(x) all(x %in% 0:1), logical(1))))

if (requireNamespace("bayesmokken", quietly = TRUE)) {
  h <- bayesmokken::mokken_h(srs, warn = FALSE)$scale$H
  published <- 0.130205929962575
  if (abs(h - published) > 1e-12) {
    stop("Built srs does not reproduce the published H.\n  got: ", h,
         "\n  expected: ", published)
  }
  message("scale H reproduces the published value: ", format(h, digits = 15))
}

usethis_available <- requireNamespace("usethis", quietly = TRUE)
if (usethis_available) {
  usethis::use_data(srs, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(srs, file = "data/srs.rda", compress = "xz", version = 3)
}
message("data/srs.rda written: ", nrow(srs), " x ", ncol(srs), " (",
        format(file.size("data/srs.rda"), big.mark = ","), " bytes)")
