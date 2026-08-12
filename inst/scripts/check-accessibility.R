# =============================================================================
# G-9  Accessibility
#
# This gate always exercises package plots, even before vignettes exist. It
# checks generated alternative text, redundant non-colour encodings, palette
# properties that can actually be verified, caption contrast, and a real
# 5-inch render. When vignettes are present it additionally scans their plot
# chunks for fig.alt and their stochastic chunks for explicit seeds.
#
#     Rscript inst/scripts/check-accessibility.R
# =============================================================================

cat("G-9 accessibility / G-6 seed scan\n")

failures <- character()
check <- function(ok, label, detail = NULL) {
  ok <- isTRUE(ok)
  cat(sprintf("  %-28s: %s%s\n", label, if (ok) "PASS" else "FAIL",
              if (is.null(detail)) "" else paste0(" (", detail, ")")))
  if (!ok) failures <<- c(failures, label)
  invisible(ok)
}

# Load the source tree so this gate never passes against a stale installed
# package. The installed-package fallback keeps the shipped script usable when
# it is run outside a source checkout.
load_error <- NULL
tryCatch(
  {
    if (requireNamespace("pkgload", quietly = TRUE) &&
        file.exists("DESCRIPTION") && dir.exists("R")) {
      suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
    } else if (requireNamespace("bayesmokken", quietly = TRUE)) {
      suppressPackageStartupMessages(
        library("bayesmokken", character.only = TRUE)
      )
    } else {
      stop("neither a source tree nor an installed bayesmokken package is available")
    }
  },
  error = function(e) load_error <<- conditionMessage(e)
)
if (!is.null(load_error)) {
  check(FALSE, "package source load", load_error)
  cat("  STATUS: FAIL\n")
  invisible(quit(status = 1L))
}
check(TRUE, "package source load")

ns <- asNamespace("bayesmokken")
house_colours <- get(".bm_colours", envir = ns)

# A small fit is enough to exercise every structural contract. Cluster target
# encoding is deterministic and therefore uses bm_cluster_info(), avoiding an
# unnecessary hierarchical bootstrap in this fast gate.
plot_error <- NULL
plots <- tryCatch(
  {
    old_options <- options(bayesmokken.figure_width = 5)
    on.exit(options(old_options), add = TRUE)
    fit <- bb_mokken_h(srs, draws = 120L, seed = 9102L, warn = FALSE)
    cluster_info <- bm_cluster_info(
      classroom_wb$class_id,
      data = classroom_wb[, paste0("wb", 1:6), drop = FALSE]
    )
    list(
      scale = bm_plot_scale_h(fit),
      draws = bm_plot_draws(fit),
      item = bm_plot_item_h(fit, show_interval = TRUE),
      pair = bm_plot_pair_h(fit, show_values = FALSE),
      calibration = bm_plot_calibration(fit),
      mc = bm_plot_mc_stability(fit),
      margins = bm_plot_margins(fit),
      clusters = bm_plot_clusters(cluster_info)
    )
  },
  error = function(e) {
    plot_error <<- conditionMessage(e)
    NULL
  }
)
if (is.null(plots)) {
  check(FALSE, "package plot construction", plot_error)
  cat("  STATUS: FAIL\n")
  invisible(quit(status = 1L))
}
check(TRUE, "package plot construction", sprintf("%d plots", length(plots)))

alts <- vapply(plots, bm_alt_text, character(1))
alt_ok <- !is.na(alts) & nzchar(trimws(alts)) & nchar(alts) >= 30L
check(all(alt_ok), "generated alternative text",
      sprintf("%d/%d nonempty", sum(alt_ok), length(alt_ok)))

mapping_names <- function(p) {
  unique(c(
    names(p$mapping),
    unlist(lapply(p$layers, function(layer) names(layer$mapping)),
           use.names = FALSE)
  ))
}
item_aes <- mapping_names(plots$item)
check(all(c("colour", "shape") %in% item_aes),
      "item redundant status", "colour + shape")

cluster_aes <- mapping_names(plots$clusters)
check(all(c("colour", "shape", "linetype") %in% cluster_aes),
      "cluster target encoding", "colour + shape + linetype")

draw_linetypes <- unique(unlist(lapply(plots$draws$layers, function(layer) {
  layer$aes_params$linetype
}), use.names = FALSE))
check(all(c("dashed", "solid", "dotdash") %in% draw_linetypes),
      "draw endpoint encoding", "dashed + solid + dotdash")

raw_item_layers <- Filter(function(layer) {
  identical(class(layer$geom)[[1L]], "GeomLinerange")
}, plots$item$layers)
raw_item_ok <- length(raw_item_layers) == 1L &&
  identical(raw_item_layers[[1L]]$aes_params$linetype, "dashed") &&
  identical(toupper(raw_item_layers[[1L]]$aes_params$colour),
            toupper(unname(house_colours[["ink"]]))) &&
  is.null(raw_item_layers[[1L]]$aes_params$alpha)
check(raw_item_ok, "raw item interval stroke", "opaque ink + dashed")

pair_built <- ggplot2::ggplot_build(plots$pair)
pair_labels <- unlist(lapply(pair_built$data, function(layer_data) {
  if ("label" %in% names(layer_data)) as.character(layer_data$label) else NULL
}), use.names = FALSE)
check(any(pair_labels %in% c("+", "\u2212", "-")),
      "pair sign redundancy", "+/- glyph present")

all_palette <- unlist(lapply(
  c("qualitative", "sequential", "diverging"),
  function(type) bm_palette(9L, type = type)
), use.names = FALSE)
check(all(grepl("^#[0-9A-Fa-f]{6}$", all_palette)), "palette hex values")

sequential_rgb <- t(grDevices::col2rgb(
  bm_palette(9L, type = "sequential")
)) / 255
sequential_l <- grDevices::convertColor(
  sequential_rgb, from = "sRGB", to = "Lab", scale.in = 1
)[, "L"]
lightness_step <- diff(sequential_l)
check(all(lightness_step > 0) || all(lightness_step < 0),
      "sequential lightness order",
      sprintf("L* %.1f to %.1f", sequential_l[[1L]], tail(sequential_l, 1L)))

relative_luminance <- function(colour) {
  rgb <- as.numeric(grDevices::col2rgb(colour)) / 255
  rgb <- ifelse(
    rgb <= 0.04045,
    rgb / 12.92,
    ((rgb + 0.055) / 1.055)^2.4
  )
  sum(c(0.2126, 0.7152, 0.0722) * rgb)
}
contrast_ratio <- function(foreground, background) {
  lum <- c(relative_luminance(foreground), relative_luminance(background))
  (max(lum) + 0.05) / (min(lum) + 0.05)
}
caption_contrast <- contrast_ratio("grey40", house_colours[["paper"]])
check(caption_contrast >= 4.5, "caption contrast",
      sprintf("%.2f:1 on white", caption_contrast))

# Render the longest representative default caption on a real 5-inch device,
# then measure each already-wrapped line against the plot's usable width.
render_file <- tempfile(fileext = ".png")
render_error <- NULL
caption_widths <- numeric()
available_width <- NA_real_
tryCatch(
  {
    grDevices::png(render_file, width = 5, height = 4.8, units = "in",
                   res = 130, bg = "white")
    print(plots$item)

    caption_element <- ggplot2::calc_element("plot.caption", plots$item$theme)
    caption_lines <- strsplit(plots$item$labels$caption, "\n", fixed = TRUE)[[1L]]
    text_gp <- grid::gpar(
      fontsize = caption_element$size,
      fontfamily = caption_element$family,
      fontface = caption_element$face
    )
    caption_widths <- vapply(caption_lines, function(line) {
      grid::convertWidth(
        grid::grobWidth(grid::textGrob(line, gp = text_gp)),
        "in", valueOnly = TRUE
      )
    }, numeric(1))
    plot_margin <- ggplot2::calc_element("plot.margin", plots$item$theme)
    side_margins <- grid::convertWidth(
      plot_margin[c(4L, 2L)], "in", valueOnly = TRUE
    )
    available_width <- 5 - sum(side_margins)
    invisible(grDevices::dev.off())
  },
  error = function(e) {
    render_error <<- conditionMessage(e)
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
  }
)
render_ok <- is.null(render_error) && file.exists(render_file) &&
  file.info(render_file)$size > 0
check(render_ok, "5-inch package render",
      if (render_ok) sprintf("%d bytes", file.info(render_file)$size) else render_error)
caption_fit <- render_ok && length(caption_widths) > 0L &&
  is.finite(available_width) && max(caption_widths) <= available_width + 0.02
check(caption_fit, "5-inch caption fit",
      if (length(caption_widths)) {
        sprintf("max %.2f in / available %.2f in",
                max(caption_widths), available_width)
      } else {
        "caption could not be measured"
      })
unlink(render_file)

# Vignette checks are additive. Their absence is reported, but can no longer
# turn this gate into a vacuous pass because the package checks above ran.
vignettes <- list.files("vignettes", pattern = "\\.(Rmd|qmd)$",
                        full.names = TRUE)
chunk_open <- "^\\s*```\\{r[ ,}]"
plot_calls <- "ggplot\\(|autoplot\\(|bm_plot_|plot\\(|print\\(p"
rng_calls <- "bb_mokken_h\\(|bm_simulate\\(|sample\\(|r(norm|unif|binom|exp|multinom)\\("

rows <- list()
for (f in vignettes) {
  ln <- readLines(f, warn = FALSE)
  opens <- grep(chunk_open, ln)
  closes <- grep("^\\s*```\\s*$", ln)
  for (o in opens) {
    e <- closes[closes > o]
    if (!length(e)) next
    body <- paste(ln[(o + 1L):(e[[1L]] - 1L)], collapse = "\n")
    header <- ln[[o]]
    # A chunk that is never evaluated produces no figure, so it cannot be
    # missing alternative text for one. Demanding fig.alt there would push
    # authors to attach a description to an image that does not exist.
    if (grepl("eval\\s*=\\s*FALSE", header)) next
    draws_plot <- grepl(plot_calls, body)
    uses_rng <- grepl(rng_calls, body)
    has_alt <- grepl("fig\\.alt\\s*=", header) || grepl("fig-alt:", body)
    has_seed <- grepl("seed\\s*=", body) || grepl("set\\.seed\\(", body)
    if (draws_plot || uses_rng) {
      rows[[length(rows) + 1L]] <- data.frame(
        file = basename(f), line = o,
        needs_alt = draws_plot, has_alt = has_alt,
        needs_seed = uses_rng, has_seed = has_seed,
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(rows)) {
  vignette_res <- do.call(rbind, rows)
  missing_alt <- vignette_res[
    vignette_res$needs_alt & !vignette_res$has_alt, , drop = FALSE
  ]
  missing_seed <- vignette_res[
    vignette_res$needs_seed & !vignette_res$has_seed, , drop = FALSE
  ]
} else {
  vignette_res <- data.frame()
  missing_alt <- data.frame()
  missing_seed <- data.frame()
}
cat("  vignette files             : ", length(vignettes), "\n", sep = "")
cat("  vignette chunks inspected  : ", nrow(vignette_res), "\n", sep = "")
cat("  missing vignette fig.alt   : ", nrow(missing_alt), "\n", sep = "")
cat("  missing vignette seed      : ", nrow(missing_seed), "\n", sep = "")
if (nrow(missing_alt)) {
  print(missing_alt[, c("file", "line")], row.names = FALSE)
  failures <- c(failures, "vignette fig.alt")
}
if (nrow(missing_seed)) {
  print(missing_seed[, c("file", "line")], row.names = FALSE)
  failures <- c(failures, "vignette seed")
}

ok <- !length(failures)
cat("  STATUS: ", if (ok) "PASS" else "FAIL", "\n", sep = "")
if (!ok) invisible(quit(status = 1L))
