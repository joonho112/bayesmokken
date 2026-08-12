# =============================================================================
# Visual grammar
#
# One theme, one palette, one set of scales, shared by all eight figures so
# they read as a family. Everything is built on grDevices::hcl.colors(), so
# there is no extra dependency and the ramps are perceptually uniform.
#
# Three rules the whole figure layer obeys:
#   1. categorical and signed distinctions are repeated with shape, line type,
#      glyph, stroke, or a direct label; the palette alone is not treated as a
#      guarantee of greyscale distinguishability
#   2. captions and alt text are generated from the same object as the data,
#      never typed a second time
#   3. every function returns a ggplot and renders nothing itself
# =============================================================================

#' bayesmokken plotting internals
#'
#' Import anchor for the figure layer. The plotting code uses many ggplot2
#' verbs, so the whole namespace is imported; data-frame columns are reached
#' through the `rlang` `.data` pronoun inside `aes()` to avoid the R CMD check
#' note about undefined global variables.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @keywords internal
#' @name bayesmokken-plots
NULL


# The house colours. `primary` is also the accent of the package logo, so the
# figures, the website and the sticker share one hue.
.bm_colours <- c(
  primary   = "#1F6F70",   # deep teal: point estimates, calibrated intervals
  secondary = "#7FB3B4",   # muted teal: raw intervals, secondary series
  accent    = "#C6742B",   # amber: the thing to look at
  warn      = "#B4552D",   # rust: near-zero denominators
  muted     = "#8C8C8C",   # grey: zero denominators, reference rules
  ink       = "#2B2B2B",   # near-black: text
  paper     = "#FFFFFF"
)


#' The bayesmokken color palette
#'
#' Returns colors for the three kinds of scale the package's figures use.
#' The sequential ramp has ordered lightness. The qualitative and diverging
#' palettes are intended for screen use, but their hues are not guaranteed to
#' remain distinct when converted to grayscale. Package plots therefore repeat
#' categorical and signed distinctions with shape, line type, glyphs, stroke,
#' direct labels, or ordering rather than relying on color alone.
#'
#' @param n Number of colors. For `"qualitative"` the default is the full set
#'   of five house colors; for the continuous types it is 256.
#' @param type `"qualitative"` for unordered categories, `"sequential"` for a
#'   magnitude that runs from low to high, `"diverging"` for a quantity with a
#'   meaningful zero, such as a pair coefficient that can be negative.
#'
#' @return A character vector of hex colors.
#'
#' @examples
#' bm_palette(3)
#' bm_palette(5, type = "diverging")
#'
#' @family bayesmokken-plots
#' @seealso [theme_bayesmokken()], [scale_colour_bayesmokken()]
#' @export
bm_palette <- function(n = NULL, type = c("qualitative", "sequential",
                                          "diverging")) {
  type <- match.arg(type)
  if (identical(type, "qualitative")) {
    base <- unname(.bm_colours[c("primary", "accent", "secondary", "warn",
                                 "muted")])
    if (is.null(n)) return(base)
    if (n <= length(base)) return(base[seq_len(n)])
    return(grDevices::hcl.colors(n, "Dark 3"))
  }
  if (is.null(n)) n <- 256L
  if (identical(type, "sequential")) {
    grDevices::hcl.colors(n, "Teal", rev = TRUE)
  } else {
    grDevices::hcl.colors(n, "Blue-Red 3")
  }
}


#' The bayesmokken theme
#'
#' A quiet theme: no panel border, horizontal grid lines only, titles aligned
#' to the plot rather than the panel, and a small left-aligned caption, because
#' every figure in this package carries its method identifier and its
#' interpretation there. Default package captions wrap to the current graphics
#' device width. Code that constructs a plot before opening its output device
#' can set `options(bayesmokken.figure_width = 5)` (in inches); when neither an
#' option nor an active device supplies a width, wrapping uses a conservative
#' 5-inch fallback.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family. The default lets the device decide,
#'   which keeps figures reproducible across machines.
#'
#' @return A ggplot2 theme object, to add to a plot with `+`.
#'
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = 1:5, y = (1:5)^2), aes(x, y)) +
#'   geom_line(color = bm_palette(1)) +
#'   labs(title = "A quiet theme", caption = "Captions sit bottom left.") +
#'   theme_bayesmokken()
#'
#' @family bayesmokken-plots
#' @seealso [bm_palette()], [scale_colour_bayesmokken()]
#' @export
theme_bayesmokken <- function(base_size = 11, base_family = "") {
  grid_col <- "grey92"
  ink <- unname(.bm_colours[["ink"]])
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.15), face = "bold",
                                colour = ink, margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.92), colour = "grey35",
                                   margin = margin(b = 10)),
      plot.caption = element_text(size = rel(0.76), colour = "grey40",
                                  hjust = 0, margin = margin(t = 10)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(12, 14, 10, 12),
      axis.title = element_text(size = rel(0.92), colour = "grey30"),
      axis.title.x = element_text(margin = margin(t = 7)),
      axis.title.y = element_text(margin = margin(r = 7)),
      axis.text = element_text(size = rel(0.88), colour = "grey25"),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.35),
      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.88), colour = "grey30"),
      legend.text = element_text(size = rel(0.85)),
      legend.key = element_blank(),
      legend.margin = margin(t = 2),
      strip.text = element_text(size = rel(0.92), face = "bold", colour = ink,
                                hjust = 0, margin = margin(b = 5))
    )
}


#' bayesmokken color and fill scales
#'
#' Discrete and continuous scales drawn from [bm_palette()]. Add one to a plot
#' whose color or fill aesthetic should follow the package's palette.
#'
#' @param type `"qualitative"`, `"sequential"` or `"diverging"`.
#' @param ... Passed to the underlying ggplot2 scale constructor, so `name`,
#'   `labels`, `guide` and friends all work as usual.
#'
#' @return A ggplot2 scale, to add to a plot with `+`.
#'
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = 1:3, y = 1:3, g = factor(c("a", "b", "c")))
#' ggplot(d, aes(x, y, color = g)) +
#'   geom_point(size = 4) +
#'   scale_colour_bayesmokken() +
#'   theme_bayesmokken()
#'
#' @family bayesmokken-plots
#' @seealso [bm_palette()], [theme_bayesmokken()]
#' @export
scale_colour_bayesmokken <- function(type = c("qualitative", "sequential",
                                              "diverging"), ...) {
  type <- match.arg(type)
  if (identical(type, "qualitative")) {
    discrete_scale("colour", palette = function(n) bm_palette(n), ...)
  } else {
    continuous_scale(
      "colour",
      palette = function(x) {
        grDevices::colorRamp(bm_palette(type = type))(x) |>
          apply(1L, function(rgb) grDevices::rgb(rgb[1], rgb[2], rgb[3],
                                                 maxColorValue = 255))
      },
      ...
    )
  }
}

#' @rdname scale_colour_bayesmokken
#' @export
scale_color_bayesmokken <- scale_colour_bayesmokken

#' @rdname scale_colour_bayesmokken
#' @export
scale_fill_bayesmokken <- function(type = c("qualitative", "sequential",
                                            "diverging"), ...) {
  type <- match.arg(type)
  if (identical(type, "qualitative")) {
    discrete_scale("fill", palette = function(n) bm_palette(n), ...)
  } else {
    scale_fill_gradientn(colours = bm_palette(type = type), ...)
  }
}


# --- shared figure machinery --------------------------------------------------

# Denominator status is encoded three ways at once: colour, point shape, and a
# legend label. Remove the colour and the distinction survives.
.bm_status_levels <- c("positive", "near_zero", "zero")
.bm_status_shape <- c(positive = 16L, near_zero = 21L, zero = 4L)
.bm_status_colour <- c(
  positive = unname(.bm_colours[["primary"]]),
  near_zero = unname(.bm_colours[["warn"]]),
  zero = unname(.bm_colours[["muted"]])
)

.bm_status_factor <- function(x) {
  factor(x, levels = .bm_status_levels)
}

# Captions are assembled here and nowhere else, so a figure cannot describe
# itself differently from the way the package describes itself.
.bm_caption_wrap_width <- function(width = NULL, device_width = NULL) {
  # `width` is the historical character-count override used by internal
  # callers. New code supplies a device width in inches, either directly or
  # through the option below, because ggplot objects are commonly constructed
  # before ggsave() opens its device.
  if (is.numeric(width) && length(width) == 1L && is.finite(width) &&
      width > 0) {
    return(as.integer(width))
  }

  if (is.null(device_width)) {
    device_width <- getOption("bayesmokken.figure_width", NULL)
  }
  if (is.null(device_width) && grDevices::dev.cur() > 1L) {
    device_width <- tryCatch(
      unname(grDevices::dev.size("in")[[1L]]),
      error = function(e) NULL
    )
  }
  if (!is.numeric(device_width) || length(device_width) != 1L ||
      !is.finite(device_width) ||
      device_width <= 0) {
    device_width <- 5
  }

  # Reserve space for plot margins and use a deliberately conservative average
  # character density. The cap preserves the established wide-device layout.
  as.integer(max(48L, min(105L, floor((device_width - 0.7) * 14))))
}

.bm_caption <- function(user_caption, ..., uncalibrated = FALSE,
                        width = NULL) {
  if (!is.null(user_caption)) {
    return(if (identical(user_caption, NA)) NULL else user_caption)
  }
  parts <- c(...)
  if (isTRUE(uncalibrated)) parts <- c(parts, .bm_uncalibrated_note)
  if (!length(parts)) return(NULL)
  # ggplot2 does not wrap captions, and every figure here carries a method
  # identifier plus an interpretation sentence, so an unwrapped caption runs
  # off the right edge of the device.
  paste(
    strwrap(
      paste(parts, collapse = " "),
      width = .bm_caption_wrap_width(width = width)
    ),
    collapse = "\n"
  )
}

.bm_method_caption <- function(x) {
  itv <- .bm_parts(x)$interval
  if (is.null(itv)) return("Deterministic fit; no interval computed.")
  paste0("Method ", itv$method_id, ". ", .bm_interpretation_note)
}

# Alt text, generated from the same numbers the figure draws.
.bm_alt <- function(...) paste(c(...), collapse = " ")
