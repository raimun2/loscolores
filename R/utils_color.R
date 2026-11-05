# ==============================================================================
# utils_color.R
# Input customs and OKLab maths. Mirrors web/public/engine.js operation by
# operation (same constants, same order of arithmetic) so both engines return
# the same hex codes. Single source of truth: do NOT redefine these elsewhere.
# ==============================================================================

#' Sanitize and normalize a color input vector (the customs check)
#'
#' Accepts a single palette key from internal sysdata (\code{names(paletas)}),
#' HEX strings (#RGB, #RRGGBB, #RRGGBBAA), or R color names. Strips alpha,
#' upper-cases, deduplicates, and returns a clean #RRGGBB vector.
#'
#' @param x Character vector of HEX codes / R color names, or one sysdata key.
#' @return Character vector of unique, valid #RRGGBB codes.
#' @importFrom grDevices col2rgb rgb
#' @export
get_input_vector <- function(x) {
  x <- as.character(x)

  if (length(x) == 1L && exists("paletas", inherits = TRUE) &&
      x %in% names(paletas)) {
    x <- paletas[[x]]$hexcolors
  }

  x <- x[!is.na(x)]
  if (length(x) == 0L)
    stop("Signal collapse: input is empty or contains only NAs.", call. = FALSE)

  out <- vapply(x, function(s) {
    h <- sub("^#", "", trimws(s))
    if (grepl("^[0-9A-Fa-f]{3}$", h))
      h <- paste(rep(strsplit(h, "")[[1L]], each = 2L), collapse = "")
    if (grepl("^[0-9A-Fa-f]{8}$", h)) h <- substr(h, 1L, 6L)
    if (grepl("^[0-9A-Fa-f]{6}$", h)) return(toupper(paste0("#", h)))
    tryCatch({
      v <- grDevices::col2rgb(s)
      toupper(grDevices::rgb(v[1L], v[2L], v[3L], maxColorValue = 255))
    }, error = function(e) NA_character_)
  }, character(1L), USE.NAMES = FALSE)

  valid <- unique(out[!is.na(out)])
  if (length(valid) == 0L)
    stop("Signal collapse: no valid color survived customs.", call. = FALSE)
  valid
}

# -- sRGB <-> OKLab (Bjorn Ottosson) --------------------------------------------

lc_to_lin <- function(v) ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
lc_to_gam <- function(v) ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)

lc_hex_to_rgb <- function(hex) {
  h <- sub("#", "", hex)
  c(strtoi(substr(h, 1, 2), 16L), strtoi(substr(h, 3, 4), 16L),
    strtoi(substr(h, 5, 6), 16L)) / 255
}

lc_rgb_to_hex <- function(rgb) {
  v <- floor(pmin(1, pmax(0, rgb)) * 255 + 0.5)   # JS Math.round, not banker's rounding
  toupper(sprintf("#%02X%02X%02X", v[1L], v[2L], v[3L]))
}

# A colour is a named numeric: L, a, b, c (chroma), h (hue, degrees)
lc_polar <- function(L, a, b) {
  C <- sqrt(a * a + b * b)
  h <- (atan2(b, a) * 180 / pi + 360) %% 360
  c(L = L, a = a, b = b, c = C, h = h)
}

lc_from_lch <- function(L, C, h) {
  lc_polar(L, C * cos(h * pi / 180), C * sin(h * pi / 180))
}

lc_grey <- function(L) c(L = L, a = 0, b = 0, c = 0, h = 0)

lc_hex_to_lab <- function(hex) {
  rgb <- lc_to_lin(lc_hex_to_rgb(hex))
  R <- rgb[1L]; G <- rgb[2L]; B <- rgb[3L]
  l <- (0.4122214708 * R + 0.5363325363 * G + 0.0514459929 * B)^(1 / 3)
  m <- (0.2119034982 * R + 0.6806995451 * G + 0.1073969566 * B)^(1 / 3)
  s <- (0.0883024619 * R + 0.2817188376 * G + 0.6299787005 * B)^(1 / 3)
  lc_polar(
    0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
    1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
    0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s
  )
}

lc_lab_to_linear <- function(L, a, b) {
  l <- (L + 0.3963377774 * a + 0.2158037573 * b)^3
  m <- (L - 0.1055613458 * a - 0.0638541728 * b)^3
  s <- (L - 0.0894841775 * a - 1.291485548 * b)^3
  c(4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s)
}

lc_in_gamut <- function(lin, eps = 1e-6) all(lin >= -eps & lin <= 1 + eps)

# Gamut mapping that keeps L and hue: shrink chroma until the colour fits sRGB.
lc_map_gamut <- function(col) {
  L <- min(1, max(0, col[["L"]]))
  if (lc_in_gamut(lc_lab_to_linear(L, col[["a"]], col[["b"]])))
    return(list(col = lc_polar(L, col[["a"]], col[["b"]]), clipped = FALSE))
  lo <- 0; hi <- col[["c"]]
  for (i in seq_len(30L)) {
    mid <- (lo + hi) / 2
    t <- lc_from_lch(L, mid, col[["h"]])
    if (lc_in_gamut(lc_lab_to_linear(t[["L"]], t[["a"]], t[["b"]]))) lo <- mid else hi <- mid
  }
  list(col = lc_from_lch(L, lo, col[["h"]]), clipped = TRUE)
}

lc_lab_to_hex <- function(col) {
  g <- lc_map_gamut(col)$col
  lc_rgb_to_hex(lc_to_gam(lc_lab_to_linear(g[["L"]], g[["a"]], g[["b"]])))
}


#' Convert HEX vector to OKLab data frame
#'
#' @param hex_vector Character vector of #RRGGBB codes.
#' @return Data frame with columns L, a, b (OKLab) plus c (chroma) and h (hue in degrees).
#' @export
to_oklab <- function(hex_vector) {
  m <- t(vapply(hex_vector, lc_hex_to_lab, numeric(5L)))
  data.frame(L = m[, "L"], a = m[, "a"], b = m[, "b"], c = m[, "c"], h = m[, "h"],
             row.names = NULL)
}


#' Convert OKLab coordinates to HEX with gamut mapping
#'
#' Out-of-gamut colours keep their lightness and hue; only chroma is reduced
#' until they fit sRGB (luminance is the backbone of the system).
#'
#' @param L Numeric vector. OKLab lightness in `[0, 1]`.
#' @param a Numeric vector. OKLab green-red axis.
#' @param b Numeric vector. OKLab blue-yellow axis.
#' @return Character vector of #RRGGBB codes.
#' @export
to_hex <- function(L, a, b) {
  n <- max(length(L), length(a), length(b))
  L <- rep_len(L, n); a <- rep_len(a, n); b <- rep_len(b, n)
  vapply(seq_len(n), function(i) lc_lab_to_hex(lc_polar(L[i], a[i], b[i])), character(1L))
}
