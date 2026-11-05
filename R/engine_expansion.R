# ==============================================================================
# engine_expansion.R
# One core, many uses: categorical, sequential and divergent palettes.
# Mirrors the expansion functions in web/public/engine.js.
# ==============================================================================

LC_CAT_ORDER <- list(A = c(1, 2, 3, 4, 5), B = c(1, 4, 2, 5, 3),
                     C = c(1, 5, 2, 4, 3), D = c(1, 2, 3, 4, 5))
LC_RAMPS <- c("A", "B", "C", "D", "E")

lc_lerp <- function(p, q, t) {
  lc_polar(p[["L"]] + (q[["L"]] - p[["L"]]) * t,
           p[["a"]] + (q[["a"]] - p[["a"]]) * t,
           p[["b"]] + (q[["b"]] - p[["b"]]) * t)
}

lc_option <- function(option, allowed) {
  o <- sub("^Option_", "", option)
  if (!o %in% allowed)
    stop(sprintf("Option '%s' not available. Use one of: %s.", option,
                 paste0("Option_", allowed, collapse = ", ")), call. = FALSE)
  o
}

lc_out <- function(cols, grayscale) {
  if (grayscale) cols <- lapply(cols, function(p) lc_grey(p[["L"]]))
  vapply(cols, lc_lab_to_hex, "")
}

lc_categorical <- function(sys, option, n, grayscale) {
  o <- lc_option(option, names(LC_CAT_ORDER))
  nodes <- sys$lab$raw5[LC_CAT_ORDER[[o]]]
  if (o == "D") nodes <- lapply(seq_along(nodes), function(i) {
    p <- nodes[[i]]
    if (i == 1L || p[["c"]] < 0.01) p else lc_from_lch(p[["L"]], max(p[["c"]], 0.15), (p[["h"]] + 15) %% 360)
  })
  cols <- if (n <= 5) nodes[seq_len(n)] else lapply(seq_len(n) - 1L, function(i) {
    t <- (i / (n - 1)) * 4
    k <- min(3, floor(t))
    lc_lerp(nodes[[k + 1L]], nodes[[k + 2L]], t - k)
  })
  hex <- lc_out(cols, grayscale)
  if (!grayscale && o != "D" && n > 0) hex[1L] <- sys$raw_5[1L]   # the anchor stays exact
  hex
}

lc_sequential <- function(sys, option, n, grayscale) {
  base <- sys$lab$raw5[[match(lc_option(option, LC_RAMPS), LC_RAMPS)]]
  end  <- lc_polar(sys$lab$white[["L"]] * 0.97, base[["a"]] * 0.04, base[["b"]] * 0.04)
  lc_out(lapply(seq_len(n) - 1L, function(i) lc_lerp(base, end, i / (n - 1))), grayscale)
}

# The base and its complement at the same L and chroma, so both arms weigh the
# same in greyscale; the centre is a pole.
lc_divergent <- function(sys, option, n, center, grayscale) {
  high <- sys$lab$raw5[[match(lc_option(option, LC_RAMPS), LC_RAMPS)]]
  low  <- lc_map_gamut(lc_from_lch(high[["L"]], high[["c"]], (high[["h"]] + 180) %% 360))$col
  mid  <- if (center == "black") sys$lab$black else sys$lab$white
  cols <- lapply(seq_len(n) - 1L, function(i) {
    t <- i / (n - 1)
    if (t <= 0.5) lc_lerp(low, mid, t * 2) else lc_lerp(mid, high, (t - 0.5) * 2)
  })
  lc_out(cols, grayscale)
}


#' Sample a palette from an lc_system object
#'
#' @param sys       An \code{lc_system} object.
#' @param type      One of \code{"categorical"}, \code{"sequential"},
#'                  \code{"divergent"}.
#' @param option    Variant key: \code{"Option_A"} through \code{"Option_D"}
#'                  (categorical) or \code{"Option_A"} through \code{"Option_E"}
#'                  (sequential/divergent, one per core colour). \code{"A"} also works.
#' @param n         Number of output colors.
#' @param center    \code{"white"} or \code{"black"} (divergent only).
#' @param grayscale Logical. Strip chroma from output.
#' @return Character vector of \code{n} #RRGGBB codes.
#' @export
sample_system <- function(sys,
                          type      = "categorical",
                          option    = "Option_A",
                          n         = 10L,
                          center    = "white",
                          grayscale = FALSE) {
  n <- as.integer(n)
  switch(type,
    categorical = lc_categorical(sys, option, n, grayscale),
    sequential  = lc_sequential(sys, option, n, grayscale),
    divergent   = lc_divergent(sys, option, n, center, grayscale),
    stop("type must be 'categorical', 'sequential', or 'divergent'.", call. = FALSE)
  )
}
