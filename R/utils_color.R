# ==============================================================================
# utils_color.R
# Core utilities: input sanitization, Oklab encoding/decoding
# Single source of truth — do NOT redefine these functions elsewhere.
# ==============================================================================

#' Sanitize and normalize a color input vector (La Aduana)
#'
#' Accepts HEX strings, R color names, or a sysdata palette key (single string).
#' Strips alpha, deduplicates, and returns a clean #RRGGBB vector.
#'
#' @param x Character vector of HEX codes, R color names, or one sysdata key.
#' @return Character vector of unique, valid #RRGGBB codes.
#' @importFrom grDevices col2rgb rgb
#' @export
get_input_vector <- function(x) {
  x <- as.character(x)

  # ── 1. Sysdata lookup (single-key shortcut) ─────────────────────────────────
  if (length(x) == 1L) {
    env <- new.env(parent = emptyenv())
    tryCatch(
      load(system.file("R/sysdata.rda", package = "loscolores"), envir = env),
      error = function(e) NULL
    )
    if (exists("paletas", envir = env) && x %in% names(env$paletas)) {
      return(env$paletas[[x]]$hexcolors)
    }
  }

  # ── 2. Drop NAs ─────────────────────────────────────────────────────────────
  x <- x[!is.na(x)]
  if (length(x) == 0L)
    stop("Colapso de señal: el input está vacío o compuesto sólo por NAs.",
         call. = FALSE)

  # ── 3. Resolve R color names → HEX ──────────────────────────────────────────
  is_hex <- grepl("^#[0-9A-Fa-f]{6}$|^#[0-9A-Fa-f]{8}$", x)
  if (!all(is_hex)) {
    non_hex <- which(!is_hex)
    for (i in non_hex) {
      tryCatch({
        rgb_val <- grDevices::col2rgb(x[i])
        x[i] <- grDevices::rgb(rgb_val[1L], rgb_val[2L], rgb_val[3L],
                               maxColorValue = 255)
      }, error = function(e) { x[i] <<- NA_character_ })
    }
  }

  # ── 4. Final purge ───────────────────────────────────────────────────────────
  # Strip alpha channel if present (#RRGGBBAA → #RRGGBB)
  valid <- x[!is.na(x) & grepl("^#[0-9A-Fa-f]{6,8}$", x)]
  valid <- substr(valid, 1L, 7L)
  valid <- unique(valid)

  if (length(valid) == 0L)
    stop("Colapso de señal: ningún color válido sobrevivió la aduana.",
         call. = FALSE)

  valid
}


#' Convert HEX vector to Oklab data frame
#'
#' @param hex_vector Character vector of #RRGGBB codes.
#' @return Data frame with columns L, a, b (Oklab) plus c (chroma) and h (hue°).
#' @importFrom farver decode_colour
#' @export
to_oklab <- function(hex_vector) {
  mat <- farver::decode_colour(hex_vector, to = "oklab")
  df  <- data.frame(L = mat[, 1L], a = mat[, 2L], b = mat[, 3L])
  df$c <- sqrt(df$a^2 + df$b^2)
  df$h <- (atan2(df$b, df$a) * 180 / pi) %% 360
  df
}


#' Convert Oklab coordinates to HEX with gamut clipping
#'
#' @param L Numeric vector. Oklab lightness [0, 1].
#' @param a Numeric vector. Oklab green-red axis.
#' @param b Numeric vector. Oklab blue-yellow axis.
#' @return Character vector of #RRGGBB codes.
#' @importFrom farver encode_colour
#' @export
to_hex <- function(L, a, b) {
  L <- pmax(0, pmin(1, L))   # hard-clamp before handing to farver
  farver::encode_colour(cbind(L, a, b), from = "oklab")
}