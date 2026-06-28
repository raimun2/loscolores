# ==============================================================================
# scales.R
# ggplot2 scale integration: categorical, sequential, divergent
# ==============================================================================

# ── Categorical ───────────────────────────────────────────────────────────────

#' Loscolores categorical fill scale
#' @param option   Variant key ("Option_A" through "Option_D").
#' @param sys      Active \code{lc_system}. Defaults to session system.
#' @param grayscale Logical. Strip chroma.
#' @param ...      Passed to \code{ggplot2::discrete_scale}.
#' @import ggplot2
#' @export
scale_fill_loscolores_cat <- function(option    = "Option_A",
                                      sys       = getOption("loscolores.default_system"),
                                      grayscale = FALSE,
                                      ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::discrete_scale(
    "fill", "loscolores_cat",
    palette = function(n) sample_system(sys, "categorical", option, n,
                                        grayscale = grayscale),
    ...
  )
}

#' Loscolores categorical colour scale
#' @rdname scale_fill_loscolores_cat
#' @export
scale_color_loscolores_cat <- function(option    = "Option_A",
                                       sys       = getOption("loscolores.default_system"),
                                       grayscale = FALSE,
                                       ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::discrete_scale(
    "colour", "loscolores_cat",
    palette = function(n) sample_system(sys, "categorical", option, n,
                                        grayscale = grayscale),
    ...
  )
}

#' @rdname scale_fill_loscolores_cat
#' @export
scale_colour_loscolores_cat <- scale_color_loscolores_cat


# ── Sequential ────────────────────────────────────────────────────────────────

#' Loscolores sequential fill scale
#' @param option   Variant key ("Option_A" through "Option_E").
#' @param sys      Active \code{lc_system}. Defaults to session system.
#' @param grayscale Logical. Strip chroma.
#' @param ...      Passed to \code{ggplot2::continuous_scale}.
#' @import ggplot2
#' @importFrom scales gradient_n_pal
#' @export
scale_fill_loscolores_seq <- function(option    = "Option_A",
                                      sys       = getOption("loscolores.default_system"),
                                      grayscale = FALSE,
                                      ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::continuous_scale(
    "fill", "loscolores_seq",
    palette = scales::gradient_n_pal(
      sample_system(sys, "sequential", option, n = 100L, grayscale = grayscale)
    ),
    ...
  )
}

#' Loscolores sequential colour scale
#' @rdname scale_fill_loscolores_seq
#' @export
scale_color_loscolores_seq <- function(option    = "Option_A",
                                       sys       = getOption("loscolores.default_system"),
                                       grayscale = FALSE,
                                       ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::continuous_scale(
    "colour", "loscolores_seq",
    palette = scales::gradient_n_pal(
      sample_system(sys, "sequential", option, n = 100L, grayscale = grayscale)
    ),
    ...
  )
}

#' @rdname scale_fill_loscolores_seq
#' @export
scale_colour_loscolores_seq <- scale_color_loscolores_seq


# ── Divergent ─────────────────────────────────────────────────────────────────

#' Loscolores divergent fill scale
#' @param option   Variant key ("Option_A" through "Option_E").
#' @param center   \code{"white"} or \code{"black"} midpoint pole.
#' @param sys      Active \code{lc_system}. Defaults to session system.
#' @param grayscale Logical. Strip chroma.
#' @param ...      Passed to \code{ggplot2::continuous_scale}.
#' @import ggplot2
#' @importFrom scales gradient_n_pal
#' @export
scale_fill_loscolores_div <- function(option    = "Option_A",
                                      center    = "white",
                                      sys       = getOption("loscolores.default_system"),
                                      grayscale = FALSE,
                                      ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::continuous_scale(
    "fill", "loscolores_div",
    palette = scales::gradient_n_pal(
      sample_system(sys, "divergent", option, n = 101L, center = center,
                    grayscale = grayscale)
    ),
    ...
  )
}

#' Loscolores divergent colour scale
#' @rdname scale_fill_loscolores_div
#' @export
scale_color_loscolores_div <- function(option    = "Option_A",
                                       center    = "white",
                                       sys       = getOption("loscolores.default_system"),
                                       grayscale = FALSE,
                                       ...) {
  if (is.null(sys))
    stop("No hay sistema activo. Llama set.loscolores() primero.", call. = FALSE)
  ggplot2::continuous_scale(
    "colour", "loscolores_div",
    palette = scales::gradient_n_pal(
      sample_system(sys, "divergent", option, n = 101L, center = center,
                    grayscale = grayscale)
    ),
    ...
  )
}

#' @rdname scale_fill_loscolores_div
#' @export
scale_colour_loscolores_div <- scale_color_loscolores_div