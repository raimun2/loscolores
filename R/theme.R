# ==============================================================================
# theme.R
# Dynamic ggplot2 theme driven by the active lc_system poles
# ==============================================================================

#' Compute a utility slate color from the active poles
#'
#' Blends 85% of the white pole's lightness with 15% of the black pole's,
#' carrying a ghost of the black pole's chromatic tint.
#'
#' @param sys An \code{lc_system} object.
#' @return One #RRGGBB string.
#' @keywords internal
get_dynamic_slate <- function(sys) {
  # FIX: renamed lab_white / lab_black to avoid the ambiguous b$b read
  lab_white <- to_oklab(sys$polos$white)
  lab_black <- to_oklab(sys$polos$black)

  to_hex(
    L = lab_white$L * 0.85 + lab_black$L * 0.15,
    a = lab_black$a * 0.20,
    b = lab_black$b * 0.20
  )
}


#' Dynamic loscolores ggplot2 theme
#'
#' All colors are derived from the currently active \code{lc_system} poles,
#' so the theme stays coherent after any \code{set.loscolores()} call.
#'
#' @param base_family Primary typeface. Defaults to \code{"dm"} (DM Sans via
#'   Google Fonts). Falls back to \code{"sans"} if unavailable.
#' @import ggplot2
#' @importFrom sysfonts font_families font_add_google
#' @importFrom showtext showtext_auto
#' @export
theme_loscolores <- function(base_family = "dm") {

  # Lazy font loading — silently skips if no internet
  if (!("dm" %in% sysfonts::font_families())) {
    tryCatch(
      sysfonts::font_add_google("DM Sans", "dm"),
      error = function(e) base_family <<- "sans"
    )
  }
  if (!("space" %in% sysfonts::font_families())) {
    tryCatch(
      sysfonts::font_add_google("Space Grotesk", "space"),
      error = function(e) NULL
    )
  }
  showtext::showtext_auto()

  sys <- getOption("loscolores.default_system")
  if (is.null(sys))
    stop("El sistema no está inicializado. Llama set.loscolores() primero.",
         call. = FALSE)

  col_white <- sys$polos$white
  col_black <- sys$polos$black
  col_slate <- get_dynamic_slate(sys)

  title_family <- if ("space" %in% sysfonts::font_families()) "space" else base_family

  ggplot2::theme_minimal(base_family = base_family) +
    ggplot2::theme(
      text              = ggplot2::element_text(color = col_black),
      plot.title        = ggplot2::element_text(family = title_family,
                                                face   = "bold"),
      plot.subtitle     = ggplot2::element_text(family = base_family),
      plot.background   = ggplot2::element_rect(fill = col_white, color = NA),
      panel.background  = ggplot2::element_rect(fill = col_white, color = NA),
      panel.grid.major  = ggplot2::element_line(color     = col_slate,
                                                linewidth = 0.2),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.title        = ggplot2::element_text(color = col_black),
      axis.text         = ggplot2::element_text(color = col_black),
      legend.background = ggplot2::element_rect(fill = col_white, color = NA),
      legend.key        = ggplot2::element_rect(fill = col_white, color = NA),
      legend.text       = ggplot2::element_text(color = col_black),
      legend.title      = ggplot2::element_text(color = col_black),
      strip.background  = ggplot2::element_rect(fill = col_slate, color = NA),
      strip.text        = ggplot2::element_text(family = title_family,
                                                color  = col_white)
    )
}