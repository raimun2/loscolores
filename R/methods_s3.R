# ==============================================================================
# methods_s3.R
# Public constructors, state mutators, and S3 methods for lc_system
# ==============================================================================

#' Build a loscolores perceptual color system
#'
#' @param x HEX vector, R color names, or a sysdata palette key.
#' @param pivot Logical. Rotate two satellites when hues are jammed (turn off
#'   for monochrome systems).
#' @return An object of class \code{lc_system}.
#' @export
loscolores_system <- function(x, pivot = TRUE) {
  input_hex <- get_input_vector(x)
  core      <- get_raw_5(input_hex, pivot = pivot)

  structure(
    list(
      engine        = LC_ENGINE_VERSION,
      input         = input_hex,
      raw_5         = core$raw_5,
      polos         = list(black = core$hex_black, white = core$hex_white),
      pole_source   = core$pole_source,
      anchor_origin = core$anchor_origin,
      lab           = core$lab,
      min_gap       = core$min_gap,
      hue_variance  = core$hue_variance,
      pivot         = pivot,
      log           = core$log
    ),
    class = "lc_system"
  )
}


#' Set the active loscolores system for the session
#'
#' Stores the computed system in \code{getOption("loscolores.default_system")}.
#' Every \code{scale_*_loscolores_*()} and \code{theme_loscolores()} call reads
#' from this same option, so the palette stays set for the rest of the script.
#' Returns the system invisibly to allow optional inspection via assignment.
#'
#' @param ... One or more HEX strings or R color names (e.g. \code{set_loscolores("red")}).
#' @param pivot Logical. Rotate two satellites when hues are jammed.
#' @return An \code{lc_system} object (invisible).
#' @export
set_loscolores <- function(..., pivot = TRUE) {
  input_raw <- unlist(list(...))
  sys       <- loscolores_system(input_raw, pivot = pivot)
  options(loscolores.default_system = sys)
  invisible(sys)
}


#' Retrieve the active loscolores system
#'
#' @return The current \code{lc_system} from session options, or \code{NULL}.
#' @export
get_loscolores <- function() {
  getOption("loscolores.default_system")
}


# -- S3: print -----------------------------------------------------------------

#' @export
print.lc_system <- function(x, ...) {
  cli::cli_h1("loscolores - Stabilized Perceptual System")
  cli::cli_text("{.strong Engine:} {x$engine}")
  cli::cli_text("{.strong Dark pole:} {x$polos$black}  {.strong Light pole:} {x$polos$white}")
  cli::cli_text("{.strong Anchor:} {x$raw_5[1]}")
  styled <- vapply(c(x$polos$black, x$raw_5, x$polos$white), function(h) {
    cli::make_ansi_style(h, bg = TRUE)("  ")
  }, character(1L))
  cat(paste(x$raw_5, collapse = "  "), "\n")
  cat(paste(styled,  collapse = ""),  "\n\n")
  cli::cli_h2("How it got here")
  for (i in seq_len(nrow(x$log)))
    cli::cli_text("{.strong [{x$log$law[i]}]} {x$log$text[i]}")
  invisible(x)
}


# -- S3: system_report ---------------------------------------------------------

#' Diagnostic report for an lc_system
#'
#' @param sys An \code{lc_system} object.
#' @export
system_report <- function(sys) {
  cli::cli_h1("loscolores \u2014 System Diagnostics")

  L_raw <- vapply(sys$lab$raw5, `[[`, 0, "L")
  c_raw <- vapply(sys$lab$raw5, `[[`, 0, "c")

  cli::cli_h2("Luminance ladder (poles + core)")
  l_vals <- round(c(black = sys$lab$black[["L"]], stats::setNames(L_raw, paste0("C", 1:5)),
                    white = sys$lab$white[["L"]]), 3L)
  cli::cli_dl(as.list(l_vals))
  cli::cli_text("Minimum gap: {round(sys$min_gap, 3)}")

  cli::cli_h2("Chroma")
  avg_c  <- mean(c_raw)
  status <- if (avg_c > 0.12) "Vibrant" else if (avg_c > 0.05) "Standard" else "Muted"
  cli::cli_text("Average chroma: {round(avg_c, 3)} [{status}]")

  cli::cli_h2("Hue circular variance (input after distil/expand)")
  jam_flag <- if (sys$hue_variance < 0.05) " \u26a0\ufe0f  Traffic jam" else " \u2713 OK"
  cli::cli_text("Circular variance: {round(sys$hue_variance, 3)}{jam_flag}")

  invisible(sys)
}


# -- S3: plot ------------------------------------------------------------------

#' Plot a full diagnostic panel for an lc_system
#'
#' Three blocks:
#'   A  - Raw signal integration (input, Raw7, greyscale check)
#'   B1 - Sequential ramps (Options A-E)
#'   B2 - Divergent ramps (Options A-E, white center)
#'   C  - Categorical projections N=10 (Options A-D only; DIAG excluded)
#'
#' @param x   An \code{lc_system} object.
#' @param ... Ignored.
#' @import ggplot2
#' @import patchwork
#' @importFrom dplyr bind_rows
#' @export
plot.lc_system <- function(x, ...) {

  # -- Block A: signal integration ----------------------------------------------
  raw_7  <- c(x$polos$white, x$raw_5, x$polos$black)
  gray_7 <- to_hex(to_oklab(raw_7)$L, 0, 0)  # greyscale check of the 7 levels

  # Tile width must match each row's own point spacing, or short input vectors
  # (e.g. 4 colors stretched across positions 1-7) leave visible gaps between
  # tiles instead of filling the row edge-to-edge.
  n_in <- length(x$input)
  w_in <- if (n_in > 1L) (6 / (n_in - 1L)) * 0.95 else 6 * 0.95

  df_A <- dplyr::bind_rows(
    data.frame(level = "1. Raw Input",
               pos   = seq(1, 7, length.out = n_in),
               hex   = x$input,
               wid   = w_in),
    data.frame(level = "2. Raw 7 (Poles + Core)",
               pos   = seq_len(7L),
               hex   = raw_7,
               wid   = 0.95),
    data.frame(level = "3. L-Check (Greyscale)",
               pos   = seq_len(7L),
               hex   = gray_7,
               wid   = 0.95)
  )
  df_A$level <- factor(df_A$level, levels = rev(unique(df_A$level)))

  # Axis limits must fit the widest tile actually drawn (the input row can be
  # much wider than 0.95 when n_in is small), or the outer edge tiles get
  # clipped by the panel boundary.
  max_half_w <- max(df_A$wid) / 2 + 0.05
  x_lims     <- c(1 - max_half_w, 7 + max_half_w)

  pA <- ggplot2::ggplot(df_A, ggplot2::aes(x = pos, y = level, fill = hex, width = wid)) +
    ggplot2::geom_tile(color = x$polos$white, linewidth = 1,
                       height = 0.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = x_lims, breaks = seq_len(7L)) +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(hjust = 1,
                                           margin = ggplot2::margin(r = 10),
                                           size   = 9),
      plot.title   = ggplot2::element_text(face   = "bold",
                                           margin = ggplot2::margin(b = 10))
    ) +
    ggplot2::labs(title = "Block A: Signal Integration")

  # -- Block B: continuous topologies ------------------------------------------
  opts_all <- paste0("Option_", LETTERS[1L:5L])

  df_B1 <- dplyr::bind_rows(lapply(opts_all, function(opt) {
    pal <- sample_system(x, "sequential", opt, n = 100L)
    data.frame(variant = sub("Option_", "Opt ", opt), y = seq_len(100L), hex = pal)
  }))
  df_B1$variant <- factor(df_B1$variant, levels = rev(unique(df_B1$variant)))

  pB1 <- ggplot2::ggplot(df_B1, ggplot2::aes(y = variant, x = y, fill = hex)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5))
    ) +
    ggplot2::labs(title = "Block B1: Sequential")

  df_B2 <- dplyr::bind_rows(lapply(opts_all, function(opt) {
    pal <- sample_system(x, "divergent", opt, center = "white", n = 100L)
    data.frame(variant = sub("Option_", "Opt ", opt), y = seq_len(100L), hex = pal)
  }))
  df_B2$variant <- factor(df_B2$variant, levels = rev(unique(df_B2$variant)))

  pB2 <- ggplot2::ggplot(df_B2, ggplot2::aes(y = variant, x = y, fill = hex)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5))
    ) +
    ggplot2::labs(title = "Block B2: Divergent (white center)")

  # -- Block C: categorical (Options A-D only; DIAG not plotted) ---------------
  opts_cat <- paste0("Option_", LETTERS[1L:4L])

  df_C <- dplyr::bind_rows(lapply(opts_cat, function(opt) {
    pal <- sample_system(x, "categorical", opt, n = 10L)
    data.frame(variant = sub("Option_", "Opt ", opt), x_pos = seq_len(10L), hex = pal)
  }))
  df_C$variant <- factor(df_C$variant, levels = rev(unique(df_C$variant)))

  pC <- ggplot2::ggplot(df_C, ggplot2::aes(x = x_pos, y = variant, fill = hex)) +
    ggplot2::geom_tile(color = x$polos$white, linewidth = 1) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 1,
                                          margin = ggplot2::margin(r = 10))
    ) +
    ggplot2::labs(title = "Block C: Categorical Projections (N=10, Options A\u2013D)")

  # -- Assembly -----------------------------------------------------------------
  (pA) / (pB1 | pB2) / (pC) +
    patchwork::plot_layout(heights = c(1.5, 1.5, 1.5))
}