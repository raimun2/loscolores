# ==============================================================================
# methods_s3.R
# Public constructors, state mutators, and S3 methods for lc_system
# ==============================================================================

#' Build a loscolores perceptual color system
#'
#' @param x HEX vector, R color names, or a sysdata palette key.
#' @return An object of class \code{lc_system}.
#' @export
loscolores_system <- function(x) {
  input_hex  <- get_input_vector(x)
  core       <- get_raw_5(input_hex)
  expansions <- get_expansion_params(core$raw_5, core$hex_black, core$hex_white)

  structure(
    list(
      input     = input_hex,
      raw_5     = core$raw_5,
      polos     = list(black = core$hex_black, white = core$hex_white),
      expansion = expansions
    ),
    class = "lc_system"
  )
}


#' Set the active loscolores system for the session
#'
#' Stores the computed system in \code{getOption("loscolores.default_system")}.
#' Returns the system invisibly to allow optional inspection via assignment.
#'
#' @param ... One or more HEX strings, R color names, or a single palette key.
#' @return An \code{lc_system} object (invisible).
#' @export
set.loscolores <- function(...) {
  input_raw <- unlist(list(...))
  sys       <- loscolores_system(input_raw)
  options(loscolores.default_system = sys)
  invisible(sys)
}


#' Retrieve the active loscolores system
#'
#' @return The current \code{lc_system} from session options, or \code{NULL}.
#' @export
get.loscolores <- function() {
  getOption("loscolores.default_system")
}


# ── S3: print ─────────────────────────────────────────────────────────────────

#' @export
print.lc_system <- function(x, ...) {
  cli::cli_h1("loscolores \u2014 Sistema Perceptual Estabilizado")
  cli::cli_alert_success("Estado: Activo | Entr\u00f4\u00eda: Controlada")

  cli::cli_text("{.strong Polo claro:} {x$polos$white}")
  cli::cli_text("{.strong Polo oscuro:} {x$polos$black}")
  cli::cli_text("{.strong Ancla (raw_5[1]):} {x$raw_5[1]}")
  cli::cli_text("{.strong Raw 5:}")

  # Render each HEX using cli ANSI styles (col_br_* are available in cli >= 3.6)
  styled <- vapply(x$raw_5, function(h) {
    cli::make_ansi_style(h, bg = TRUE)(" ") # colored block
  }, character(1L))
  cat(paste(x$raw_5, collapse = "  "), "\n")
  cat(paste(styled,  collapse = " "),  "\n\n")

  invisible(x)
}


# ── S3: system_report ─────────────────────────────────────────────────────────

#' Diagnostic report for an lc_system
#'
#' @param sys An \code{lc_system} object.
#' @export
system_report <- function(sys) {
  cli::cli_h1("loscolores \u2014 System Diagnostics")

  raw_lab <- to_oklab(sys$raw_5)

  cli::cli_h2("Luminance Hierarchy (L-Dictator output)")
  l_vals        <- round(raw_lab$L, 3L)
  names(l_vals) <- paste0("C", seq_along(l_vals))
  cli::cli_dl(as.list(l_vals))

  cli::cli_h2("Luminance gaps")
  gaps        <- round(diff(sort(raw_lab$L)), 3L)
  names(gaps) <- paste0("gap", seq_along(gaps))
  cli::cli_dl(as.list(gaps))

  cli::cli_h2("Chroma Entropy")
  avg_c  <- mean(raw_lab$c)
  status <- if (avg_c > 0.12) "Vibrant" else if (avg_c > 0.05) "Standard" else "Muted"
  cli::cli_text("Average chroma: {round(avg_c, 3)} [{status}]")

  cli::cli_h2("Hue Circular Variance")
  h_rad    <- raw_lab$h * pi / 180
  circ_var <- 1 - sqrt(mean(cos(h_rad))^2 + mean(sin(h_rad))^2)
  jam_flag <- if (circ_var < 0.05) " \u26a0\ufe0f  Traffic Jam detected" else " \u2713 OK"
  cli::cli_text("Circular variance: {round(circ_var, 3)}{jam_flag}")

  invisible(sys)
}


# ── S3: plot ──────────────────────────────────────────────────────────────────

#' Plot a full diagnostic panel for an lc_system
#'
#' Three blocks:
#'   A  — Raw signal integration (input, Raw7, greyscale check)
#'   B1 — Sequential ramps (Options A–E)
#'   B2 — Divergent ramps (Options A–E, white center)
#'   C  — Categorical projections N=10 (Options A–D only; DIAG excluded)
#'
#' @param x   An \code{lc_system} object.
#' @param ... Ignored.
#' @import ggplot2
#' @import patchwork
#' @importFrom dplyr bind_rows
#' @export
plot.lc_system <- function(x, ...) {

  # ── Block A: signal integration ──────────────────────────────────────────────
  raw_7  <- c(x$polos$white, x$raw_5, x$polos$black)
  gray_7 <- to_hex(to_oklab(raw_7)$L, 0, 0)

  df_A <- dplyr::bind_rows(
    data.frame(nivel = "1. Raw Input",
               pos   = seq(1, 7, length.out = length(x$input)),
               hex   = x$input),
    data.frame(nivel = "2. Raw 7 (Poles + Core)",
               pos   = seq_len(7L),
               hex   = raw_7),
    data.frame(nivel = "3. L-Check (Greyscale)",
               pos   = seq_len(7L),
               hex   = gray_7)
  )
  df_A$nivel <- factor(df_A$nivel, levels = rev(unique(df_A$nivel)))

  pA <- ggplot2::ggplot(df_A, ggplot2::aes(x = pos, y = nivel, fill = hex)) +
    ggplot2::geom_tile(color = x$polos$white, linewidth = 1,
                       width = 0.95, height = 0.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0.5, 7.5), breaks = seq_len(7L)) +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(hjust = 1,
                                           margin = ggplot2::margin(r = 10),
                                           size   = 9),
      plot.title   = ggplot2::element_text(face   = "bold",
                                           margin = ggplot2::margin(b = 10))
    ) +
    ggplot2::labs(title = "Block A: Signal Integration")

  # ── Block B: continuous topologies ──────────────────────────────────────────
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

  # ── Block C: categorical (Options A–D only; DIAG not plotted) ───────────────
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

  # ── Assembly ─────────────────────────────────────────────────────────────────
  (pA) / (pB1 | pB2) / (pC) +
    patchwork::plot_layout(heights = c(1.5, 1.5, 1.5))
}