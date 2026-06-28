# ==============================================================================
# engine_expansion.R
# Oklab Topology Projections and Interpolation Routing
# ==============================================================================

#' Build expansion parameter nodes for all palette topologies
#'
#' @param raw_5_hex  Character[5]. Core colors from \code{get_raw_5}.
#' @param hex_black  Active dark pole.
#' @param hex_white  Active light pole.
#' @return Nested list with slots \code{categorical}, \code{sequential},
#'         \code{divergent}. Each slot has Options A–E (categorical) or A–E
#'         (sequential/divergent).
#' @importFrom dplyr mutate select
#' @export
get_expansion_params <- function(raw_5_hex, hex_black, hex_white) {

  lab_5  <- to_oklab(raw_5_hex)
  w_node <- to_oklab(hex_white)
  b_node <- to_oklab(hex_black)

  exp <- list()

  # ── 1. CATEGORICAL TOPOLOGIES ────────────────────────────────────────────────
  # Option_A  : Natural order (L-Dictator sequence)
  # Option_B/C: Contrast-jump reorderings
  # Option_D  : Forced chroma floor + subtle hue shift (max vibrance)
  # Option_DIAG: Isostatic L=0.60 — diagnostic only, not exposed in production
  exp$categorical <- list(
    Option_A = lab_5,
    Option_B = lab_5[c(1L, 4L, 2L, 5L, 3L), ],
    Option_C = lab_5[c(1L, 5L, 2L, 4L, 3L), ],
    Option_D = dplyr::mutate(
      lab_5,
      c_new = ifelse(c > 0.01, pmax(c, 0.15), c),
      h_new = (h + 15) %% 360,
      a     = cos(h_new * pi / 180) * c_new,
      b     = sin(h_new * pi / 180) * c_new
    ) |> dplyr::select(L, a, b, c = c_new, h = h_new)
  )
  # Diagnostic variant kept separate — never iterated in plot.lc_system
  exp$categorical_diag <- list(
    Option_DIAG = dplyr::mutate(lab_5, L = 0.60)
  )

  # ── 2. HELPER: polar target displacement ─────────────────────────────────────
  get_target <- function(row, dL, dh) {
    nL <- pmax(0.05, pmin(0.95, row$L + dL))
    nh <- (row$h + dh) %% 360
    data.frame(L = nL,
               a = cos(nh * pi / 180) * row$c,
               b = sin(nh * pi / 180) * row$c,
               c = row$c,
               h = nh)
  }

  # ── 3. SEQUENTIAL TOPOLOGIES ─────────────────────────────────────────────────
  # FIX: end node → white pole (desaturated), not a blind +0.5 L offset.
  # This guarantees the ramp always reaches a real light anchor without clipping.
  exp$sequential <- lapply(seq_len(5L), function(i) {
    list(
      start = lab_5[i, ],
      end   = data.frame(
        L = w_node$L * 0.97,          # stay just shy of pure white
        a = lab_5[i, ]$a * 0.04,      # bleed only a ghost of the hue
        b = lab_5[i, ]$b * 0.04,
        c = lab_5[i, ]$c * 0.04,
        h = lab_5[i, ]$h
      )
    )
  })

  # ── 4. DIVERGENT TOPOLOGIES ──────────────────────────────────────────────────
  # low  : complement of the base color, shifted darker
  # mid  : white pole (default center)
  # mid_dark: black pole (alternative center)
  # high : base color
  exp$divergent <- lapply(seq_len(5L), function(i) {
    list(
      low      = get_target(lab_5[i, ], -0.4, 180),
      mid      = w_node,
      mid_dark = b_node,
      high     = lab_5[i, ]
    )
  })

  names(exp$sequential) <- names(exp$divergent) <- paste0("Option_", LETTERS[1L:5L])

  exp
}


#' Sample a palette from an lc_system object
#'
#' @param sys       An \code{lc_system} object.
#' @param type      One of \code{"categorical"}, \code{"sequential"},
#'                  \code{"divergent"}.
#' @param option    Variant key: \code{"Option_A"} through \code{"Option_D"}
#'                  (categorical) or \code{"Option_A"} through \code{"Option_E"}
#'                  (sequential/divergent).
#' @param n         Number of output colors.
#' @param center    \code{"white"} or \code{"black"} (divergent only).
#' @param grayscale Logical. Strip chroma from output.
#' @return Character vector of \code{n} #RRGGBB codes.
#' @importFrom stats splinefun
#' @export
sample_system <- function(sys,
                          type      = "categorical",
                          option    = "Option_A",
                          n         = 10L,
                          center    = "white",
                          grayscale = FALSE) {

  params <- sys$expansion[[type]][[option]]
  if (is.null(params))
    stop(sprintf("Topología '%s / %s' no encontrada en el sistema.", type, option),
         call. = FALSE)

  # ── Categorical ──────────────────────────────────────────────────────────────
  if (type == "categorical") {
    n_nodes <- nrow(params)
    t_in    <- seq(0, 1, length.out = n_nodes)
    t_out   <- seq(0, 1, length.out = n)

    L_out <- stats::splinefun(t_in, params$L, method = "monoH.FC")(t_out)
    a_out <- stats::splinefun(t_in, params$a, method = "monoH.FC")(t_out)
    b_out <- stats::splinefun(t_in, params$b, method = "monoH.FC")(t_out)

    if (grayscale) { a_out[] <- 0; b_out[] <- 0 }

    hex_out <- to_hex(L_out, a_out, b_out)

    # Anchor protection: slot 1 is always the exact anchor
    if (!grayscale && n > 0L)
      hex_out[1L] <- to_hex(params$L[1L], params$a[1L], params$b[1L])

    return(hex_out)
  }

  # ── Sequential ───────────────────────────────────────────────────────────────
  t <- seq(0, 1, length.out = n)

  if (type == "sequential") {
    L_out <- params$start$L + t * (params$end$L - params$start$L)
    a_out <- if (grayscale) rep(0, n) else
      params$start$a + t * (params$end$a - params$start$a)
    b_out <- if (grayscale) rep(0, n) else
      params$start$b + t * (params$end$b - params$start$b)
    return(to_hex(L_out, a_out, b_out))
  }

  # ── Divergent ─────────────────────────────────────────────────────────────────
  if (type == "divergent") {
    mid_pt   <- if (center == "white") params$mid else params$mid_dark
    L_out    <- numeric(n)
    a_out    <- numeric(n)
    b_out    <- numeric(n)

    idx_low  <- t <= 0.5
    t_low    <- t[idx_low] * 2
    L_out[idx_low] <- params$low$L + t_low * (mid_pt$L - params$low$L)
    a_out[idx_low] <- params$low$a + t_low * (mid_pt$a - params$low$a)
    b_out[idx_low] <- params$low$b + t_low * (mid_pt$b - params$low$b)

    idx_high    <- !idx_low
    t_high      <- (t[idx_high] - 0.5) * 2
    L_out[idx_high] <- mid_pt$L + t_high * (params$high$L - mid_pt$L)
    a_out[idx_high] <- mid_pt$a + t_high * (params$high$a - mid_pt$a)
    b_out[idx_high] <- mid_pt$b + t_high * (params$high$b - mid_pt$b)

    if (grayscale) { a_out[] <- 0; b_out[] <- 0 }

    return(to_hex(L_out, a_out, b_out))
  }

  stop("type debe ser 'categorical', 'sequential' o 'divergent'.", call. = FALSE)
}