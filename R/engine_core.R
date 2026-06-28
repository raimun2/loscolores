# ==============================================================================
# engine_core.R
# Oklab Mathematical Engine: Pole Hijacking, Dimensional Expansion & L-Dictator
# ==============================================================================

#' Default pole accessors
#' @export
lc_white <- function() to_oklab("#F4F3F3")
#' @export
lc_black <- function() to_oklab("#1D1C1B")


#' Build the stabilized 5-color core (Raw 5)
#'
#' Accepts a sanitized HEX vector of any length and returns exactly 5 colors
#' whose luminance values are perceptually separated (L-Dictator guarantee),
#' plus the active black and white poles.
#'
#' @param input_hex   Sanitized #RRGGBB vector (output of \code{get_input_vector}).
#' @param hex_black   Default dark pole.
#' @param hex_white   Default light pole.
#' @return Named list: \code{raw_5} (chr[5]), \code{hex_black}, \code{hex_white}.
#' @importFrom stats kmeans
#' @export
get_raw_5 <- function(input_hex,
                      hex_black = "#1D1C1B",
                      hex_white = "#F4F3F3") {

  lab_init     <- to_oklab(input_hex)
  L_black_base <- to_oklab(hex_black)$L[1L]
  L_white_base <- to_oklab(hex_white)$L[1L]

  # ── 1. POLE HIJACKING (The Horizon) ─────────────────────────────────────────
  # Colors falling inside the pole zones override the default poles.
  zone_black <- L_black_base + 0.08
  zone_white <- L_white_base - 0.08

  in_black <- lab_init$L <= zone_black
  in_white <- lab_init$L >= zone_white

  hex_black_active <- if (any(in_black)) input_hex[which(in_black)[1L]] else hex_black
  hex_white_active <- if (any(in_white)) input_hex[which(in_white)[1L]] else hex_white

  L_black_active <- to_oklab(hex_black_active)$L[1L]
  L_white_active <- to_oklab(hex_white_active)$L[1L]

  # Remove pole-zone colors from the chromatic signal
  is_signal <- !in_black & !in_white

  # Edge case: input is entirely poles → keep first color as a degenerate anchor
  if (!any(is_signal)) {
    lab_signal <- lab_init[1L, , drop = FALSE]
  } else {
    lab_signal <- lab_init[is_signal, , drop = FALSE]
  }

  n_sig     <- nrow(lab_signal)
  basal_lab <- lab_signal[1L, , drop = FALSE]   # The Immutable Anchor

  # ── 2. DISTILLATION / EXPANSION ─────────────────────────────────────────────
  min_L_safe <- L_black_active + 0.08
  max_L_safe <- L_white_active - 0.08

  if (n_sig > 5L) {
    # Overflow → k-means on satellites, anchor is protected
    others_data   <- lab_signal[-1L, c("L", "a", "b"), drop = FALSE]
    unique_others <- unique(others_data)

    if (nrow(unique_others) >= 4L) {
      suppressWarnings(
        centros <- stats::kmeans(others_data,
                                 centers  = 4L,
                                 iter.max = 20L,
                                 nstart   = 5L)$centers
      )
    } else {
      idx     <- rep(seq_len(nrow(unique_others)), length.out = 4L)
      centros <- unique_others[idx, , drop = FALSE]
    }
    pool <- as.data.frame(rbind(basal_lab[, c("L", "a", "b")], centros))

  } else if (n_sig == 5L) {
    pool <- as.data.frame(lab_signal[, c("L", "a", "b")])

  } else {
    # Underflow → radial expansion
    # FIX: seed satellites at evenly-spread L values, not at anchor's L.
    # This prevents L-Dictator from having to shove everything upward.
    n_missing <- 5L - n_sig

    is_void <- basal_lab$c < 0.02
    c_seed  <- if (is_void) 0.12 else basal_lab$c
    h_ref   <- if (is_void) 240  else basal_lab$h

    h_offsets <- seq(60, 60 * n_missing, length.out = n_missing)
    new_hues  <- (h_ref + h_offsets) %% 360

    # Spread satellite L values across the safe luminance range,
    # excluding positions already close to the anchor's L.
    L_candidates <- seq(min_L_safe, max_L_safe, length.out = n_missing + 2L)
    L_candidates <- L_candidates[
      abs(L_candidates - basal_lab$L[1L]) > 0.10
    ]
    # Guarantee we always have enough slots (rare edge: very tight range)
    if (length(L_candidates) < n_missing)
      L_candidates <- seq(min_L_safe, max_L_safe, length.out = n_missing)
    L_new <- L_candidates[seq_len(n_missing)]

    others <- data.frame(
      L = L_new,
      a = cos(new_hues * pi / 180) * c_seed,
      b = sin(new_hues * pi / 180) * c_seed
    )
    pool <- as.data.frame(rbind(lab_signal[, c("L", "a", "b")], others))
  }

  # Recalculate polar metrics for the consolidated pool
  pool$c <- sqrt(pool$a^2 + pool$b^2)
  pool$h <- (atan2(pool$b, pool$a) * 180 / pi) %% 360

  # ── 3. HUE PIVOT (Traffic Jam Protection) ───────────────────────────────────
  h_rad    <- pool$h * pi / 180
  circ_var <- 1 - sqrt(mean(cos(h_rad))^2 + mean(sin(h_rad))^2)

  if (circ_var < 0.05) {
    pool$h[3L] <- (pool$h[3L] + 40) %% 360
    pool$h[5L] <- (pool$h[5L] - 40) %% 360
    pool$a[c(3L, 5L)] <- cos(pool$h[c(3L, 5L)] * pi / 180) * pool$c[c(3L, 5L)]
    pool$b[c(3L, 5L)] <- sin(pool$h[c(3L, 5L)] * pi / 180) * pool$c[c(3L, 5L)]
  }

  # ── 4. L-DICTATOR (Iron Law of Perceptual Separation) ───────────────────────
  min_gap <- 0.15
  L_vals  <- pool$L

  # Anchor: clamp to safe zone but never change further — L is sacred.
  L_anchor_original <- basal_lab$L[1L]
  L_vals[1L] <- pmax(min_L_safe, pmin(max_L_safe, L_anchor_original))

  # Process satellites in ascending L order.
  idx_sat <- 2L:5L
  sat_ord <- idx_sat[order(L_vals[idx_sat])]

  for (i in sat_ord) {
    # Re-read others_L inside the while so moves made to earlier satellites
    # are visible when checking the current one. Capture by index, not value.
    other_idx <- setdiff(seq_along(L_vals), i)
    while (any(abs(L_vals[i] - L_vals[other_idx]) < min_gap) &&
           L_vals[i] < max_L_safe) {
      L_vals[i] <- L_vals[i] + 0.02
    }
  }

  # Structural compression fallback: fires when iterative push overflows the
  # safe ceiling or gaps are still too tight.
  # Critical: the anchor (slot 1) is NEVER reassigned here.
  # Only the 4 satellites are redistributed.
  needs_reset <- any(L_vals[idx_sat] > max_L_safe) ||
    any(diff(sort(L_vals)) < (min_gap * 0.8))

  if (needs_reset) {
    L_anchor <- L_vals[1L]   # preserve
    # Build 4 satellite positions that are all >= min_gap from the anchor
    # and from each other, distributed across the safe range.
    L_lineal_full <- seq(min_L_safe, max_L_safe, length.out = 5L)
    # Drop any candidate too close to the anchor
    L_sat_candidates <- L_lineal_full[abs(L_lineal_full - L_anchor) >= min_gap]
    if (length(L_sat_candidates) < 4L) {
      # Fallback: force-distribute ignoring the anchor position constraint
      # (degenerate input, e.g. anchor at exact midpoint of a tiny range)
      L_sat_candidates <- seq(min_L_safe, max_L_safe, length.out = 6L)
      L_sat_candidates <- L_sat_candidates[L_sat_candidates != L_anchor]
      L_sat_candidates <- head(L_sat_candidates, 4L)
    }
    L_sat_new <- L_sat_candidates[seq_len(4L)]
    # Assign to satellites preserving their relative luminance order
    sat_ord_reset      <- idx_sat[order(L_vals[idx_sat])]
    L_vals[sat_ord_reset] <- sort(L_sat_new)
    L_vals[1L]         <- L_anchor          # anchor unchanged
  }

  pool$L <- L_vals

  # ── 5. FINAL ASSEMBLY ────────────────────────────────────────────────────────
  # Output order: anchor first, satellites sorted ascending by L.
  anchor_row    <- pool[1L, , drop = FALSE]
  sat_rows      <- pool[2L:5L, ]
  sat_rows      <- sat_rows[order(sat_rows$L), ]
  raw_5_df      <- rbind(anchor_row, sat_rows)

  raw_5_hex <- to_hex(raw_5_df$L, raw_5_df$a, raw_5_df$b)

  list(
    raw_5     = raw_5_hex,
    hex_black = hex_black_active,
    hex_white = hex_white_active
  )
}