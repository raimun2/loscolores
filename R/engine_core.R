# ==============================================================================
# engine_core.R
# The core: 2 neutral poles + 5 colours. Mirrors web/public/engine.js
# (buildSystem). Change both engines together; tests/testthat/test-engine.R
# checks they return the same hex codes.
#
# Laws:
#  1. Rules over taste: every output obeys the same perceptual laws.
#  2. The anchor (first input) is sacred: exact hex, exact L, always slot 1.
#  3. Luminance is the backbone: 7 levels spread in L so the system reads in
#     greyscale. Gamut mapping never touches L, only chroma.
#  4. Scaffolding first: poles are the horizon and are always neutral greys.
#     Only an achromatic input near a pole can become that pole.
#  5. Any input resolves to the same skeleton: distil (>5) or expand (<5).
#  6. Pathological inputs have explicit exits, never a collapse.
#  7. One core, many uses: categorical, sequential, divergent, theme.
#  8. Transparent: every decision is logged.
# ==============================================================================

LC_ENGINE_VERSION <- "1.1.1"
LC_POLE_ZONE   <- 0.08   # L distance from a default pole that counts as pole territory
LC_POLE_CHROMA <- 0.04   # above this chroma a colour is a colour, never a pole
LC_VOID_CHROMA <- 0.02   # below this chroma a colour has no usable hue
LC_VOID_HUE    <- 240    # hue seed when the anchor is achromatic
LC_VOID_SEED_C <- 0.12   # chroma seed when the anchor is achromatic
LC_JAM_VAR     <- 0.05   # hue circular variance below this is a traffic jam
LC_PIVOT_DEG   <- 40
LC_TIE         <- 1e-9   # near-ties go to the first candidate (stable across engines)

lc_sum <- function(x) Reduce(`+`, x, 0)   # left-to-right double sum, like JS reduce

lc_dist <- function(p, q) {
  dL <- p[["L"]] - q[["L"]]; da <- p[["a"]] - q[["a"]]; db <- p[["b"]] - q[["b"]]
  sqrt(dL * dL + da * da + db * db)
}

lc_hue_gap <- function(x, y) { d <- abs(x - y) %% 360; min(d, 360 - d) }

lc_circular_variance <- function(hues) {
  if (length(hues) < 2L) return(1)
  r <- hues * pi / 180
  C <- lc_sum(cos(r)) / length(r)
  S <- lc_sum(sin(r)) / length(r)
  1 - sqrt(C * C + S * S)
}

# Deterministic k-means in OKLab (farthest-point init seeded by the anchor).
# Returns medoids: real input colours, so satellites are colours the user brought.
lc_distil <- function(points, k, anchor) {
  first <- 1L; best <- -1
  for (i in seq_along(points)) { d <- lc_dist(points[[i]], anchor); if (d > best + LC_TIE) { best <- d; first <- i } }
  centres <- list(points[[first]])
  while (length(centres) < k) {
    idx <- 1L; far <- -1
    for (i in seq_along(points)) {
      d <- min(c(lc_dist(points[[i]], anchor), vapply(centres, function(cc) lc_dist(points[[i]], cc), 0)))
      if (d > far + LC_TIE) { far <- d; idx <- i }
    }
    centres[[length(centres) + 1L]] <- points[[idx]]
  }
  for (it in seq_len(25L)) {
    assign <- vapply(points, function(p) {
      bi <- 1L
      for (i in seq_along(centres)) if (lc_dist(p, centres[[i]]) < lc_dist(p, centres[[bi]]) - LC_TIE) bi <- i
      bi
    }, 1L)
    for (i in seq_along(centres)) {
      mem <- points[assign == i]
      if (length(mem)) {
        centres[[i]][["L"]] <- lc_sum(vapply(mem, `[[`, 0, "L")) / length(mem)
        centres[[i]][["a"]] <- lc_sum(vapply(mem, `[[`, 0, "a")) / length(mem)
        centres[[i]][["b"]] <- lc_sum(vapply(mem, `[[`, 0, "b")) / length(mem)
      }
    }
  }
  used <- integer(0)
  lapply(centres, function(cc) {
    bi <- -1L; bd <- Inf
    for (i in seq_along(points)) {
      if (!(i %in% used) && lc_dist(points[[i]], cc) < bd - LC_TIE) { bd <- lc_dist(points[[i]], cc); bi <- i }
    }
    used <<- c(used, bi)
    points[[bi]]
  })
}

# Greedy hue placement: each new hue sits as far as possible from the others.
lc_spread_hues <- function(existing, n, start) {
  hues <- existing
  out <- numeric(0)
  for (i in seq_len(n)) {
    if (!length(hues)) { hues <- start; out <- start; next }
    bestH <- 0; bestD <- -1
    for (h in 0:359) {
      d <- min(vapply(hues, function(x) lc_hue_gap(x, h), 0))
      if (d > bestD + 1e-9) { bestD <- d; bestH <- h }
    }
    hues <- c(hues, bestH); out <- c(out, bestH)
  }
  out
}

lc_pole <- function(L, source) list(col = lc_grey(L), hex = lc_lab_to_hex(lc_grey(L)), source = source)


#' Build the stabilized core: 2 neutral poles + 5 colours
#'
#' The anchor (first non-pole input) keeps its exact hex and L. The other four
#' slots are distilled from or expanded around the input, then placed on an L
#' ladder that maximises the minimum lightness gap. Every decision is logged.
#'
#' @param input_hex Sanitized #RRGGBB vector (output of \code{get_input_vector}).
#' @param pivot Logical. Rotate two satellites when hues are jammed.
#' @return List with \code{raw_5} (hex), \code{hex_black}, \code{hex_white},
#'   \code{lab} (OKLab values), \code{min_gap}, \code{hue_variance}, \code{log}.
#' @export
get_raw_5 <- function(input_hex, pivot = TRUE) {
  log <- list()
  add <- function(law, text) log[[length(log) + 1L]] <<- c(law = law, text = text)
  lab <- lapply(input_hex, lc_hex_to_lab)

  # -- 1. Poles (the horizon), always neutral ----------------------------------
  Lb0 <- lc_hex_to_lab(lc_black)[["L"]]; Lw0 <- lc_hex_to_lab(lc_white)[["L"]]
  black <- lc_pole(Lb0, "default"); white <- lc_pole(Lw0, "default")
  signal <- list(); signal_hex <- character(0)
  for (i in seq_along(lab)) {
    p <- lab[[i]]; hx <- input_hex[i]
    achromatic <- p[["c"]] < LC_POLE_CHROMA
    if (achromatic && p[["L"]] <= Lb0 + LC_POLE_ZONE && black$source == "default") {
      black <- lc_pole(p[["L"]], hx)
      add("pole", sprintf("%s is near-black and neutral: it becomes the dark pole.", hx))
    } else if (achromatic && p[["L"]] >= Lw0 - LC_POLE_ZONE && white$source == "default") {
      white <- lc_pole(p[["L"]], hx)
      add("pole", sprintf("%s is near-white and neutral: it becomes the light pole.", hx))
    } else if (achromatic && (p[["L"]] <= Lb0 + LC_POLE_ZONE || p[["L"]] >= Lw0 - LC_POLE_ZONE)) {
      add("pole", sprintf("%s sits in a pole zone already taken: dropped.", hx))
    } else {
      signal[[length(signal) + 1L]] <- p; signal_hex <- c(signal_hex, hx)
    }
  }
  for (i in seq_along(lab)) {
    p <- lab[[i]]
    if (p[["c"]] >= LC_POLE_CHROMA && (p[["L"]] <= Lb0 + LC_POLE_ZONE || p[["L"]] >= Lw0 - LC_POLE_ZONE))
      add("pole", sprintf("%s is very %s but has colour (chroma %.2f): kept as a colour, not a pole.",
                          input_hex[i], if (p[["L"]] < 0.5) "dark" else "light", p[["c"]]))
  }

  # -- 2. Anchor -----------------------------------------------------------------
  if (length(signal)) {
    anchor <- signal[[1L]]; anchor_hex <- signal_hex[1L]; anchor_origin <- "input"
    if (anchor_hex != input_hex[1L])
      add("anchor", sprintf("%s became a pole, so %s is the anchor.", input_hex[1L], anchor_hex))
  } else {
    anchor <- lc_grey((black$col[["L"]] + white$col[["L"]]) / 2)
    anchor_hex <- lc_lab_to_hex(anchor); anchor_origin <- "synthetic"
    add("anchor", sprintf("The input is only poles: a neutral mid-grey %s anchors the system.", anchor_hex))
  }

  # -- 3. Distil or expand to 4 satellites --------------------------------------
  sats <- signal[-1L]
  n_in <- length(signal)
  if (length(sats) > 4L) {
    sats <- lc_distil(sats, 4L, anchor)
    add("distil", sprintf("%d colours distilled to 5: the anchor plus 4 cluster representatives (real input colours).", n_in))
  }
  if (length(sats) < 4L) {
    missing <- 4L - length(sats)
    chromatic <- Filter(function(p) p[["c"]] >= LC_VOID_CHROMA, c(list(anchor), sats))
    seed_c <- if (length(chromatic)) lc_sum(vapply(chromatic, `[[`, 0, "c")) / length(chromatic) else LC_VOID_SEED_C
    start_hue <- if (anchor[["c"]] >= LC_VOID_CHROMA) (anchor[["h"]] + 180) %% 360 else LC_VOID_HUE
    hues <- lc_spread_hues(vapply(chromatic, `[[`, 0, "h"), missing, start_hue)
    for (h in hues) sats[[length(sats) + 1L]] <- lc_from_lch(0.5, seed_c, h)
    nn <- if (n_in == 0L) 1L else n_in
    add("expand", sprintf("%d colour%s expanded to 5: %d new hue%s placed as far as possible from the others (chroma %.2f).",
                          nn, if (n_in == 1L) "" else "s", missing, if (missing == 1L) "" else "s", seed_c))
  }

  # -- 4. Hue pivot (traffic jam) -------------------------------------------------
  chroma_hues <- vapply(Filter(function(p) p[["c"]] >= LC_VOID_CHROMA, c(list(anchor), sats)), `[[`, 0, "h")
  cv <- lc_circular_variance(chroma_hues)
  sats <- sats[order(vapply(sats, `[[`, 0, "L"))]
  if (length(chroma_hues) >= 3L && cv < LC_JAM_VAR) {
    if (pivot) {
      for (pv in list(c(2, LC_PIVOT_DEG), c(4, -LC_PIVOT_DEG))) {
        s <- sats[[pv[1]]]
        if (s[["c"]] >= LC_VOID_CHROMA)
          sats[[pv[1]]] <- lc_from_lch(s[["L"]], s[["c"]], (s[["h"]] + pv[2] + 360) %% 360)
      }
      add("pivot", sprintf("Hues are jammed (circular variance %.3f): two satellites rotated \u00b1%d\u00b0.", cv, LC_PIVOT_DEG))
    } else {
      add("pivot", sprintf("Hues are jammed (circular variance %.3f) and the pivot is off: monochrome system.", cv))
    }
  }

  # -- 5. L ladder (luminance is the backbone) -----------------------------------
  target <- (Lw0 - Lb0) / 6
  La <- anchor[["L"]]
  if (white$col[["L"]] - La < target) {
    nl <- min(1, La + target)
    if (nl > white$col[["L"]]) {
      white <- lc_pole(nl, white$source)
      add("ladder", sprintf("The anchor is very light: the light pole moves up to L %.2f to make room.", nl))
    }
  }
  if (La - black$col[["L"]] < target) {
    nl <- max(0, La - target)
    if (nl < black$col[["L"]]) {
      black <- lc_pole(nl, black$source)
      add("ladder", sprintf("The anchor is very dark: the dark pole moves down to L %.2f to make room.", nl))
    }
  }
  Lb <- black$col[["L"]]; Lw <- white$col[["L"]]
  best_k <- 0L; best_min <- -1
  for (k in 0:4) {
    m <- min((La - Lb) / (k + 1), (Lw - La) / (5 - k))
    if (m > best_min + 1e-12) { best_min <- m; best_k <- k }
  }
  gb <- (La - Lb) / (best_k + 1); ga <- (Lw - La) / (5 - best_k)
  slots <- c(if (best_k > 0) Lb + seq_len(best_k) * gb, if (best_k < 4) La + seq_len(4 - best_k) * ga)
  clipped <- 0L
  core <- lapply(seq_along(sats), function(i) {
    s <- sats[[i]]
    want <- if (s[["c"]] < LC_VOID_CHROMA) lc_grey(slots[i]) else lc_from_lch(slots[i], s[["c"]], s[["h"]])
    m <- lc_map_gamut(want)
    if (m$clipped) clipped <<- clipped + 1L
    m$col
  })
  add("ladder", sprintf("L ladder: %d satellite%s below the anchor and %d above. Minimum gap %.3f.",
                        best_k, if (best_k == 1L) "" else "s", 4L - best_k, best_min))
  if (clipped) add("gamut", sprintf("%d satellite%s lost chroma to fit sRGB at their L (L and hue kept).",
                                    clipped, if (clipped == 1L) "" else "s"))

  raw5 <- c(list(lc_polar(anchor[["L"]], anchor[["a"]], anchor[["b"]])), core)
  raw5_hex <- c(anchor_hex, vapply(core, lc_lab_to_hex, ""))
  levels <- sort(c(black$col[["L"]], vapply(raw5, `[[`, 0, "L"), white$col[["L"]]))

  list(
    raw_5        = raw5_hex,
    hex_black    = black$hex,
    hex_white    = white$hex,
    pole_source  = c(black = black$source, white = white$source),
    anchor_origin = anchor_origin,
    lab          = list(raw5 = raw5, black = black$col, white = white$col),
    min_gap      = min(diff(levels)),
    hue_variance = cv,
    log          = data.frame(law  = vapply(log, `[[`, "", "law"),
                              text = vapply(log, `[[`, "", "text"),
                              stringsAsFactors = FALSE)
  )
}
