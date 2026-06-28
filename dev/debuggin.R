# ==============================================================================
# dev/debug.R
# loscolores — Bootstrapping + Regression Suite + Visual Inspection
#
# Corre desde la raíz del paquete (sin instalar):
#   source("dev/debug.R")
#
# Produce:
#   dev/plots/P1_oklab_roundtrip.png
#   dev/plots/P2_raw5_cases.png
#   dev/plots/P3_L_dictator.png
#   dev/plots/P4_palettes_topology.png
#   dev/plots/P5_sysdata_sample.png
#   dev/plots/P6_greyscale_audit.png
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(cli)
  library(farver)
})

cli_h1("loscolores | dev/debug.R")

# ── 0. BOOTSTRAPPING ──────────────────────────────────────────────────────────
out_dir <- "dev/plots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
cli_alert_info("Plots → {.file {out_dir}/}")

# Source all R files in dependency order
for (f in c("utils_color.R", "engine_core.R", "engine_expansion.R",
            "methods_s3.R", "scales.R", "theme.R", "zzz.R")) {
  source(file.path("R", f))
}
load("R/sysdata.rda")
.onLoad(libname = "", pkgname = "loscolores")
cli_alert_success("Package sourced & bootstrapped")

# ── SAVE HELPER ───────────────────────────────────────────────────────────────
save_plot <- function(p, filename, w = 14, h = 10) {
  path <- file.path(out_dir, filename)
  ggsave(path, plot = p, width = w, height = h, dpi = 150, bg = "white")
  cli_alert_success("Saved: {.file {path}}")
}

# ── TEST HARNESS ──────────────────────────────────────────────────────────────
.lc_pass <- 0L
.lc_fail <- 0L
.lc_log  <- character(0L)

lc_expect <- function(label, expr, condition_fn) {
  result <- tryCatch(expr, error = function(e) structure(e, class = "lc_error"))
  if (inherits(result, "lc_error")) {
    .lc_fail <<- .lc_fail + 1L
    msg <- sprintf("  FAIL [%s] — error: %s", label, conditionMessage(result))
    .lc_log  <<- c(.lc_log, msg)
    message(msg); return(invisible(NULL))
  }
  ok <- tryCatch(isTRUE(condition_fn(result)), error = function(e) FALSE)
  if (ok) {
    .lc_pass <<- .lc_pass + 1L
    message(sprintf("  pass [%s]", label))
  } else {
    .lc_fail <<- .lc_fail + 1L
    msg <- sprintf("  FAIL [%s] — got: %s",
                   label, paste(capture.output(str(result)), collapse = " "))
    .lc_log  <<- c(.lc_log, msg)
    message(msg)
  }
  invisible(result)
}

lc_expect_error <- function(label, expr) {
  caught <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  if (caught) {
    .lc_pass <<- .lc_pass + 1L
    message(sprintf("  pass [%s] (expected error caught)", label))
  } else {
    .lc_fail <<- .lc_fail + 1L
    msg <- sprintf("  FAIL [%s] — expected error but none thrown", label)
    .lc_log  <<- c(.lc_log, msg)
    message(msg)
  }
}

section <- function(title) {
  message("\n", strrep("─", 60))
  message(title)
  message(strrep("─", 60))
}

# ── Shared helpers ────────────────────────────────────────────────────────────
is_hex_vector <- function(x, n = NULL) {
  ok <- is.character(x) && all(grepl("^#[0-9A-Fa-f]{6}$", x))
  if (!is.null(n)) ok <- ok && length(x) == n
  ok
}
L_values <- function(hex) to_oklab(hex)$L

# Minimal tile strip for palette visualization
pal_strip <- function(hex_vec, label = "", col_bg = "white") {
  n <- length(hex_vec)
  df <- data.frame(x = seq_len(n), hex = hex_vec, stringsAsFactors = FALSE)
  ggplot(df, aes(x = x, y = 1, fill = hex)) +
    geom_tile(color = col_bg, linewidth = 0.8) +
    scale_fill_identity() +
    theme_void() +
    theme(plot.title = element_text(size = 8, hjust = 0, face = "bold",
                                    margin = margin(b = 2))) +
    labs(title = label)
}

# ==============================================================================
# SECTION 1 — get_input_vector
# ==============================================================================
section("1. get_input_vector — La Aduana")

lc_expect("single valid HEX",
  get_input_vector("#FF5733"), function(r) is_hex_vector(r, 1L))

lc_expect("R color name 'red'",
  get_input_vector("red"),
  function(r) is_hex_vector(r, 1L) && toupper(r) == "#FF0000")

lc_expect("R color name 'steelblue'",
  get_input_vector("steelblue"), function(r) is_hex_vector(r, 1L))

lc_expect("8-char HEX alpha stripped",
  get_input_vector("#FF573380"),
  function(r) is_hex_vector(r, 1L) && r == "#FF5733")

lc_expect("duplicates removed",
  get_input_vector(c("#FF5733", "#FF5733", "#1A1A2E")),
  function(r) length(r) == 2L)

lc_expect("NA elements dropped",
  get_input_vector(c("#FF5733", NA, "#1A1A2E")),
  function(r) is_hex_vector(r, 2L))

lc_expect("invalid color name dropped silently",
  get_input_vector(c("#FF5733", "notacolor")),
  function(r) is_hex_vector(r, 1L))

lc_expect_error("all-NA input",   get_input_vector(c(NA, NA)))
lc_expect_error("empty vector",   get_input_vector(character(0L)))
lc_expect_error("garbage string", get_input_vector("ZZZZZZ"))

# ==============================================================================
# SECTION 2 — to_oklab / to_hex
# ==============================================================================
section("2. to_oklab / to_hex — round-trip fidelity")

test_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFFFF", "#000000",
                 "#F4F3F3", "#1D1C1B", "#8A7F8D")

lc_expect("to_oklab: 5-column df",
  to_oklab(test_colors),
  function(r) is.data.frame(r) && ncol(r) == 5L && nrow(r) == length(test_colors))

lc_expect("Oklab L in [0,1]",
  to_oklab(test_colors)$L, function(r) all(r >= 0 & r <= 1))

lc_expect("chroma >= 0",
  to_oklab(test_colors)$c, function(r) all(r >= 0))

lc_expect("hue in [0,360)",
  to_oklab(test_colors)$h, function(r) all(r >= 0 & r < 360))

lc_expect("round-trip max L error < 0.01", {
  lab  <- to_oklab(test_colors)
  lab2 <- to_oklab(to_hex(lab$L, lab$a, lab$b))
  max(abs(lab2$L - lab$L))
}, function(r) r < 0.01)

lc_expect("L clamp: L > 1 ok", to_hex(1.5, 0, 0), function(r) is_hex_vector(r, 1L))
lc_expect("L clamp: L < 0 ok", to_hex(-0.5, 0, 0), function(r) is_hex_vector(r, 1L))

# ── P1: Oklab round-trip scatter ─────────────────────────────────────────────
{
  lab      <- to_oklab(test_colors)
  lab_rt   <- to_oklab(to_hex(lab$L, lab$a, lab$b))
  df_rt    <- data.frame(
    color    = test_colors,
    L_in     = lab$L,    L_out  = lab_rt$L,
    a_in     = lab$a,    a_out  = lab_rt$a,
    b_in     = lab$b,    b_out  = lab_rt$b,
    c_in     = lab$c,    h_in   = lab$h
  )

  p_L <- ggplot(df_rt, aes(x = L_in, y = L_out, color = color)) +
    geom_abline(linetype = "dashed", color = "grey70") +
    geom_point(size = 4) +
    scale_color_identity() +
    labs(title = "L round-trip", x = "L input", y = "L recovered") +
    theme_minimal(base_size = 10)

  p_ab <- ggplot(df_rt, aes(x = a_in, y = b_in, color = color)) +
    geom_point(size = 4) +
    geom_point(aes(x = a_out, y = b_out), shape = 1, size = 5) +
    scale_color_identity() +
    labs(title = "a/b plane (filled=input, open=recovered)",
         x = "a", y = "b") +
    coord_equal() +
    theme_minimal(base_size = 10)

  p_chroma <- ggplot(df_rt, aes(x = h_in, y = c_in, color = color)) +
    geom_point(size = 4) +
    scale_color_identity() +
    labs(title = "Chroma vs Hue (input)", x = "Hue °", y = "Chroma") +
    theme_minimal(base_size = 10)

  # Greyscale projections of test colors
  grey_hex  <- to_hex(lab$L, 0, 0)
  df_grey   <- data.frame(
    x     = seq_along(test_colors),
    color = test_colors,
    grey  = grey_hex,
    L     = lab$L
  )
  p_grey_strip <- ggplot(df_grey) +
    geom_tile(aes(x = x, y = 1.5, fill = color), height = 0.8, color = "white") +
    geom_tile(aes(x = x, y = 0.5, fill = grey),  height = 0.8, color = "white") +
    geom_text(aes(x = x, y = 0.5, label = round(L, 2)), size = 2.5,
              color = ifelse(df_grey$L > 0.5, "#1D1C1B", "#F4F3F3")) +
    scale_fill_identity() +
    scale_y_continuous(breaks = c(0.5, 1.5), labels = c("Grey proj.", "Original")) +
    theme_void(base_size = 9) +
    theme(axis.text.y = element_text(hjust = 1, size = 8)) +
    labs(title = "Greyscale projection of test colors (with L value)")

  p1 <- (p_L | p_ab | p_chroma) / p_grey_strip +
    plot_layout(heights = c(2, 1)) +
    plot_annotation(title = "P1 — Oklab encoding & round-trip fidelity",
                    theme = theme(plot.title = element_text(face = "bold")))

  save_plot(p1, "P1_oklab_roundtrip.png", w = 14, h = 8)
}

# ==============================================================================
# SECTION 3 — get_raw_5 / L-Dictator
# ==============================================================================
section("3. get_raw_5 — Destilación y L-Dictator")

lc_expect("single color → 5 valid HEX",
  get_raw_5(get_input_vector("#3A86FF"))$raw_5, function(r) is_hex_vector(r, 5L))

lc_expect("5 colors → 5 valid HEX",
  get_raw_5(c("#3A86FF","#FF006E","#FFBE0B","#FB5607","#8338EC"))$raw_5,
  function(r) is_hex_vector(r, 5L))

lc_expect("L gaps >= 0.12 (single color)", {
  min(diff(sort(L_values(get_raw_5(get_input_vector("#3A86FF"))$raw_5))))
}, function(r) r >= 0.12)

lc_expect("anchor[1] is valid HEX",
  get_raw_5(get_input_vector("#3A86FF"))$raw_5[1L],
  function(r) is_hex_vector(r, 1L))

lc_expect("2 colors → 5 HEX (radial expansion)",
  get_raw_5(c("#3A86FF","#FF006E"))$raw_5, function(r) is_hex_vector(r, 5L))

lc_expect("3 colors → L gaps >= 0.12", {
  min(diff(sort(L_values(get_raw_5(c("#3A86FF","#FF006E","#FFBE0B"))$raw_5))))
}, function(r) r >= 0.12)

lc_expect("8 colors → exactly 5 via k-means",
  get_raw_5(c("#3A86FF","#FF006E","#FFBE0B","#FB5607",
              "#8338EC","#06D6A0","#E63946","#457B9D"))$raw_5,
  function(r) is_hex_vector(r, 5L))

lc_expect("8 colors → L gaps >= 0.12", {
  min(diff(sort(L_values(
    get_raw_5(c("#3A86FF","#FF006E","#FFBE0B","#FB5607",
                "#8338EC","#06D6A0","#E63946","#457B9D"))$raw_5
  ))))
}, function(r) r >= 0.12)

lc_expect("dark color hijacks black pole",
  get_raw_5(c("#0A0908","#3A86FF"))$hex_black != "#1D1C1B",
  function(r) isTRUE(r))

lc_expect("light color hijacks white pole",
  get_raw_5(c("#F8F8F8","#3A86FF"))$hex_white != "#F4F3F3",
  function(r) isTRUE(r))

lc_expect("all-black-zone → 5 HEX",
  get_raw_5(c("#000000","#0A0908","#111111"))$raw_5,
  function(r) is_hex_vector(r, 5L))

lc_expect("all-white-zone → 5 HEX",
  get_raw_5(c("#FFFFFF","#F5F5F5","#FEFEFE"))$raw_5,
  function(r) is_hex_vector(r, 5L))

lc_expect("5 identical colors: L gaps >= 0.12", {
  min(diff(sort(L_values(get_raw_5(rep("#3A86FF", 5L))$raw_5))))
}, function(r) r >= 0.12)

lc_expect("grey anchor cold-start → 5 HEX",
  get_raw_5(get_input_vector("#808080"))$raw_5, function(r) is_hex_vector(r, 5L))

lc_expect("neon green → 5 HEX",
  get_raw_5(get_input_vector("#00FF00"))$raw_5, function(r) is_hex_vector(r, 5L))

lc_expect("pure red → 5 HEX",
  get_raw_5(get_input_vector("#FF0000"))$raw_5, function(r) is_hex_vector(r, 5L))

lc_expect("dark chromatic #1A0033 → L gaps >= 0.12", {
  min(diff(sort(L_values(get_raw_5(get_input_vector("#1A0033"))$raw_5))))
}, function(r) r >= 0.12)

lc_expect("anchor L in safe zone", {
  res <- get_raw_5(get_input_vector("#3A86FF"))
  L_anc <- L_values(res$raw_5[1L])
  L_b   <- to_oklab(res$hex_black)$L + 0.08
  L_w   <- to_oklab(res$hex_white)$L - 0.08
  L_anc >= L_b && L_anc <= L_w
}, function(r) isTRUE(r))

lc_expect("anchor L = clamp(input_L, safe_zone) only", {
  inp    <- "#3A86FF"
  res    <- get_raw_5(get_input_vector(inp))
  L_in   <- to_oklab(inp)$L
  L_b    <- to_oklab(res$hex_black)$L + 0.08
  L_w    <- to_oklab(res$hex_white)$L - 0.08
  exp_L  <- pmax(L_b, pmin(L_w, L_in))
  abs(L_values(res$raw_5[1L]) - exp_L) < 0.005
}, function(r) isTRUE(r))

# ── P2: edge case raw_5 grid ──────────────────────────────────────────────────
{
  cases <- list(
    "Single: #3A86FF"          = get_raw_5(get_input_vector("#3A86FF")),
    "2 colors (radial)"        = get_raw_5(c("#3A86FF","#FF006E")),
    "3 colors (radial)"        = get_raw_5(c("#3A86FF","#FF006E","#FFBE0B")),
    "5 colors (exact)"         = get_raw_5(c("#3A86FF","#FF006E","#FFBE0B","#FB5607","#8338EC")),
    "8 colors (k-means)"       = get_raw_5(c("#3A86FF","#FF006E","#FFBE0B","#FB5607",
                                              "#8338EC","#06D6A0","#E63946","#457B9D")),
    "5 identical (traffic jam)"= get_raw_5(rep("#3A86FF", 5L)),
    "Grey anchor (cold start)" = get_raw_5(get_input_vector("#808080")),
    "Dark chromatic #1A0033"   = get_raw_5(get_input_vector("#1A0033")),
    "Neon green #00FF00"       = get_raw_5(get_input_vector("#00FF00")),
    "Dark pole hijack"         = get_raw_5(c("#0A0908","#3A86FF")),
    "White pole hijack"        = get_raw_5(c("#F8F8F8","#3A86FF")),
    "All-black zone"           = get_raw_5(c("#000000","#0A0908","#111111")),
    "All-white zone"           = get_raw_5(c("#FFFFFF","#F5F5F5","#FEFEFE"))
  )

  # For each case build: raw_7 row (white, raw_5, black) + grey projection
  df_p2 <- bind_rows(lapply(seq_along(cases), function(i) {
    nm  <- names(cases)[i]
    res <- cases[[i]]
    raw7 <- c(res$hex_white, res$raw_5, res$hex_black)
    grey <- to_hex(to_oklab(raw7)$L, 0, 0)
    bind_rows(
      data.frame(case = nm, row = "color", pos = 1:7, hex = raw7,
                 L = to_oklab(raw7)$L, idx = i),
      data.frame(case = nm, row = "grey",  pos = 1:7, hex = grey,
                 L = to_oklab(grey)$L,  idx = i)
    )
  }))

  df_p2$case <- factor(df_p2$case, levels = rev(names(cases)))
  df_p2$row  <- factor(df_p2$row,  levels = c("grey","color"))

  p2 <- ggplot(df_p2, aes(x = pos, y = interaction(row, case, sep = " | "),
                           fill = hex)) +
    geom_tile(color = "white", linewidth = 0.6) +
    scale_fill_identity() +
    scale_x_continuous(breaks = 1:7,
                       labels = c("W", "C1*", "C2", "C3", "C4", "C5", "B")) +
    theme_void(base_size = 8) +
    theme(
      axis.text.x = element_text(size = 7, margin = margin(t = 3)),
      axis.text.y = element_text(hjust = 1, size = 7, margin = margin(r = 4)),
      plot.title  = element_text(face = "bold", size = 11, margin = margin(b = 8))
    ) +
    labs(title = "P2 — Raw 5 edge cases (colour + greyscale projection)\n* C1 = anchor")

  save_plot(p2, "P2_raw5_cases.png", w = 11, h = 14)
}

# ── P3: L-Dictator gap audit ──────────────────────────────────────────────────
{
  # Run 200 random sysdata palettes through get_raw_5 and collect L gap stats
  set.seed(42L)
  pal_keys <- sample(names(paletas), min(200L, length(paletas)))

  gap_data <- lapply(pal_keys, function(k) {
    tryCatch({
      res  <- get_raw_5(paletas[[k]]$hexcolors)
      Ls   <- sort(L_values(res$raw_5))
      gaps <- diff(Ls)
      data.frame(
        key      = k,
        min_gap  = min(gaps),
        max_gap  = max(gaps),
        mean_gap = mean(gaps),
        L_anchor = L_values(res$raw_5[1L]),
        n_input  = length(paletas[[k]]$hexcolors)
      )
    }, error = function(e) NULL)
  })
  gap_df <- bind_rows(gap_data)

  p3a <- ggplot(gap_df, aes(x = min_gap)) +
    geom_histogram(binwidth = 0.01, fill = "#3A86FF", color = "white") +
    geom_vline(xintercept = 0.12, color = "#E63946", linetype = "dashed") +
    annotate("text", x = 0.13, y = Inf, vjust = 1.5, hjust = 0,
             label = "threshold 0.12", size = 3, color = "#E63946") +
    labs(title = "Min L gap distribution (n=200 sysdata palettes)",
         x = "min gap", y = "count") +
    theme_minimal(base_size = 10)

  p3b <- ggplot(gap_df, aes(x = L_anchor)) +
    geom_histogram(binwidth = 0.02, fill = "#FF006E", color = "white") +
    labs(title = "Anchor L distribution after L-Dictator",
         x = "L anchor (raw_5[1])", y = "count") +
    theme_minimal(base_size = 10)

  # Scatter: min gap vs n_input
  p3c <- ggplot(gap_df, aes(x = n_input, y = min_gap, color = min_gap < 0.12)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    geom_hline(yintercept = 0.12, color = "#E63946", linetype = "dashed") +
    scale_color_manual(values = c("FALSE" = "#3A86FF", "TRUE" = "#E63946"),
                       name   = "Below threshold") +
    labs(title = "Min gap vs input size",
         x = "n input colors", y = "min L gap") +
    theme_minimal(base_size = 10)

  n_fail <- sum(gap_df$min_gap < 0.12)
  p3d <- ggplot(gap_df, aes(x = mean_gap, y = min_gap)) +
    geom_point(alpha = 0.5, color = "#8338EC") +
    geom_hline(yintercept = 0.12, color = "#E63946", linetype = "dashed") +
    annotate("text", x = min(gap_df$mean_gap), y = 0.115, hjust = 0,
             label = sprintf("%d / %d palettes below threshold", n_fail, nrow(gap_df)),
             size = 3, color = "#E63946") +
    labs(title = "Mean vs min gap", x = "mean gap", y = "min gap") +
    theme_minimal(base_size = 10)

  p3 <- (p3a | p3b) / (p3c | p3d) +
    plot_annotation(title = "P3 — L-Dictator audit across 200 sysdata palettes",
                    theme = theme(plot.title = element_text(face = "bold")))

  save_plot(p3, "P3_L_dictator.png", w = 13, h = 9)
}

# ==============================================================================
# SECTION 4 — get_expansion_params / sample_system
# ==============================================================================
section("4. get_expansion_params / sample_system")

.sys_single <- loscolores_system("#3A86FF")
.sys_multi  <- loscolores_system(c("#3A86FF","#FF006E","#FFBE0B","#FB5607","#8338EC"))

lc_expect("expansion slots present",
  names(.sys_single$expansion),
  function(r) all(c("categorical","sequential","divergent") %in% r))

lc_expect("categorical has A–D",
  names(.sys_single$expansion$categorical),
  function(r) all(paste0("Option_", LETTERS[1:4]) %in% r))

lc_expect("sequential has A–E",
  names(.sys_single$expansion$sequential),
  function(r) all(paste0("Option_", LETTERS[1:5]) %in% r))

lc_expect("divergent has A–E",
  names(.sys_single$expansion$divergent),
  function(r) all(paste0("Option_", LETTERS[1:5]) %in% r))

for (.opt in paste0("Option_", LETTERS[1:4])) {
  local({ opt <- .opt
    lc_expect(sprintf("categorical %s n=7 → 7", opt),
      sample_system(.sys_single, "categorical", opt, n = 7L),
      function(r) is_hex_vector(r, 7L))
  })
}
for (.opt in paste0("Option_", LETTERS[1:5])) {
  local({ opt <- .opt
    lc_expect(sprintf("sequential %s n=100 → 100", opt),
      sample_system(.sys_single, "sequential", opt, n = 100L),
      function(r) is_hex_vector(r, 100L))
    lc_expect(sprintf("divergent %s n=11 → 11", opt),
      sample_system(.sys_single, "divergent", opt, n = 11L),
      function(r) is_hex_vector(r, 11L))
  })
}

lc_expect("sequential: last > first (L)", {
  r <- sample_system(.sys_single, "sequential", "Option_A", n = 20L)
  L_values(r[20L]) > L_values(r[1L])
}, function(r) isTRUE(r))

lc_expect("sequential: last L >= 0.80", {
  L_values(sample_system(.sys_single, "sequential", "Option_A", n = 20L)[20L])
}, function(r) r >= 0.80)

lc_expect("divergent white: mid_L > both ends", {
  r  <- sample_system(.sys_single, "divergent", "Option_A", center = "white", n = 101L)
  L_values(r[51L]) > L_values(r[1L]) && L_values(r[51L]) > L_values(r[101L])
}, function(r) isTRUE(r))

lc_expect("divergent black: mid_L < at least one end", {
  r  <- sample_system(.sys_single, "divergent", "Option_A", center = "black", n = 101L)
  L_values(r[51L]) < L_values(r[1L]) || L_values(r[51L]) < L_values(r[101L])
}, function(r) isTRUE(r))

lc_expect("categorical greyscale: max chroma < 0.02", {
  max(to_oklab(sample_system(.sys_single,"categorical","Option_A",5L,grayscale=TRUE))$c)
}, function(r) r < 0.02)

lc_expect("sequential greyscale: max chroma < 0.02", {
  max(to_oklab(sample_system(.sys_single,"sequential","Option_A",20L,grayscale=TRUE))$c)
}, function(r) r < 0.02)

lc_expect("categorical A slot 1 == raw_5[1]", {
  sample_system(.sys_single,"categorical","Option_A",5L)[1L] == .sys_single$raw_5[1L]
}, function(r) isTRUE(r))

lc_expect("categorical n=1", sample_system(.sys_single,"categorical","Option_A",1L),
  function(r) is_hex_vector(r, 1L))
lc_expect("sequential n=1", sample_system(.sys_single,"sequential","Option_A",1L),
  function(r) is_hex_vector(r, 1L))
lc_expect("divergent n=1",  sample_system(.sys_single,"divergent","Option_A",1L),
  function(r) is_hex_vector(r, 1L))

lc_expect_error("bad option key",
  sample_system(.sys_single, "categorical", "Option_Z", 5L))

# ── P4: full topology panel for two systems ───────────────────────────────────
{
  make_topology_panel <- function(sys, title_prefix) {
    opts_cat <- paste0("Option_", LETTERS[1:4])
    opts_all <- paste0("Option_", LETTERS[1:5])

    # Block A: Raw 7 + greyscale
    raw7  <- c(sys$polos$white, sys$raw_5, sys$polos$black)
    grey7 <- to_hex(to_oklab(raw7)$L, 0, 0)
    dfA   <- bind_rows(
      data.frame(row = "Input",   pos = seq(1,7,l=length(sys$input)), hex = sys$input),
      data.frame(row = "Raw 7",   pos = 1:7, hex = raw7),
      data.frame(row = "Grey",    pos = 1:7, hex = grey7)
    )
    dfA$row <- factor(dfA$row, levels = rev(c("Input","Raw 7","Grey")))
    pA <- ggplot(dfA, aes(x = pos, y = row, fill = hex)) +
      geom_tile(color = "white", linewidth = 0.8, width = 0.93, height = 0.8) +
      scale_fill_identity() +
      scale_x_continuous(limits = c(0.5,7.5), breaks = 1:7,
                         labels = c("W","C1","C2","C3","C4","C5","B")) +
      theme_void(base_size = 8) +
      theme(axis.text.y = element_text(hjust=1, size=8, margin=margin(r=5)),
            axis.text.x = element_text(size=7),
            plot.title  = element_text(face="bold", size=9)) +
      labs(title = "Signal integration")

    # Block B1: Sequential
    dfB1 <- bind_rows(lapply(opts_all, function(opt) {
      data.frame(opt = sub("Option_","",opt), x = 1:100,
                 hex = sample_system(sys,"sequential",opt,100L))
    }))
    dfB1$opt <- factor(dfB1$opt, levels = rev(sub("Option_","",opts_all)))
    pB1 <- ggplot(dfB1, aes(y = opt, x = x, fill = hex)) +
      geom_raster() + scale_fill_identity() +
      theme_void(base_size = 8) +
      theme(axis.text.y = element_text(size=7, margin=margin(r=4)),
            plot.title  = element_text(face="bold", size=9)) +
      labs(title = "Sequential (A–E)")

    # Block B2: Divergent white center
    dfB2 <- bind_rows(lapply(opts_all, function(opt) {
      data.frame(opt = sub("Option_","",opt), x = 1:100,
                 hex = sample_system(sys,"divergent",opt,100L,center="white"))
    }))
    dfB2$opt <- factor(dfB2$opt, levels = rev(sub("Option_","",opts_all)))
    pB2 <- ggplot(dfB2, aes(y = opt, x = x, fill = hex)) +
      geom_raster() + scale_fill_identity() +
      theme_void(base_size = 8) +
      theme(axis.text.y = element_text(size=7, margin=margin(r=4)),
            plot.title  = element_text(face="bold", size=9)) +
      labs(title = "Divergent white (A–E)")

    # Block C: Categorical N=10
    dfC <- bind_rows(lapply(opts_cat, function(opt) {
      data.frame(opt = sub("Option_","",opt), x = 1:10,
                 hex = sample_system(sys,"categorical",opt,10L))
    }))
    dfC$opt <- factor(dfC$opt, levels = rev(sub("Option_","",opts_cat)))
    pC <- ggplot(dfC, aes(x = x, y = opt, fill = hex)) +
      geom_tile(color = "white", linewidth = 0.8) +
      scale_fill_identity() +
      theme_void(base_size = 8) +
      theme(axis.text.y = element_text(hjust=1, size=7, margin=margin(r=5)),
            plot.title  = element_text(face="bold", size=9)) +
      labs(title = "Categorical N=10 (A–D)")

    (pA / (pB1 | pB2) / pC) +
      plot_layout(heights = c(1.2, 1, 1)) +
      plot_annotation(title = sprintf("%s | %s → raw_5[1]=%s",
                                      title_prefix, paste(sys$input, collapse=" "),
                                      sys$raw_5[1]),
                      theme = theme(plot.title = element_text(face="bold", size=10)))
  }

  p4a <- make_topology_panel(.sys_single, "Single anchor")
  p4b <- make_topology_panel(.sys_multi,  "Multi anchor")

  save_plot(p4a, "P4a_topology_single.png", w = 13, h = 10)
  save_plot(p4b, "P4b_topology_multi.png",  w = 13, h = 10)
}

# ==============================================================================
# SECTION 5 — loscolores_system full pipeline
# ==============================================================================
section("5. loscolores_system — full pipeline")

lc_expect("single HEX → lc_system",
  loscolores_system("#3A86FF"), function(r) inherits(r, "lc_system"))

lc_expect("required slots present", {
  all(c("input","raw_5","polos","expansion") %in% names(loscolores_system("#3A86FF")))
}, function(r) isTRUE(r))

lc_expect("R name 'tomato' → lc_system",
  loscolores_system("tomato"), function(r) inherits(r, "lc_system"))

lc_expect("20 random colors → lc_system",
  loscolores_system(sample(grDevices::colors(), 20L)),
  function(r) inherits(r, "lc_system"))

lc_expect("monochromatic input → lc_system (hue pivot)",
  loscolores_system(c("#FF0000","#EE0000","#DD0000","#CC0000","#BB0000","#AA0000")),
  function(r) inherits(r, "lc_system"))

lc_expect("white+black only → lc_system (fallback anchor)",
  loscolores_system(c("#FFFFFF","#000000")),
  function(r) inherits(r, "lc_system"))

# ── P5: sysdata sample — 20 random palettes through the system ────────────────
{
  set.seed(7L)
  sample_keys <- sample(names(paletas), 20L)

  strips <- lapply(seq_along(sample_keys), function(i) {
    k   <- sample_keys[i]
    sys <- loscolores_system(paletas[[k]]$hexcolors)
    raw7  <- c(sys$polos$white, sys$raw_5, sys$polos$black)
    grey7 <- to_hex(to_oklab(raw7)$L, 0, 0)
    input_stretched <- paletas[[k]]$hexcolors

    df <- bind_rows(
      data.frame(row = "In",    pos = seq(1,7,l=length(input_stretched)),
                 hex = input_stretched),
      data.frame(row = "Color", pos = 1:7, hex = raw7),
      data.frame(row = "Grey",  pos = 1:7, hex = grey7)
    )
    df$row   <- factor(df$row, levels = c("Grey","Color","In"))
    df$panel <- sprintf("[%02d] %s", i, k)
    df
  })

  df_p5 <- bind_rows(strips)
  df_p5$panel <- factor(df_p5$panel, levels = unique(df_p5$panel))

  p5 <- ggplot(df_p5, aes(x = pos, y = row, fill = hex)) +
    geom_tile(color = "white", linewidth = 0.5, width = 0.93, height = 0.85) +
    scale_fill_identity() +
    scale_x_continuous(limits = c(0.5, 7.5), breaks = 1:7,
                       labels = c("W","1*","2","3","4","5","B")) +
    facet_wrap(~panel, ncol = 4) +
    theme_void(base_size = 7) +
    theme(
      strip.text  = element_text(size = 6.5, face = "bold", hjust = 0,
                                  margin = margin(b = 2)),
      axis.text.y = element_text(hjust = 1, size = 6, margin = margin(r = 3)),
      axis.text.x = element_text(size = 5.5),
      panel.spacing = unit(6, "pt"),
      plot.title  = element_text(face = "bold", size = 11,
                                  margin = margin(b = 10))
    ) +
    labs(title = "P5 — 20 random sysdata palettes through loscolores_system\n(In = original input, Color = raw7, Grey = greyscale projection | 1* = anchor)")

  save_plot(p5, "P5_sysdata_sample.png", w = 16, h = 14)
}

# ==============================================================================
# SECTION 6 — Session state
# ==============================================================================
section("6. Session state")

lc_expect("set.loscolores returns without error", {
  set.loscolores("#3A86FF"); TRUE
}, function(r) isTRUE(r))

lc_expect("get.loscolores → lc_system after set", {
  set.loscolores("#FF006E"); get.loscolores()
}, function(r) inherits(r, "lc_system"))

lc_expect("variadic set works", {
  set.loscolores("#3A86FF","#FF006E","#FFBE0B"); get.loscolores()
}, function(r) inherits(r, "lc_system"))

lc_expect("anchor L = clamp(input_L, safe_zone) only", {
  set.loscolores("#06D6A0")
  sys    <- get.loscolores()
  L_in   <- to_oklab("#06D6A0")$L
  L_b    <- to_oklab(sys$polos$black)$L + 0.08
  L_w    <- to_oklab(sys$polos$white)$L - 0.08
  exp_L  <- pmax(L_b, pmin(L_w, L_in))
  abs(to_oklab(sys$raw_5[1L])$L - exp_L) < 0.005
}, function(r) isTRUE(r))

# ── P6: greyscale audit — categorical palettes for 6 selected sysdata keys ───
{
  audit_keys <- c("metric_trails", "Zissou1", "Hokusai1", "batlow",
                  "VanGogh1", "GrandBudapest1")

  audit_rows <- bind_rows(lapply(audit_keys, function(k) {
    sys  <- loscolores_system(paletas[[k]]$hexcolors)
    opts <- paste0("Option_", LETTERS[1:4])
    bind_rows(lapply(opts, function(opt) {
      pal  <- sample_system(sys, "categorical", opt, n = 7L)
      grey <- sample_system(sys, "categorical", opt, n = 7L, grayscale = TRUE)
      bind_rows(
        data.frame(key=k, opt=opt, mode="color", x=1:7, hex=pal),
        data.frame(key=k, opt=opt, mode="grey",  x=1:7, hex=grey)
      )
    }))
  }))

  audit_rows$label <- paste0(audit_rows$key, " | ", audit_rows$opt)
  audit_rows$mode  <- factor(audit_rows$mode, levels = c("grey","color"))
  audit_rows$label <- factor(audit_rows$label, levels = rev(unique(audit_rows$label)))

  p6 <- ggplot(audit_rows, aes(x = x, y = mode, fill = hex)) +
    geom_tile(color = "white", linewidth = 0.6) +
    scale_fill_identity() +
    facet_wrap(~label, ncol = 4) +
    theme_void(base_size = 7.5) +
    theme(
      strip.text    = element_text(size = 6.5, face = "bold", hjust = 0,
                                   margin = margin(b = 2)),
      axis.text.y   = element_text(size = 6.5, hjust = 1, margin = margin(r = 3)),
      panel.spacing = unit(5, "pt"),
      plot.title    = element_text(face = "bold", size = 11,
                                   margin = margin(b = 8))
    ) +
    labs(title = "P6 — Greyscale audit: categorical Options A–D for 6 sysdata palettes\n(each panel: color row top, greyscale row bottom)")

  save_plot(p6, "P6_greyscale_audit.png", w = 16, h = 14)
}

# ==============================================================================
# SUMMARY
# ==============================================================================
section("SUMMARY")
total <- .lc_pass + .lc_fail
cli_h2(sprintf("Resultado: %d / %d passed  |  %d failed", .lc_pass, total, .lc_fail))
if (.lc_fail > 0L) {
  cli_alert_danger("Fallos:")
  message(paste(.lc_log, collapse = "\n"))
} else {
  cli_alert_success("All tests passed.")
}
cli_alert_info("Plots saved to {.file {out_dir}/}")