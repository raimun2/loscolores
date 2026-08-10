# ==============================================================================
# dev/debug_engine_pass.R
# Passes every palette in sysdata `paletas` through the full engine
# (loscolores_system) and saves plot.lc_system() for each one -- 3 blocks:
#   Block A  -- original input vs. raw_7 (raw_5 + white/black poles)
#   Block B1 -- 5 sequential options
#   Block B2 -- 5 divergent options
#   Block C  -- categorical options
#
# plot.lc_system() already solves the width problem: the input row is
# remapped to pos = seq(1, 7, length.out = length(input)), so it spans the
# same horizontal width as the raw_7 row regardless of how many colors the
# input has.
#
# Run from the package root:
#   source("dev/debug_engine_pass.R")
#
# Saves: dev/plots/<palette_name>.png -- one file per palette (727 total)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(cli)
})

out_dir <- "dev/plots"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load("R/sysdata.rda")  # paletas, lc_black, lc_white
for (f in c("utils_color.R", "engine_core.R", "engine_expansion.R",
            "methods_s3.R", "scales.R", "theme.R", "zzz.R")) {
  source(file.path("R", f))
}

sanitize_filename <- function(nm) {
  nm <- gsub("[^A-Za-z0-9_-]+", "_", nm)
  gsub("_+", "_", nm)
}

n      <- length(paletas)
failed <- character(0L)
cli_h1(sprintf("Passing %d palettes through the engine", n))

for (nm in names(paletas)) {
  original_hex <- paletas[[nm]]$hexcolors

  result <- tryCatch({
    sys <- loscolores_system(original_hex)
    plot(sys)
  }, error = function(e) e)

  if (inherits(result, "error")) {
    failed <- c(failed, nm)
    cli_alert_danger(sprintf("[%s] %s", nm, conditionMessage(result)))
    next
  }

  path <- file.path(out_dir, paste0(sanitize_filename(nm), ".png"))
  ggplot2::ggsave(path, plot = result, width = 12, height = 10, dpi = 150, bg = "white")
}

cli_alert_success(sprintf("Done: %d / %d palettes saved to %s/",
                           n - length(failed), n, out_dir))
if (length(failed) > 0L) {
  cli_alert_warning(sprintf("%d palettes failed:", length(failed)))
  for (nm in failed) message("  - ", nm)
}
