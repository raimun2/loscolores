#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @export

# ==========================================
# 1. VISUALIZACIÓN DE SISTEMA (plot.lc_system)
# ==========================================
plot.lc_system <- function(x, ...) {
  sys <- x # Convención S3
 
  # Helper 1: Bloque Input/Raw5 (Desacoplado)
  build_head <- function(input, raw_5) {
    n_in <- length(input)
    gray_5 <- sapply(raw_5, function(x) to_hex(to_oklab(x)$L, 0, 0))
    
    # PLOT A: Input original (Soporte para vectores masivos)
    df_input <- data.frame(x = seq_along(input), y = 1, color = input)
    p_in <- ggplot(df_input, aes(x = x, y = y, fill = color)) +
      geom_tile(color = ifelse(n_in > 25, NA, "white"), linewidth = 0.5) +
      scale_fill_identity() +
      theme_void() +
      labs(title = paste0("1. Input (Caos - ", n_in, " colores)")) +
      theme(plot.title = element_text(face = "bold", size = 11, hjust = 0, margin = margin(b = 5)))
    
    # Inyección condicional de texto
    if (n_in <= 15) {
      text_cols <- ifelse(to_oklab(input)$L > 0.65, "#121212", "#FAFAFA")
      p_in <- p_in + geom_text(aes(label = color), color = text_cols, fontface = "bold", size = 2.5)
    }
    
    # PLOT B: Raw 5 
    df_raw <- rbind(
      data.frame(x = 1:5, color = raw_5, type = "2. Raw 5 (L-Dictator)"),
      data.frame(x = 1:5, color = gray_5, type = "3. Raw 5 (Grayscale)")
    )
    df_raw$type <- factor(df_raw$type, levels = c("2. Raw 5 (L-Dictator)", "3. Raw 5 (Grayscale)"))
    text_cols_raw <- ifelse(to_oklab(df_raw$color)$L > 0.65, "#121212", "#FAFAFA")
    
    p_raw <- ggplot(df_raw, aes(x = x, y = type, fill = color)) +
      geom_tile(color = "white", linewidth = 1, width = 0.95, height = 0.8) +
      geom_text(aes(label = color), color = text_cols_raw, fontface = "bold", size = 2.5) +
      scale_fill_identity() +
      facet_grid(type ~ ., scales = "free_y") +
      theme_void() +
      theme(strip.text = element_text(face = "bold", hjust = 0, margin = margin(b = 5, t = 5)))
    
    # Ensamblaje interno aislado para evitar conflictos de escalas X
    wrap_elements(p_in / p_raw + plot_layout(heights = c(1, 1.8)))
  }
  
  # Helper 2: Bloques Variantes (Adaptables a dimensionalidad extendida)
  build_block <- function(pal_list, title) {
    df <- do.call(rbind, lapply(names(pal_list), function(name) {
      colors <- pal_list[[name]]
      gray_colors <- sapply(colors, function(x) to_hex(to_oklab(x)$L, 0, 0))
      rbind(
        data.frame(variant = name, x = seq_along(colors), color = colors, mode = "Color"),
        data.frame(variant = name, x = seq_along(colors), color = gray_colors, mode = "Grayscale")
      )
    }))
    
    df$mode <- factor(df$mode, levels = c("Color", "Grayscale"))
    max_n <- max(sapply(pal_list, length))
    
    p <- ggplot(df, aes(x = x, y = mode, fill = color)) +
      geom_tile(color = ifelse(max_n > 25, NA, "white"), linewidth = 0.5) +
      scale_fill_identity() +
      facet_wrap(~ variant, ncol = 1) +
      theme_minimal() +
      labs(title = title) +
      theme(
        strip.text = element_text(face = "bold", hjust = 0, margin = margin(b = 2, t = 8)),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", color = "grey40", size = 7),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.background = element_rect(fill = "#FAFAFA", color = NA)
      )
    
    if (max_n <= 15) {
      text_colors <- ifelse(to_oklab(df$color)$L > 0.65, "#121212", "#FAFAFA")
      p <- p + geom_text(aes(label = color), color = text_colors, size = 2, fontface = "bold")
    }
    
    return(p)
  }
  
  # Matriz Base
  p_head <- build_head(sys$input, sys$raw_5)
  p_cat <- build_block(sys$categorical, "CATEGORICAL (N Dynamic)")
  p_seq <- build_block(sys$sequential, "SEQUENTIAL (N Dynamic)")
  p_div <- build_block(sys$divergent, "DIVERGENT (N Dynamic)")
  
  # Topología Macro
  layout <- "
  AAAAAA
  BBBBBB
  CCCDDD
  "
  
  p_head + p_cat + p_seq + p_div + 
    plot_layout(design = layout, heights = c(1, 1.5, 2)) +
    plot_annotation(
      title = "loscolores | System Diagnostic",
      theme = theme(
        plot.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10))
      )
    )
}

# ==========================================
# 2. REPORTE DE NORMALIZACIÓN (Pre vs Post)
# ==========================================
analyze_palette <- function(hex_vec, name) {
  lab <- to_oklab(hex_vec)
  n <- length(hex_vec)
  
  df <- data.frame(
    index = 1:n,
    L = lab[,1], a = lab[,2], b = lab[,3],
    hex = hex_vec,
    palette = name
  )
  
  df <- df %>%
    mutate(
      c = sqrt(a^2 + b^2),
      h = atan2(b, a) * (180 / pi)
    )
  
  if (n > 1) {
    de_vec <- c(NA, sapply(2:n, function(i) {
      sqrt(sum((lab[i,] - lab[i-1,])^2))
    }))
    df$delta_e <- de_vec
  } else {
    df$delta_e <- NA
  }
  return(df)
}

#' @export
plot_normalization_report <- function(pre_hex, post_hex, title = "loscolores | Normalization Report") {
  
  # Análisis de ambas paletas
  df_pre <- analyze_palette(pre_hex, "1. Pre-Normalización")
  df_post <- analyze_palette(post_hex, "2. Post-Normalización (Hélice)")
  df_all <- rbind(df_pre, df_post)
  
  # Configuración global de tema
  theme_report <- function() {
    theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 11, hjust = 0),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none"
      )
  }
  
  # A. TRAYECTORIA 3D (Proyección a-b y L-Croma)
  
  # A1. Plano a-b (El "Garabato" vs. La "Espirales")
  p_ab <- ggplot(df_all, aes(x = a, y = b, color = palette)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_point(aes(fill = hex), size = 3, shape = 21, color = "white", stroke = 1) +
    scale_color_manual(values = c("1. Pre-Normalización" = "#E74C3C", "2. Post-Normalización (Hélice)" = "#2ECC71")) +
    scale_fill_identity() +
    coord_fixed() +
    labs(title = "Trayectoria de Tono (Plano a-b)", x = "a* (Verde-Rojo)", y = "b* (Azul-Amarillo)") +
    theme_report() +
    theme(panel.grid.major = element_line(color = "grey90"))
  
  # A2. Perfil de Luminosidad y Croma (L vs. c)
  p_lc <- ggplot(df_all, aes(x = L, y = c, color = palette)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_point(aes(fill = hex), size = 3, shape = 21, color = "white", stroke = 1) +
    scale_color_manual(values = c("1. Pre-Normalización" = "#E74C3C", "2. Post-Normalización (Hélice)" = "#2ECC71")) +
    scale_fill_identity() +
    labs(title = "Perfil de Luminosidad vs. Croma", x = "L* (Luminosidad)", y = "c* (Croma / Saturación)") +
    theme_report()
  
  # B. DIAGNÓSTICO DE LINEALIDAD (L* por Índice)
  p_linealidad <- ggplot(df_all, aes(x = index, y = L, color = palette)) +
    geom_line(linewidth = 1.5) +
    geom_point(aes(fill = hex), size = 4, shape = 21, color = "white", stroke = 1.5) +
    scale_color_manual(values = c("1. Pre-Normalización" = "#E74C3C", "2. Post-Normalización (Hélice)" = "#2ECC71")) +
    scale_fill_identity() +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(title = "Linealidad Perceptual (L*)", x = "Índice de Color", y = "L* (Luminosidad)") +
    theme_report() +
    theme(panel.grid.major.y = element_line(color = "grey90"))
  
  # C. MATRIZ DE COLISIÓN (Delta E)
  df_de <- df_all %>% filter(!is.na(delta_e))
  p_de <- ggplot(df_de, aes(x = index, y = delta_e, fill = palette)) +
    geom_col(position = "dodge", alpha = 0.9) +
    scale_fill_manual(values = c("1. Pre-Normalización" = "#E74C3C", "2. Post-Normalización (Hélice)" = "#2ECC71")) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "#E67E22", linewidth = 0.8) + # Umbral de colisión aproximado
    annotate("text", x = 2, y = 0.06, label = "Umbral de Colisión", color = "#E67E22", fontface = "bold", size = 3, hjust = 0) +
    labs(title = "Distancia Perceptual (ΔE) al Anterior", x = "Índice de Color", y = "ΔE (Oklab)") +
    theme_report()
  
  # D. VISUALIZACIÓN DE BARRAS (El Antes y Después)
  # Preparamos los datos para geom_tile
  df_bar_pre <- df_pre %>% mutate(y = 2, mode = "Color")
  df_bar_post <- df_post %>% mutate(y = 1, mode = "Color")
  df_bars <- rbind(df_bar_pre, df_bar_post)
  df_bars$palette <- factor(df_bars$palette, levels = c("1. Pre-Normalización", "2. Post-Normalización (Hélice)"))
  
  # Determinamos color de texto condicional
  df_bars$text_color <- ifelse(df_bars$L > 0.65, "#121212", "#FAFAFA")
  
  p_bars <- ggplot(df_bars, aes(x = index, y = palette, fill = hex)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = hex, color = text_color), fontface = "bold", size = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    labs(title = "Comparación de Secuencias Completa") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b=10)),
      axis.text.y = element_text(face = "bold", size = 10, hjust = 1, margin = margin(r=10)),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # E. ENSAMBLAJE FINAL CON PATCHWORK
  # Diseño de Layout: Barras arriba, diagnósticos abajo en grilla 2x2
  layout_main <- "
  AAAAAAA
  BCDEEEE
  "
  
  report_patch <- (p_ab | p_lc) / (p_linealidad | p_de) / wrap_elements(p_bars) +
    plot_layout(heights = c(2, 2, 1)) +
    plot_annotation(
      title = title,
      subtitle = "Evaluación de la corrección de entropía perceptual en el espacio OKLab",
      theme = theme(
        plot.background = element_rect(fill = "#FAFAFA", color = NA),
        plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, color = "grey40", margin = margin(b = 15))
      )
    )
  
  return(report_patch)
}