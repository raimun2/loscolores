# ==========================================
# loscolores: THE COLOR ARTIFACT (v1.0)
# ==========================================
library(farver)
library(ggplot2)
library(patchwork)
paletas = readr::read_rds("extdata/paletas_prop.rds")

# --- 1. MOTOR ESPACIAL EN OKLAB ---
to_oklab <- function(hex) {
  lab <- decode_colour(hex, to = "oklab")
  data.frame(
    L = lab[,1], a = lab[,2], b = lab[,3],
    h = atan2(lab[,3], lab[,2]) * (180 / pi),
    c = sqrt(lab[,2]^2 + lab[,3]^2)
  )
}

to_hex <- function(L, a, b) {
  L <- pmax(0, pmin(1, L)) 
  mat <- matrix(c(L, a, b), ncol = 3)
  encode_colour(mat, from = "oklab")
}

# --- 2. EL EMBUDO (RAW 5 & L-DICTATOR) ---
get_raw_5 <- function(input_hex) {
  n_in <- length(input_hex)
  lab <- to_oklab(input_hex)
  
  out_L <- numeric(5); out_c <- numeric(5); out_h <- numeric(5)
  
  # FASE 1: EXTRACCIÓN TOPOLÓGICA
  if (n_in == 1) {
    out_h <- (lab$h + (0:4) * 72) %% 360
    out_c <- rep(lab$c, 5)
    out_L <- c(0.5, 0.3, 0.7, 0.1, 0.9) 
    
  } else if (n_in < 5) {
    out_h[1:n_in] <- lab$h; out_c[1:n_in] <- lab$c; out_L[1:n_in] <- lab$L
    
    sorted_h <- sort(lab$h)
    gaps <- c(diff(sorted_h), sorted_h[1] + 360 - sorted_h[n_in])
    start_h <- sorted_h[which.max(gaps)]
    
    n_missing <- 5 - n_in
    step <- max(gaps) / (n_missing + 1)
    
    out_h[(n_in+1):5] <- (start_h + (1:n_missing) * step) %% 360
    out_c[(n_in+1):5] <- mean(lab$c)
    out_L[(n_in+1):5] <- seq(0.1, 0.9, length.out = n_missing)
    
  } else if (n_in == 5) {
    out_h <- lab$h; out_c <- lab$c; out_L <- lab$L
    
  } else {
    set.seed(42) 
    km <- kmeans(lab[, c("L", "a", "b")], centers = 5, iter.max = 20, nstart = 10)
    cent <- km$centers
    
    for(i in 1:5) {
      out_L[i] <- cent[i, "L"]
      out_h[i] <- atan2(cent[i, "b"], cent[i, "a"]) * (180 / pi)
      out_c[i] <- sqrt(cent[i, "a"]^2 + cent[i, "b"]^2)
    }
  }
  
  # FASE 2: DICTADOR DE LUMINOSIDAD
  out_h <- out_h %% 360
  sorted_idx <- order(out_L) 
  L_anchors <- c(0.25, 0.40, 0.55, 0.70, 0.85) 
  
  final_hex <- character(5)
  for(i in 1:5) {
    orig_idx <- sorted_idx[i]
    L_final <- L_anchors[i]
    c_final <- out_c[orig_idx]
    
    if (i == 1 || i == 5) c_final <- c_final * 0.5
    
    hr <- out_h[orig_idx] * (pi / 180)
    final_hex[i] <- to_hex(L_final, cos(hr) * c_final, sin(hr) * c_final)
  }
  return(final_hex)
}

# --- 3. MOTORES DE EXPANSIÓN (LAS 15 VARIANTES) ---
gen_categorical <- function(raw_5, n = 10) {
  lab <- to_oklab(raw_5)
  cat_list <- list()
  
  t_in <- seq(0, 1, length.out = 5)
  t_out <- seq(0, 1, length.out = n)
  
  L_spline <- splinefun(t_in, lab$L, method = "natural")(t_out)
  a_spline <- splinefun(t_in, lab$a, method = "natural")(t_out)
  b_spline <- splinefun(t_in, lab$b, method = "natural")(t_out)
  
  pack_hex <- function(L, a, b) {
    out <- character(n)
    for(k in 1:n) out[k] <- to_hex(L[k], a[k], b[k])
    return(out)
  }
  
  cat_list[["Cat_A_Structural"]] <- pack_hex(L_spline, a_spline, b_spline)
  
  idx_1 <- 1:ceiling(n/2)
  idx_2 <- (ceiling(n/2) + 1):n
  zig_idx <- c(rbind(idx_1, c(idx_2, NA)))
  zig_idx <- zig_idx[!is.na(zig_idx)][1:n]
  
  cat_list[["Cat_B_ZigZag"]] <- cat_list[["Cat_A_Structural"]][zig_idx]
  cat_list[["Cat_C_Vibrant"]] <- pack_hex(L_spline, a_spline * 1.5, b_spline * 1.5)
  cat_list[["Cat_D_Muted"]] <- pack_hex(L_spline, a_spline * 0.4, b_spline * 0.4)
  cat_list[["Cat_E_Isoluminant"]] <- pack_hex(rep(0.55, n), a_spline, b_spline)
  
  return(cat_list)
}

gen_sequential <- function(raw_5, n = 7) {
  lab <- to_oklab(raw_5)
  seq_list <- list()
  
  for(i in 1:5) {
    L_anc <- lab$L[i]; a_anc <- lab$a[i]; b_anc <- lab$b[i]
    L_tgt <- ifelse(L_anc >= 0.5, L_anc - 0.5, L_anc + 0.5)
    
    L_seq <- seq(L_anc, L_tgt, length.out = n)
    a_seq <- seq(a_anc, a_anc * 0.15, length.out = n)
    b_seq <- seq(b_anc, b_anc * 0.15, length.out = n)
    
    hex_seq <- character(n)
    for(k in 1:n) hex_seq[k] <- to_hex(L_seq[k], a_seq[k], b_seq[k])
    seq_list[[paste0("Seq_", i)]] <- hex_seq
  }
  return(seq_list)
}

gen_divergent <- function(raw_5, n = 9) {
  lab <- to_oklab(raw_5)
  div_list <- list()
  
  for(i in 1:5) {
    L_anc <- lab$L[i]; a_anc <- lab$a[i]; b_anc <- lab$b[i]
    L_tgt <- ifelse(L_anc >= 0.5, L_anc - 0.5, L_anc + 0.5)
    
    L_seq <- seq(L_anc, L_tgt, length.out = n)
    a_seq <- seq(a_anc, -a_anc, length.out = n)
    b_seq <- seq(b_anc, -b_anc, length.out = n)
    
    hex_seq <- character(n)
    for(k in 1:n) hex_seq[k] <- to_hex(L_seq[k], a_seq[k], b_seq[k])
    div_list[[paste0("Div_", i)]] <- hex_seq
  }
  return(div_list)
}

# --- 4. ENSAMBLADOR DEL SISTEMA (CONSTRUCTOR) ---
loscolores_system <- function(input_hex, n_cat = 10, n_seq = 7, n_div = 9) {
  raw_5 <- get_raw_5(input_hex)
  sys <- list(
    input = input_hex,
    raw_5 = raw_5,
    categorical = gen_categorical(raw_5, n_cat),
    sequential = gen_sequential(raw_5, n_seq),
    divergent = gen_divergent(raw_5, n_div)
  )
  class(sys) <- "lc_system"
  return(sys)
}

# --- 5. RENDERIZADO VISUAL (ACTUALIZADO PARA VECTORES MASIVOS) ---
plot.lc_system <- function(sys) {
  
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
# EJECUCIÓN DEL ARTEFACTO
# ==========================================

# Test Input: Vector de 3 colores (El motor inyectará 2 Hues faltantes y dictará la luz)
input_test <- c("#264653", "#2A9D8F", "#E9C46A")

# 1. Instanciar el sistema
mi_sistema <- loscolores_system(input_test, n_cat = 12, n_seq = 7, n_div = 9)

# 2. Renderizar el panel de control completo
plot(mi_sistema)

# ==========================================
# MOTOR DE RENDERIZADO Y EXPORTACIÓN
# ==========================================
# ==========================================
# MOTOR DE RENDERIZADO Y EXPORTACIÓN (FIXED)
# ==========================================

base_dir <- "figures"

invisible(lapply(names(paletas), function(p_name) {
  
  p_data <- paletas[[p_name]]
  p_source <- p_data$source
  target_dir <- file.path(base_dir, p_source)
  
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  file_path <- file.path(target_dir, paste0(p_name, ".png"))
  
  # Aumento sustancial de altura (1200px) para acomodar la grilla de patchwork
  png(filename = file_path, width = 1600, height = 1200, res = 150)
  
  tryCatch({
    # 1. Procesar latente
    sys_obj <- loscolores_system(p_data$hexcolors)
    
    # 2. Renderizado explícito (obligatorio dentro de loops/lapply para ggplot/patchwork)
    print(plot(sys_obj))
    
  }, error = function(e) {
    message(paste("Fallo en latente [", p_name, "]: ", e$message, sep=""))
  })
  
  dev.off()
}))

cat("Exportación batch finalizada. Artefactos impresos y colapsados en:", base_dir, "\n")


# ==========================================
# REPORTE DE NORMALIZACIÓN (PRE VS. POST)
# ==========================================
library(farver)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

# --- 1. MOTOR DE DIAGNÓSTICO GEOMÉTRICO (OKLAB) ---
# Extracción de coordenadas y cálculo de métricas perceptuales
analyze_palette <- function(hex_vec, name) {
  lab <- decode_colour(hex_vec, to = "oklab")
  n <- length(hex_vec)
  
  # Coordenadas base
  df <- data.frame(
    index = 1:n,
    L = lab[,1], a = lab[,2], b = lab[,3],
    hex = hex_vec,
    palette = name
  )
  
  # Métricas derivadas: Croma y Hue
  df <- df %>%
    mutate(
      c = sqrt(a^2 + b^2),
      h = atan2(b, a) * (180 / pi)
    )
  
  # Delta E (distancia perceptual al color anterior)
  if (n > 1) {
    # Distancia Euclidiana en Oklab
    de_vec <- c(NA, sapply(2:n, function(i) {
      sqrt(sum((lab[i,] - lab[i-1,])^2))
    }))
    df$delta_e <- de_vec
  } else {
    df$delta_e <- NA
  }
  
  return(df)
}

# --- 2. MOTOR DE RENDERIZADO DEL REPORTE (PLOT) ---
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

# ==========================================
# EJECUCIÓN DEL ARTEFACTO VISUAL
# ==========================================

# 1. Definir la paleta "Mala" (Pre)
# El 'rainbow' estándar de R es el ejemplo perfecto de caos perceptual.
n_colors <- 15
paleta_pre <- rainbow(n_colors)

# 2. Generar la paleta "Normalizada" (Post)
# Usamos el motor loscolores_system para extraer el Raw 5 y expandirlo a 15
# mediante splines en Oklab (Cat_A_Structural).
post_system <- loscolores_system(paleta_pre, n_cat = n_colors)
paleta_post <- post_system$categorical$Cat_A_Structural

# 3. Renderizar el Reporte Comparativo
reporte <- plot_normalization_report(paleta_pre, paleta_post, title = "loscolores | Rainbow Normalization Report")

# Visualizar
reporte

