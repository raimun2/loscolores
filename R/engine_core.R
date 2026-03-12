#' @importFrom stats kmeans
#' @importFrom dplyr %>%
#' @export

# ==========================================
# EL EMBUDO: EXTRACCIÓN Y NORMALIZACIÓN
# ==========================================

get_raw_5 <- function(input_hex) {
  # 1. PREPARACIÓN Y SOBERANÍA DEL ANCLA
  # ------------------------------------------
  n_in <- length(input_hex)
  lab_full <- to_oklab(input_hex)
  
  # El primer color es la "Ley Basal": Inamovible en Tono y Croma
  basal_lab <- lab_full[1, ]
  basal_h <- atan2(basal_lab$b, basal_lab$a) * (180 / pi)
  basal_c <- sqrt(basal_lab$a^2 + basal_lab$b^2)

  # 2. EXTRACCIÓN / GENERACIÓN (RESILIENCIA)
  # ------------------------------------------
  if (n_in >= 5) {
    # Caso Ideal: Clustering sobre el ruido para encontrar la estructura
    set.seed(42)
    others_data <- lab_full[-1, c("L", "a", "b")]
    centros <- stats::kmeans(others_data, centers = 4, nstart = 10)$centers
    others <- data.frame(
      L = centros[, "L"], a = centros[, "a"], b = centros[, "b"]
    )
    raw_5_df <- rbind(basal_lab[, c("L", "a", "b")], others)
    
  } else {
    # CASO ESCASO: El sistema siembra su propia diversidad
    raw_5_df <- lab_full[, c("L", "a", "b")]
    n_missing <- 5 - n_in
    
    # --- MÁQUINA DE RECUPERACIÓN CRÓMATICA ---
    # Si el ancla es gris (C < 0.02), inyectamos una semilla de croma (0.1)
    # y un Hue de referencia (200 = Azul) para que la paleta no sea "muerta".
    c_seed <- if(basal_c < 0.02) 0.1 else basal_c
    h_ref  <- if(basal_c < 0.02) 200 else basal_h
    # ----------------------------------------
    
    # Generamos los faltantes rotando el Hue (Hélice latente)
    h_offsets <- seq(72, 72 * n_missing, length.out = n_missing)
    new_hues  <- (h_ref + h_offsets) %% 360
    
    generated <- data.frame(
      L = rep(basal_lab$L, n_missing), # Empezamos con el mismo L del basal
      a = cos(new_hues * pi / 180) * c_seed,
      b = sin(new_hues * pi / 180) * c_seed
    )
    
    raw_5_df <- rbind(raw_5_df, generated)
  }

  # 3. DICTADOR MINIMALISTA BIDIRECCIONAL (Luminosity Gap)
  # ------------------------------------------
  # El Basal (ID 1) es el pivote fijo. Los demás buscan su lugar.
  min_gap <- 0.15 
  raw_5_df$L_final <- raw_5_df$L
  ocupados <- raw_5_df$L[1] # L_basal es el primer "bloqueo"
  
  # Procesamos los otros 4 puntos buscando el desplazamiento mínimo
  # No forzamos orden oscuro-claro; forzamos "espacio vital".
  for(i in 2:5) {
    target_L <- raw_5_df$L[i]
    
    # ¿Hay colisión con el ancla o los colores ya aceptados?
    if(any(abs(target_L - ocupados) < min_gap)) {
      
      # BUSQUEDA DE SLOT LIBRE: Buscamos en todo el rango [0.05, 0.95]
      # Cuál es el valor de L que respeta el min_gap y está más cerca del original
      posibles <- seq(0.05, 0.95, by = 0.01)
      validos <- posibles[sapply(posibles, function(p) {
        all(abs(p - ocupados) >= min_gap)
      })]
      
      if(length(validos) > 0) {
        # Fidelidad: Elegimos el valor válido con menor Delta_L
        best_L <- validos[which.min(abs(validos - target_L))]
        raw_5_df$L_final[i] <- best_L
        ocupados <- c(ocupados, best_L)
      } else {
        # Fallback de emergencia si el espacio está saturado
        fallback_L <- (target_L + 0.2) %% 1
        raw_5_df$L_final[i] <- fallback_L
        ocupados <- c(ocupados, fallback_L)
      }
    } else {
      # Si no colisiona, se queda tal cual. El sistema es feliz.
      ocupados <- c(ocupados, target_L)
    }
  }

  # 4. RECONSTRUCCIÓN DE LOS 5 ANCLAJES (RAW 5)
  # ------------------------------------------
  final_hex <- character(5)
  for(k in 1:5) {
    row <- raw_5_df[k, ]
    
    # Gamut Protection: Suavizamos el croma si L es muy extremo 
    # para evitar colores que "no existen" en sRGB (clipping).
    l_val <- row$L_final
    c_orig <- sqrt(row$a^2 + row$b^2)
    c_factor <- if(l_val < 0.1 || l_val > 0.9) 0.6 else if(l_val < 0.2 || l_val > 0.8) 0.8 else 1.0
    
    final_hex[k] <- to_hex(
      l_val, 
      row$a * c_factor, 
      row$b * c_factor
    )
  }
  
  return(final_hex)
}