#' @importFrom stats splinefun

gen_categorical <- function(raw_5, n = 10) {
  lab <- to_oklab(raw_5)
  
  # Puntos de control (x) y valores (y)
  # t=0 es el Basal Color
  t_in <- seq(0, 1, length.out = 5)
  t_out <- seq(0, 1, length.out = n)
  
  # Splines naturales: garantizan que en t=0 el valor sea lab[1, ]
  L_spline <- splinefun(t_in, lab$L, method = "natural")(t_out)
  a_spline <- splinefun(t_in, lab$a, method = "natural")(t_out)
  b_spline <- splinefun(t_in, lab$b, method = "natural")(t_out)
  
  hex_out <- character(n)
  for(k in 1:n) {
    # Forzamos el primer color para evitar micro-errores de redondeo del spline
    if(k == 1) {
      hex_out[k] <- raw_5[1]
    } else {
      hex_out[k] <- to_hex(L_spline[k], a_spline[k], b_spline[k])
    }
  }
  
  cat_list <- list()
  cat_list[["Cat_A_Basal_Anchor"]] <- hex_out # Nombre actualizado
  
  # ZigZag modificado para que el basal siempre sea el primero
  # El resto se alterna para maximizar contraste
  idx_rest <- 2:n
  idx_1 <- idx_rest[1:ceiling(length(idx_rest)/2)]
  idx_2 <- idx_rest[(ceiling(length(idx_rest)/2) + 1):length(idx_rest)]
  if(length(idx_2) < length(idx_1)) idx_2 <- c(idx_2, NA)
  
  zig_rest <- as.vector(rbind(idx_1, idx_2))
  zig_rest <- zig_rest[!is.na(zig_rest)]
  
  cat_list[["Cat_B_ZigZag"]] <- hex_out[c(1, zig_rest)]
  
  # Otras variantes...
  cat_list[["Cat_C_Vibrant"]] <- sapply(1:n, function(k) {
    if(k==1) return(raw_5[1])
    to_hex(L_spline[k], a_spline[k]*1.5, b_spline[k]*1.5)
  })
  
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