#' @importFrom farver decode_colour encode_colour

# Transforma Hex a Oklab
to_oklab <- function(hex) {
  lab <- farver::decode_colour(hex, to = "oklab")
  data.frame(
    L = lab[,1], a = lab[,2], b = lab[,3],
    h = atan2(lab[,3], lab[,2]) * (180 / pi),
    c = sqrt(lab[,2]^2 + lab[,3]^2)
  )
}

# Transforma Oklab a Hex
to_hex <- function(L, a, b) {
  L <- pmax(0, pmin(1, L)) 
  mat <- matrix(c(L, a, b), ncol = 3)
  farver::encode_colour(mat, from = "oklab")
}

# Sanitizador de vectores (Limpia residuos de parsing)
clean_hex_vector <- function(x) {
  raw <- paste(unlist(x), collapse = " ")
  clean <- unlist(regmatches(raw, gregexpr("#[0-9a-fA-F]{6,8}", raw)))
  return(as.character(clean))
}