# Convierte matrices de color crudas a formato latente
parse_matrix_palettes <- function(color_matrix, source_name) {
  num_palettes <- nrow(color_matrix)
  
  lapply(1:num_palettes, function(i) {
    hex_vec <- as.character(color_matrix[i, ])
    hex_vec <- hex_vec[!is.na(hex_vec) & hex_vec != ""]
    
    list(
      name = paste(source_name, "seq", i, sep = "_"),
      source = source_name,
      type = "discrete",
      colorblindness = NA,
      hexcolors = clean_hex_vector(hex_vec)
    )
  })
}