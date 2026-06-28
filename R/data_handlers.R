#' @export
get_input_vector <- function(x) {
  # Carga diferida de la memoria latente
  env <- new.env()
  load(system.file("R/sysdata.rda", package = "loscolores"), envir = env)
  
  if (length(x) == 1 && x %in% names(env$paletas)) {
    return(env$paletas[[x]]$hexcolors)
  }
  
  # Aduana: Resiliencia ante vectores contaminados (ej. c("#FFFFFF", NA, "red"))
  valid_hex <- x[grepl("^#[0-9A-Fa-f]{6,8}$", x) & !is.na(x)]
  
  if (length(valid_hex) > 0) {
    return(valid_hex)
  }
  
  stop("Colapso de señal: Input no existe en sysdata ni contiene hexágonos válidos.")
}