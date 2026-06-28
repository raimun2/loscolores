#' @export
loscolores_system <- function(x) {
  input_hex <- get_input_vector(x)
  core <- get_raw_5(input_hex)
  expansions <- get_expansion_params(core)
  
  structure(
    list(
      input = input_hex,
      raw_5 = core$raw_5,
      polos = list(black = core$hex_black, white = core$hex_white),
      categorical = expansions$categorical,
      sequential = expansions$sequential,
      divergent = expansions$divergent
    ),
    class = "lc_system"
  )
}

#' @export
set.loscolores <- function(...) {
  # 1. Capturar inputs variádicos (ej. set.loscolores("#00FFCC", "#FF00FF"))
  input_raw <- c(...)
  
  # 2. Computar topología
  sys <- loscolores_system(input_raw)
  
  # 3. Mutar el estado global de la sesión
  options(loscolores.active_system = sys)
  
  # 4. Devolver visiblemente para permitir encadenamiento o ploteo directo: plot(set.loscolores("mario"))
  return(sys)
}