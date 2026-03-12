#' Constructor del sistema loscolores
#' @export
loscolores_system <- function(input_hex, n_cat = 10, n_seq = 5, n_div = 5) {
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