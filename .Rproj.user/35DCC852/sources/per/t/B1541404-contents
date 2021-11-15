#' list of palettes generated from instagram posts
#'
#' @export
laspaletas <- readRDS("extdata/palettes.rds")


#' Los colores generate palettes from my past instagram posts. function borrowed from wesanderson package
#'
#' @param index index of the palette, between 1 and 39
#' @param n number of colors for discrete palette
#' @param type type of palette, continuos or discrete
#'
#' @return a palette with desired specifications
#' @export
#'
#' @examples
loscolores <- function(index, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- laspaletas[index,]
  if (is.null(pal)) stop("Palette not found.")

  if (missing(n)) { n <- length(pal) }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[sort(sample(1:length(pal), n))]
  )
  structure(out, class = "palette", index = index)
}
