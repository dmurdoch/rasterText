
#' Draw some text to a raster
#'
#' @param text A character string to measure
#' @param color Foreground color to draw
#' @param family,font,cex Characteristics of the text
#'
#' @returns A list with components:
#' * raster, a native raster object.
#' * locations, a matrix giving the locations of each
#'   text string in the raster.
#' @importFrom rgl par3d
#' @export
#' @examples
#' draw_text_to_raster(c("a", "abc", "j"), family = "serif")
draw_text_to_raster <- function(text, family = par3d("family"),                       color = material3d("color")[1],
                         font = par3d("font"),
                         cex = par3d("cex")) {
  text <- enc2utf8(as.character(text))
  n <- length(text)
  rgba <- as.numeric(col2rgb(color, alpha = TRUE))/255
  family <- as.character(family)
  font <- as.integer(font)
  size <- as.numeric(cex)*20

  m <- measure_text(text, family, font, cex)

  # Add a single pixel margin on all sides
  width <- as.integer(max(m[, "width"] + 2))
  height <- as.integer(sum(m[, "height"] + 2))

  x <- 1 - m[, "x_bearing"]
  top <- c(0, cumsum(m[seq_len(n-1), "height"] + 2)) + 1
  y <- top - m[, "y_bearing"]

  result <- .Call("draw_text_to_raster", x, y, text, rgba, family, font,
                  size, width, height, PACKAGE = "rasterText")
  matrix(result, height, width, byrow = TRUE)
}
