
#' Draw some text to a raster
#'
#' @param text A character string to measure
#' @param color Foreground color to draw
#' @param family,font,cex Characteristics of the text
#' @param fontfile Freetype font file name, or NULL
#'
#' @returns A list with components:
#' * raster, a native raster object.
#' * locations, a matrix giving the locations of each
#'   text string in the raster.
#' @importFrom rgl par3d
#' @export
#' @examples
#' draw_text_to_raster(paste("text ", 1:3), family="serif") |> image()
draw_text_to_raster <- function(text, family = par3d("family"),                       color = material3d("color")[1],
                         font = par3d("font"),
                         fontfile = NULL,
                         cex = par3d("cex")) {
  text <- enc2utf8(as.character(text))
  n <- length(text)
  rgba <- rep_len(as.numeric(col2rgb(color, alpha = TRUE))/255,
                  4*n)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  if (!is.null(fontfile))
    fontfile <- rep_len(as.character(fontfile), n)
  size <- rep_len(as.double(cex)*20, n)

  m <- measure_text(text, family, font, fontfile, cex)

  # Add a single pixel margin on all sides
  width <- as.integer(max(m[, "width"] + 2))
  height <- as.integer(sum(m[, "height"] + 2))

  x <- 1 - m[, "x_bearing"]
  top <- c(0, cumsum(m[seq_len(n-1), "height"] + 2)) + 1
  y <- top - m[, "y_bearing"]

  result <- .Call("draw_text_to_raster", x, y, text, rgba, family, font,
                  fontfile, size, width, height, PACKAGE = "rasterText")
  t(matrix(result, height, width, byrow = TRUE))[,height:1]
}
