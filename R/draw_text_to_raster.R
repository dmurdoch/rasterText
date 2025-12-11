
#' Draw some text to a raster
#'
#' @param text A character string to measure
#' @param family,font,cex Characteristics of the text
#' @param fontfile Freetype font file name, or NULL
#'
#' @returns A list with components:
#' * raster, a native raster object.
#' * locations, a matrix giving the locations of each
#'   text string in the raster.
#' @importFrom grDevices col2rgb
#' @export
#' @examples
#' draw_text_to_raster(paste("text ", 1:3), family="serif",
#' font = 1, cex = 1) |> image()
draw_text_to_raster <- function(text, family,
                         font, fontfile = NULL,
                         cex) {
  text <- enc2utf8(as.character(text))
  n <- length(text)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  if (!is.null(fontfile))
    fontfile <- rep_len(as.character(fontfile), n)
  size <- rep_len(as.double(cex)*20, n)

  m <- measure_text(text, family, font, fontfile, cex)

  # Add a single pixel margin on all sides
  width <- as.integer(max(m[, "width"] + 2))
  width <- as.integer(2^ceiling(log(width, 2)))
  xy <- pack_text(text, m, width)
  height <- as.integer(attr(xy, "height"))

  result <- .Call(C_draw_text_to_rasterR, xy[,1], xy[,2], text, family, font,
                  fontfile, size, width, height)
  t(matrix(result, height, width, byrow = TRUE))[,height:1]
}
