
#' Draw text to a raster
#'
#' @param text A character string to measure
#' @param family,font,cex Characteristics of the text
#' @param fontfile Freetype font file name, or NULL
#' @param color The color to plot, or \code{NA} for
#' monochrome
#' @param width,height The dimensions of the output raster
#' @param measure,pack Output of \code{\link{measure_text}} and \code{\link{pack_text}} respectively
#'
#' @returns A native raster object with attribute
#' \code{locations}, a matrix giving the locations off each
#'   text string in the raster.
#' @importFrom grDevices col2rgb
#' @importFrom grDevices as.raster
#' @export
#' @examples
#' text <- "Tokyo (\u6771\u4eac) \U1F600"
#' mono <- draw_text_to_raster(text, color = NA)
#' plot(mono)
#' black <- draw_text_to_raster(text, color = "black")
#' plot(black)
draw_text_to_raster <- function(text, family = "sans",
                         font = 1, fontfile = NULL,
                         cex = 1,
                         color = "black",
                         width = max(measure[,"width"]+2),
                         height = attr(pack, "height"),
                         measure = measure_text(text, family, font, fontfile, cex),
                         pack = pack_text(key, measure, width)) {
  monochrome <- is.na(color)
  stopifnot(length(monochrome) == 1)
  if (monochrome)
    col <- as.integer(c(0,0,0,255))
  else
    col <- as.integer(col2rgb(color, alpha = TRUE))
  text <- enc2utf8(as.character(text))
  n <- length(text)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  if (!is.null(fontfile))
    fontfile <- rep_len(as.character(fontfile), n)
  size <- rep_len(as.double(cex)*20, n)

  key <- paste(text, family, font, cex, sep="_")

  storage.mode(pack) <- "integer"
  width <- as.integer(width)
  height <- as.integer(height)
  result <- .Call(C_draw_text_to_rasterR,
                  as.numeric(pack[,1]), as.numeric(pack[,2]),
                  text, family, font,
                  fontfile, size,
                  monochrome, col,
                  width, height)
  if (monochrome)
    result <- as.raster(matrix(result, height, width, byrow = TRUE),
            max = 255)
  else {
    res1 <- aperm(array(result, c(4, width, height)),
                  c(3,2,1))
    result <- as.raster(res1, max = 255)
  }
  structure(result, locations = pack)
}
