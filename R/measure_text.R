
#' Measure some text (in pixels)
#'
#' @param text A character string to measure
#' @param family,font,cex Characteristics of the text
#' @param fontfile Freetype font file name, or NULL
#' @returns A matrix of measurements, one row per entry.
#' Columns are
#' \describe{
#' \item{x_bearing, y_bearing}{The displacement from the starting point to the upper left corner of the bounding box.}
#' \item{width, height}{Size of the bounding box.}
#' \item{x_advance, y_advance}{Suggested reference point for
#' a following string.}}
#' @importFrom rgl par3d
#' @export
#' @examples
#' measure_text(c("a", "abc", "j"), family = "serif")
measure_text <- function(text, family = par3d("family"),
                         font = par3d("font"),
                         fontfile = NULL,
                         cex = par3d("cex")) {
  text <- enc2utf8(as.character(text))
  n <- length(text)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  if (!is.null(fontfile))
    fontfile <- rep_len(as.character(fontfile), n)
  size <- rep_len(as.double(cex)*20, n)
  result <- .Call("measure_text", text, family, font,
                  fontfile, size, PACKAGE = "rasterText")
  matrix(result, ncol = 6,
         dimnames = list(NULL, c("x_bearing",
                                 "y_bearing",
                                 "width",
                                 "height",
                                 "x_advance",
                                 "y_advance")))
}
