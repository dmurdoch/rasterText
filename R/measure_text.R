
#' Measure some text (in pixels)
#'
#' @param text A character string to measure
#' @param family,font,cex Characteristics of the text
#'
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
                         cex = par3d("cex")) {
  text <- enc2utf8(as.character(text))
  family <- as.character(family)
  font <- as.integer(font) - 1L
  size <- as.double(cex)*20
  result <- .Call("measure_text", text, family, font,
                  size, PACKAGE = "rasterText")
  matrix(result, ncol = 6,
         dimnames = list(NULL, c("x_bearing",
                                 "y_bearing",
                                 "width",
                                 "height",
                                 "x_advance",
                                 "y_advance")))
}
