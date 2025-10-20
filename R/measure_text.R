
#' Measure some text (in pixels)
#'
#' @param text A character string to measure
#'
#' @returns A matrix of measurements, one row per entry.
#' Columns are
#' \describe{
#' \item{x_bearing, y_bearing}{The displacement from the starting point to the upper left corner of the bounding box.}
#' \item{width, height}{Size of the bounding box.}
#' \item{x_advance, y_advance}{Suggested reference point for
#' a following string.}}
#' @export
#'
#' @examples
#' measure_text(c("a", "abc", "j"))
measure_text <- function(text) {
  text <- enc2utf8(as.character(text))
  result <- .Call("measure_text", text, PACKAGE = "rasterText")
  matrix(result, ncol = 6,
         dimnames = list(NULL, c("x_bearing",
                                 "y_bearing",
                                 "width",
                                 "height",
                                 "x_advance",
                                 "y_advance")))
}
