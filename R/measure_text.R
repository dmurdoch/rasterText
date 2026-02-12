
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
#' @export
#' @examples
#' measure_text(c("a", "abc", "j"), family = "serif",
#'              font = 1, cex = 1)
measure_text <- function(text, family,
                         font,
                         fontfile = NULL,
                         cex) {
  text <- enc2utf8(as.character(text))
  n <- length(text)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  if (!is.null(fontfile))
    fontfile <- rep_len(as.character(fontfile), n)
  size <- rep_len(as.double(cex)*20, n)
  result <- .Call(C_measure_textR, text, family, font,
                  fontfile, size)
  matrix(result, ncol = 9, byrow = TRUE,
         dimnames = list(NULL, c("height",
                                 "width",
                                 "x_advance",
                                 "x_bearing",
                                 "y_advance",
                                 "y_bearing",
                                 "ascent",
                                 "descent",
                                 "baseline")))
}
