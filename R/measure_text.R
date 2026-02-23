
#' Measure some text (in pixels)
#'
#' @param text A character string to measure
#' @param family,font,cex Characteristics of the text
#' @param fontfile Freetype font file name, or NULL
#' @returns A matrix of measurements, one row per entry.
#' Columns are
#' \describe{
#' \item{height, width}{Size of the bounding box for the string}
#' \item{x_bearing, y_bearing}{The displacement from the starting point to the upper left corner of the bounding box.}
#' \item{x_advance, y_advance}{Suggested reference point for
#' a following string.}
#' \item{ascent, descent}{Amount the fonts used for each
#' string rise above or descend below the baseline.}
#' \item{baseline}{How far from the baseline to the top of the bounding box. }
#' }
#' @export
#' @examples
#' famnum <- rep(1:3, 8)
#' family <- c("serif", "sans", "mono")[famnum]
#' font <- rep(rep(1:4, each = 3), 2)
#' cex <- rep(1:2, each = 12)
#' measure_text(family, family, font, NULL, cex)
measure_text <- function(text, family = "sans",
                         font = 1,
                         fontfile = NULL,
                         cex = 1) {
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
