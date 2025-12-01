
#' Pack text
#'
#' @param measure Result of `measure_text()`
#' @param width Width in which to pack items
#' @returns A matrix of xy values, one per entry
#' @export
#' @examples
#' m <- measure_text(c("a", "abc", "j"), family = "serif",
#'              font = 1, cex = 1)
#' pack_text(m, max(m[, "width"]) + 2)
pack_text <- function(measure, width) {
  result <- .Call(C_pack_textR, as.double(t(measure)),
                                as.integer(width))
  structure(matrix(result, ncol = 2,
         dimnames = list(NULL, c("x","y"))),
                   width = attr(result, "width"),
                   height = attr(result, "height"))

}
