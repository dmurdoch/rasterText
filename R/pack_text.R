
#' Pack text
#'
#' @param key Unique key for bitmap, used to detect dups
#' @param measure Result of \code{\link{measure_text}}
#' @param width Width in which to pack items
#' @returns A matrix of xy values, one per entry.  These
#' are the locations where the bitmap should be drawn.
#' @details This function is designed to find a way to
#' place bitmaps of all of the strings in a single
#' raster.
#'
#' If any of the bitmaps are identical, only
#' a single copy will be placed, and the addresses returned
#' will contain duplicate entries.  You can avoid this
#' by specifying unique keys, e.g. \code{seq_along(text)}.
#' @export
#' @examples
#' famnum <- rep(1:3, 8)
#' family <- c("serif", "sans", "mono")[famnum]
#' font <- rep(rep(1:4, each = 3), 2)
#' text <- family
#' m <- measure_text(text, family, font)
#' key <- paste(text, family, font, sep="_")
#' pack_text(key, m, max(m[, "width"]) + 2)
pack_text <- function(key, measure,
                      width = max(measure[,"width"]+2)) {
  result <- .Call(C_pack_textR, as.character(key),
                                as.double(t(measure)),
                                as.integer(width))
  structure(matrix(result, ncol = 2,
         dimnames = list(NULL, c("x","y"))),
                   width = attr(result, "width"),
                   height = attr(result, "height"))
}
