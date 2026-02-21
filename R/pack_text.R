
#' Pack text
#'
#' @param key Unique key for bitmap, used to detect dups
#' @param measure Result of `measure_text()`
#' @param width Width in which to pack items
#' @returns A matrix of xy values, one per entry
#' @export
#' @examples
#' famnum <- rep(1:3, 8)
#' family <- c("serif", "sans", "mono")[famnum]
#' font <- rep(rep(1:4, each = 3), 2)
#' cex <- rep(1:2, each = 12)
#' m <- measure_text(family, family, font, NULL, cex)
#' key <- paste(family, font, cex, sep="_")
#' pack_text(key, m, max(m[, "width"]) + 2)
pack_text <- function(key, measure, width) {
  result <- .Call(C_pack_textR, as.character(key),
                                as.double(t(measure)),
                                as.integer(width))
  structure(matrix(result, ncol = 2,
         dimnames = list(NULL, c("x","y"))),
                   width = attr(result, "width"),
                   height = attr(result, "height"))

}
