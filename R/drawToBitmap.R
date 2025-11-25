idToBitmap <- function(id, ...) {
  stopifnot(length(id) == 1)

  texts <- c(rgl.attrib(id, "texts"))
  if (!length(texts))
    return(NULL)

  cex <- c(rgl.attrib(id, "cex"))
  family <- c(rgl.attrib(id, "family"))
  font <- c(rgl.attrib(id, "font"))

  drawToBitmap(texts, cex = cex, family = family, font = font, ...)
}

# This function computes ascent and descent of each of a vector
# of strings using textshaping::shape_text().
# (systemfonts::shape_string() aborts).  shape_text()
# alone gives incorrect results when some of the string
# entries include glyphs that are not in the specified font;
# this function recursively uses systemfonts::font_fallback()
# to do the correct calculation for the whole thing.

bbox_with_subs <- function(strings, family = "",
                           italic = FALSE,
                           bold = FALSE,
                           size = 12,
                           path = NULL, index = 0,
                           ...) {

  # Function to make an empty result dataframe

  ascent_descent_df <- function(n)
    data.frame(string = character(n), width = numeric(n),
               ascent = numeric(n), descent = numeric(n),
               pen_x = numeric(n))

  # Function to compute ascent and descent from the shape_text() results

  extract_ascent_descent <- function(metrics, size)
    with(metrics,
         data.frame(string = string,
                    width = width,
                    ascent = top_border - top_bearing,
                    descent = height - 2*top_border - bottom_bearing,
                    pen_x = pen_x))

  # Try shape_text() on the whole vector.  If no glyphs are
  # missing, trust the result

  shape0 <- shape_text(strings, family = family, path = path, index = index, italic = italic, bold = bold, size = size, ...)

  shape <- shape0$shape
  if (all(shape$index > 0))
    result <- extract_ascent_descent(shape0$metrics, size)

  else {
    # Some glyphs are missing.

    # We're going to be subsetting the strings, so all vectors
    # should be the same length

    n <- length(strings)
    stopifnot(nrow(shape0$metrics) == n)

    family <- rep_len(family, n)
    italic <- rep_len(italic, n)
    bold <- rep_len(bold, n)
    size <- rep_len(size, n)
    if (!is.null(path)) path <- rep_len(path, n)
    index <- rep_len(index, n)

    result <- ascent_descent_df(n)

    missings <- unique(shape$metric_id[shape$index == 0])
    nonmissings <- setdiff(unique(shape$metric_id), missings)

    # The nonmissings are fine; just extract those results
    if (length(nonmissings))
      result[nonmissings, ] <- extract_ascent_descent(shape0$metrics[nonmissings,], size[nonmissings])

    # For the missings, we need to find fallbacks.  Do them
    # one at a time.

    for (m in missings) {
      # Choose the glyph entries corresponding to this string
      rows <- which(shape$metric_id == m)

      # Some glyphs are missing.  Break the string up into
      # sequences of non-missing and missing glyphs

      parts <- rle(shape$index[rows] == 0)
      lens <- parts$lengths
      starts <- c(1, 1 + cumsum(lens)[-length(lens)])
      subs <- parts$values
      indx <- seq_along(parts$lengths)
      parts <- substring(shape0$metrics$string[m],
                         starts, starts + lens - 1)

      n0 <- length(parts)
      result0 <- ascent_descent_df(n0)

      # Redo shape_text() on the parts with non-missing glyphs,
      # and save those results
      if (!all(subs)) {
        shape1 <- shape_text(parts[!subs],
                                          family = family[m],
                                          italic = italic[m],
                                          bold = bold[m],
                                          size = size[m],
                                          path = if (!is.null(path)) path[m],
                                          index = index[m], ...)


        result0[!subs, ] <- extract_ascent_descent(shape1$metrics, size[m])
      }

      # Find the fallback fonts for missing parts, and use them
      # in a recursive call

      fallback <- font_fallback(parts[subs],
                                             family = family[m],
                                             italic = italic[m],
                                             bold = bold[m],
                                             path = if (!is.null(path)) path[m],
                                             index = index[m])
      result0[subs, ] <- bbox_with_subs(parts[subs], path = fallback$path, index = fallback$index, size = size[m])

      # Combine the results from the parts of the string
      # back into a single result for the whole string
      result[m, ] <- data.frame(string = shape0$metrics[m, "string"],
                                width = sum(result0$pen_x[-n0]) + result0$width[n0],
                                ascent = max(result0$ascent),
                                descent = max(result0$descent),
                                pen_x = sum(result0$pen_x))
    }
  }
  result
}

getPowerOfTwo <- function(n) {
  2^ceiling(log(n, 2))
}

#' Draw text to bitmap
#'
#' @param texts Text to draw.
#' @param cex, family, font Text characteristics.
#' @param background Background colour.
#' @param powerOfTwo Round bitmap size up to power of two dimensions.
#' @param verbose Print progress.
#' @param showBaselines Show text baselines (for debugging).
#' @param onePerLine Put each bitmap on a new line.
#' @param minWidth, minHeight Minimum bitmap measurements.
#'
#' @returns A raster object containing the output.
#'
#' @examples
#' text <- c("abc", "Tokyo (\u6771\u4eac)", "\u6771\u4eac", "abc")
#' bm <- drawToBitmap(text, cex = 3, family = "sans", font = 1,
#'                   powerOfTwo = FALSE,
#'                   showBaselines = TRUE, onePerLine = TRUE)
#' plot.new()
#' rasterImage(bm, 0, 0, 1, 1)
#' dim(bm)
#' attr(bm, "metrics")
#'
drawToBitmap <- function(texts, cex = par3d("cex"),
                         family = par3d("family"),
                         color = material3d("color")[1],
                         font = par3d("font"),
                         background = "transparent",
                         powerOfTwo = TRUE,
                         verbose = FALSE,
                         showBaselines = FALSE,
                         onePerLine = FALSE,
                         minWidth = 0,
                         minHeight = 0) {

  if (!length(texts))
    return(NULL)

  df0 <- data.frame(texts = texts, cex = cex, family = family, font = as.integer(font))
  keys0 <- paste(texts, cex, family, font, sep = "_")

  uniq <- !duplicated(keys0)
  df <- df0[uniq, ]
  keys <- keys0[uniq]
  n <- length(keys)

  texts <- df$texts
  cex <- df$cex
  family <- df$family
  font <- df$font
  italic <- font %in% 3:4
  bold <- font %in% c(2, 4)

  # Measure the text

  # measures <- bbox_with_subs(texts, family = family, italic = italic,
  #                            bold = bold, size = cex*12)

  measures <- measure_text(text = texts, family = family, font = font, cex = cex)
  if (verbose)
    print(measures)

  # Change to integers with a measure of safety
  measures <- sign(measures)*ceiling(abs(measures))

  bmWidth <- max(measures[,"width"], minWidth)
  if (powerOfTwo)
    bmWidth <- getPowerOfTwo(bmWidth)

  # The + 1 values here and in the y[i] calculation below are
  # there just to give a measure of safety

  heights <- measures[,"height"]

  # Try to pack them in with several on each line.  Order
  # by increasing height so lines don't have too much white
  # space on top.
  x <- y <- numeric(n)
  h0 <- x0 <- y0 <- 0
  o <- order(heights)
  for (i in o) {
    width <- measures[i,"width"]
    if (x0 + width > bmWidth || onePerLine) {
      y0 <- y0 + h0
      h0 <- x0 <- 0
    }
    x[i] <- x0
    x0 <- x0 + width
    y[i] <- y0 - measures[i, "y_bearing"]
    h0 <- max(h0, heights[i])
  }
  bmHeight <- max(y0 + h0, minHeight)
  if (powerOfTwo)
    bmHeight <- getPowerOfTwo(bmHeight)

  if (verbose)
    cat("width=", bmWidth, " height=", bmHeight, "\n")

  # getraster <- agg_capture(width = bmWidth, height = bmHeight, units = "px", background = background)
  # on.exit(dev.off())

  # par(mar = c(0,0,0,0))
  # plot.new()
  # plot.window(xlim = c(0, bmWidth),
  #             ylim = c(0, bmHeight),
  #             xaxs = "i", yaxs = "i")
  #
  # for (fam in unique(family)) {
  #   gp <- fam == family
  #   text(x[gp], y[gp], texts[gp], adj = c(0,0),
  #        cex = cex[gp], font = font[gp], family = fam)
  # }

  rgba <- rep_len(as.numeric(col2rgb(color, alpha = TRUE))/255,
                  4*n)
  fontfile <- NULL
  size <- rep_len(as.double(cex)*20, n)
  result <- .Call(C_draw_text_to_raster, x, y, texts, rgba, family, font,
                  fontfile, size,
                  as.integer(bmWidth), as.integer(bmHeight))
  result <- matrix(result, bmHeight, bmWidth, byrow = TRUE)/255

  df$x <- x
  df$y <- y
  df$width <- measures[,"width"]
  df$height <- measures[,"height"]

  df <- df[match(keys0, keys),]

  structure(result, metrics = df)
}
