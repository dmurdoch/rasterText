#' @title Build a glyph atlas from strings
#' @description This function accepts strings and builds an atlas of
#' glyphs so those strings can be rendered.  It wraps a
#' a C function used by \pkg{rgl} to draw 3D text.
#' @details `glyphAtlas` starts with a small 32 x 32 pixel atlas and grows it as necessary by doubling both length and
#' width.  When it does this, it rearranges the locations of
#' the glyphs in the buffer, but doesn't change the `glyphnum`
#' values that appear in the `fragments` record.
#' @param text Strings to add to the atlas
#' @param family,font,cex  Graphical parameters for text
#' @param col Color of text (if not `monochrome`)
#' @param monochrome Whether to build a monochrome or full
#' color atlas.
#' @param atlas If not `NULL`, the atlas will be initialized
#' to this value.
#' @returns `glyphAtlas` returns a list of class `"glyph_atlas"`.  The
#' list contains these components:
#' \item{buffer}{A matrix or array holding the buffer of rendered characters.}
#' \item{fonts}{A vector of font descriptions.}
#' \item{glyphs}{A dataframe with one line per rendered glyph.}
#' \item{strings}{A dataframe holding the strings that were rendered.}
#' \item{fragments}{A dataframe holding the map from strings to glyphs.}
#' \item{monochrome}{Is this a monochrome atlas?}
#' \item{position}{Where the next glyph would be written if more are added.}
#' @examples
#' text <- c("Some text", "Tokyo (\u6771\u4eac) \U1F600")
#' atlas_monochrome <- glyphAtlas(text)
#' atlas_monochrome
#' # Each letter appears only once:
#' plot(atlas_monochrome)
#'
#' atlas_color <- glyphAtlas(text, monochrome = FALSE,
#'                           col = c("red", "blue"))
#' # Notice the "o" appears once in each color:
#' plot(atlas_color)
#'
#' atlas_mono2 <- glyphAtlas("hello", atlas = atlas_monochrome)
#' @importFrom systemfonts font_info
#' @importFrom textshaping shape_text
#' @export
glyphAtlas <- function(text, family = "sans", font = 1,
                       cex = 1, col = "black",
                       monochrome = TRUE,
                       atlas = NULL) {
  stopifnot(is.null(atlas) || inherits(atlas, "glyph_atlas"))
  n <- length(text)
  family <- rep_len(as.character(family), n)
  font <- rep_len(as.integer(font), n)
  cex <- rep_len(as.double(cex), n)
  col <- rep_len(col, n)
  monochrome <- as.logical(monochrome)

  rgb <- col2rgb(col, alpha = TRUE)

  .Call(C_build_atlasR, text, family, font, cex, rgb, monochrome, atlas)
}

#' @rdname glyphAtlas
#' @param verbose Whether to give a verbose display.
#' @export
print.glyph_atlas <- function(x, verbose = FALSE, ...) {
  dim <- dim(x$buffer)
  monochrome <- x$monochrome
  if (!monochrome) dim <- dim[-1]
  cat("Glyph atlas:\n")
  if (monochrome)
    cat("  monochrome")
  else
    cat("  color")
  cat(sprintf(" buffer size %d x %d\n", dim[1], dim[2]))
  cat(sprintf("  %d fonts\n", length(x$fonts)))
  if (verbose)
    print(x$fonts)
  cat(sprintf("  %d glyphs\n", nrow(x$glyphs)))
  if (verbose)
    print(x$glyphs)
  cat(sprintf("  %d strings\n", nrow(x$strings)))
  if (verbose) {
    for (i in seq_len(nrow(x$strings))) {
      print(x$strings[i,])
      print(x$fragments[x$fragments$stringnum == i,])
    }
  }
  invisible(x)
}

#' @rdname glyphAtlas
#' @param x The glyph atlas to plot.
#' @param y Ignored.
#' @param interpolate Whether to interpolate pixels.
#' @param ... Extra parameters are passed to the default
#' plot method, but are ignored in the print method.
#' @importFrom graphics par
#' @export
plot.glyph_atlas <- function(x, y, interpolate = FALSE, ...) {
  b <- x$buffer
  if (length(dim(b)) == 2) {
    raster <- as.raster(t(b),
                        max = 255)
  } else
    raster <- as.raster(aperm(b, c(3,2,1)), max = 255)
  save <- par(mar = c(1,1,1,1))
  on.exit(par(save))
  plot(raster, interpolate = interpolate, ...)
}

#' Convert buffer to raster
#'
#' This function converts the buffer component from
#' a \code{\link{glyphAtlas}} result to a raster object
#' that can be plotted in R.
#' @param buffer The buffer component of a glyph atlas.
#'
#' @export
bufferToRaster <- function(buffer) {
  if (length(dim(buffer)) == 2)
    as.raster(t(buffer), max = 255)
  else
    as.raster(aperm(buffer, c(3,2,1)), max = 255)
}

#' @title Render strings from atlas
#' @description
#' The glyph atlas holds one copy of each glyph, and
#' information about where each should be rendered.  This
#' function uses that information to render selected
#' strings.
#'
#' @param atlas The glyph atlas holding the glyphs,
#' produced by \code{\link{glyphAtlas}}.
#' @param num Which glyph numbers to plot?
#' @param x,y The origin at which to plot each string.
#' @param verbose Print information about each string?
#' @param interpolate Smooth the rendering?
#' @param showBaselines Show the baselines for each string?
#' @param ... Additional plot parameters.
#' @importFrom graphics rasterImage segments
#' @export
renderFromAtlas <- function(atlas, num, x = 0, y = 0,
                            verbose = FALSE,
                            interpolate = FALSE,
                            showBaselines = FALSE, ...) {
  xlim <- range(x)
  ylim <- range(y)
  xlims <- list()
  x <- rep_len(x, length(num))
  y <- rep_len(y, length(num))
  frags <- list()
  for (i in seq_along(num)) {
    frags[[i]] <- fragment <- atlas$fragments[atlas$fragments$stringnum == num[i],]
    glyphs <- cbind(fragment, atlas$glyphs[fragment$glyphnum,])
    xlims[[i]] <- range(c(x[i] + glyphs$x_offset + glyphs$x,
                          x[i] + glyphs$x_offset + glyphs$x + glyphs$width))
    xlim <- range(c(xlim, xlims[[i]]))
    ylim <- range(c(ylim,
                    y[i] + glyphs$y_offset - glyphs$y,
                    y[i] + glyphs$y_offset - glyphs$y - glyphs$height))
  }
  plot(xlim, ylim, type = "n", asp = 1, ...)
  raster <- bufferToRaster(atlas$buffer)
  for (i in seq_along(num)) {
    if (verbose) {
      string <- atlas$strings[num[i],]
      font <- atlas$fonts[string$fontnum]
      message("Rendering '", string$text, "' in font ", font)
    }
    fragment <- frags[[i]]
    glyphs <- cbind(fragment, atlas$glyphs[fragment$glyphnum,])
    if (showBaselines)
      segments(xlims[[i]][1], y[i], xlims[[i]][2], y[i])
    for (j in seq_len(nrow(glyphs))) {
      g <- glyphs[j,]
      r <- raster[g$y_atlas + 1:g$height, g$x_atlas + 1:g$width]
      rasterImage(r, xleft = x[i] + g$x_offset + g$x,
                  xright = x[i] + g$x_offset + g$x + g$width,
                  ytop = y[i] + g$y_offset - g$y,
                  ybottom = y[i] + g$y_offset - g$y- g$height,
                  interpolate = interpolate)
    }
  }
}
