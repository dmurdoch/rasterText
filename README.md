
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rasterText

<!-- badges: start -->

<!-- badges: end -->

The main goal of `rasterText` is to support drawing text in the `rgl`
package, but it should be usable by other packages that need to render
text to a raster. This can be done through R code or by direct calls to
the underlying C code.

It does just 3 things: given a vector of strings and font descriptions
in the format used by `rgl` (which is very similar to what base R
graphics uses), it can:

1.  Measure the strings in terms of pixels.
2.  Pack the strings into a single rectangular region.
3.  Render the strings into a rectangular raster.

## Installation

You can install the development version of rasterText from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dmurdoch/rasterText")
```

## Example

Here is a simple example:

``` r
library(rasterText)

text <- c("abc", "Tokyo (\u6771\u4eac)", "\u6771\u4eac", "abc")

family <- "serif"
font <- 1
cex <- 3

m <- measure_text(text, family, font, cex = cex)
m
#>      height width x_advance x_bearing y_advance y_bearing ascent descent
#> [1,]     47   103       108         2         0       -46     56      15
#> [2,]     65   359       363         0         0       -52     56      15
#> [3,]     58   117       120         1         0       -55     56      15
#> [4,]     47   103       108         2         0       -46     56      15
#>      baseline
#> [1,]       56
#> [2,]       56
#> [3,]       53
#> [4,]       56
p <- pack_text(text, m, max(m[, "width"]))
p
#>        x  y
#> [1,]  -1 -9
#> [2,]   1 56
#> [3,] 104  0
#> [4,]  -1 -9
#> attr(,"width")
#> [1] 359
#> attr(,"height")
#> [1] 126
raster <- draw_text_to_raster(text, family, font, cex = cex, measure=m, pack=p)
plot(raster)
```

<img src="man/figures/README-example-1.png" width="100%" />
