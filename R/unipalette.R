#' A 7 category qualitative palette
#'
#' `unipalette()` produces qualitative palettes based on a preset 7-color palette optimized for
#' lines, points, bars, etc, on a white background.
#'
#' Base palette, in default ordering:
#'
#' #' \tabular{lllllll}{
#' red \tab lightblue \tab black \tab green \tab purple \tab pink \tab blue \cr
#' "#a11c3e"\tab"#5798d1"\tab"#252525"\tab"#319045"\tab"#5e2b7b"\tab"#e2908c"\tab"#226e83"
#' }
#'
#' Palette characteristics:
#'
#' * designed for high contrast against a white background, with good distinction among colors
#' * colors distinguishable under two most common forms of colorblindness
#' * colors distinguishable in greyscale for b/w printing
#' * Good for bar charts, box plots, pies, etc.
#' * Line should be relatively thick
#' * Points should be relatively large
#'
#' The default ordering (`order="difference"`) maximizes differences in hue and lightness between adjacent
#' colors. This aids discrimination of data plotted side by side, and also helps avoid implications
#' that the data is ordinal.
#'
#' Two alternate orderings are also provided.
#'
#' * `unipalette(order="lightness")` orders the color by increasing perceived lightness:
#'    black    purple       red      blue     green lightblue      pink
#' * `unipalette(order="hybrid")` increases the pairwise lightness difference in an otherwise
#'    lightness-ordered palette:
#'    black       red    purple     green      blue      pink lightblue
#'
#' Palette subsets
#'
#' `unipalette(4)` returns the first 4 colors of the default ordering.
#' `unipalette(2:5)` returns the colors 2 through 5 of the default ordering.
#' `unipalette(c("red","lightblue","purple"))` returns the named colors in the order given.
#'
#' @param colors Integer vector or character vector. `colors=4` returns the first 4 colors of the
#' default ordering. `colors=2:5` returns the colors 2 through 5 of the default ordering.
#' `colors=c("red","lightblue","purple")` returns the named colors in the order given.
#' @param order Character. `"difference"`, `"lightness"`, or `"hybrid"`. The ordering for the
#' palette (see details).
#'
#' @return character vector. A vector of named RGB colors specified as hex color codes (#RRGGBB).
#' @examples
#' ## 3 orderings:
#' difference <- unipalette(); colorbar(diff,dots=TRUE)
#' lightness <- unipalette(order="lightness"); colorbar(lightness,dots=TRUE)
#' hybrid <- unipalette(order="hybrid"); colorbar(hybrid,dots=TRUE)
#'
#' ## subsets
#' p <- unipalette(4); colorbar(p,dots=TRUE)
#' @export

unipalette <- function(colors = NULL, order="difference") {
  orders <- c("difference", "lightness", "hybrid")
  o <- grep(paste0("^",order),orders,value=TRUE)
  if (!o %in% orders) {
    o <- "lightness"
    warning('order must be one of "lightness", "difference", "hybrid". Using "lightness".')
  }
  lightness <- c(
    "black"="#252525", # Nero (grey)
    "purple"="#5e2b7b", # Blue Diamond (violet)
    "red"="#a11c3e", # Fire Brick (red)
    "blue"="#226e83",  # Allports (blue)
    "green"="#319045", # Sea Green (green)
    "lightblue"="#5798d1", # Picton Blue (blue)
    "pink"="#e2908c" # Sea Pink (red)
  )
  difference <- lightness[c(3,6,1,5,2,7,4)]
  hybrid <- lightness[c(1,3,2,5,4,7,6)]

  p <- eval(parse(text=o))

  if (is.null(colors)) {
    return(p)
  }

  if (is.numeric(colors)) {
    if (length(colors)==1) {c <- 1:colors} else {c <- colors}
    return(p[c])
  }

  if (is.character(colors)) {
    c <- NULL
    for (i in 1:length(colors)) {
      c[i] <- grep(paste0("^",colors[i]),names(p),value=TRUE)[1]
    }
    if (anyNA(c)) {
      warning('Invalid color(s) specified. Outputting all colors.')
      return(p)
    }
    return(p[names(p) %in% c == TRUE])
  }

  warning("Colors must be specified as numeric indeces or strings. Outputting all colors.")
  return(p)
}
