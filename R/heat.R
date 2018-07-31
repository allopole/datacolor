#' A sequential color palette for heatmaps.
#'
#' This function returns a sequential red-to-yellow palette of length `n`, suitable as a replacement
#' for [grDevices::heat.colors] or [colorspace::heat_hcl].
#'
#' Palette characteristics:
#'
#' * perceptually linear
#' * color-blind-safe (under three most common forms of colorblindness) and print friendly
#' * under normal vision: linear lightness, chroma, and hue gradients
#' * under colorblind viewing, linear lightness, smooth chroma, flat hue gradients
#'
#' @param n Number of levels in palette (number of bins).  Default: `n = 10`.
#' @return character vector. A vector of RGB colors specified as hex color codes (#RRGGBB).
#' @examples
#' colorplot(heat(128))
#' @export

heat <- function(n=10){
  pal <- hcl2hex(H=rampx(4,86,n=n),
                 L=rampx(29,98.03,n=n),
                 C=rampx(83,75,n=n))
  return(pal)
}
