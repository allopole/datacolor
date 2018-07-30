#' A diverging color palette for AUC (Area Under ROC Curve).
#'
#' This function returns a palette suitable for a heatmap of AUC (area under the ROC or Receiver
#' Operating Characteristic). The palette is appropriate for any scale with equal positive and
#' negative ranges above and below a central value.  For AUC, the central value is 0.5, and the min
#' and max are 0. and 1.  More positive values map to bluer colors, more negative values map to
#' yellower colors.
#'
#' Palette characteristics:
#'
#' * color-blind-safe (under three most common forms of colorblindness)
#' * perceptually linear
#' * equal brightness gradients on both sides of central value
#' * constant hue and chroma on either side of central value
#' * Dark value for no correlation (near 0.5); bright values for high correlation (near 1 and 0)
#' * Blue for positive correlation (1); orange for negative correlation (0)
#' * NOT suitable for b/w printing, as low and high values are not distinguishable in greyscale
#'
#' @param n Number of levels in palette (number of bins).  Default: `n = 10`. Given error of `~0.5`,
#'   `n = 10` is recommended. For a higher precision, use `n = 20`. If `n` is odd, the central bin
#'   (encompassing the central value) will be a shade of grey.
#' @param invert If `invert = TRUE` palette edges will be dark, and center will be light. Else,
#'   center is dark and edges are light.
#' @param reverse If `reverse = TRUE` left side colors (more negative values) will be blue and right
#'   side colors will be yellow. Else, left (more negative) is yellow and right (more positive) is
#'   blue.
#' @return character vector. A vector of RGB colors specified as hex color codes (#RRGGBB).
#' @importFrom colorspace diverge_hcl
#' @examples
#' p <- AUColors()
#' p
#' colorbar(p)
#'
#' # Sample AUC Heat Map
#' randommatrix <- matrix(runif(12*6),ncol=6)
#' nlevels <- 10
#' levelplot(randommatrix,
#'   at = seq(0,1,1/nlevels), # number of breaks
#'   col.regions = AUColors(), # color map
#'   aspect = "iso")
#'
#' ## inverted and reversed, with grey center bin:
#' p <- AUColors(n=21,invert=TRUE,reverse=TRUE)
#' colorbar(p)

#' @export

AUColors <- function (n=10, invert=FALSE, reverse=FALSE) {
  if(reverse!=FALSE) {h <- c(225, 45)} else {h <- c(45, 225)}
  if(invert!=FALSE) {
    l <- c(26,97)
    c <- 26
  } else {
    l <- c(78, 3)
    c <- 74
  }
  colorspace::diverge_hcl(n=n, h=h, c=c, l=l, power=1, fixup=FALSE)
}
