#' Convert a hex palette to HCL (polarLUV)
#'
#' Converts a hex color palette to HCL. Returns a dataframe. NA's in palette are preserved.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @return Data frame with three columns: `$H`, `$C`, `$L`
#' @importFrom colorspace hex2RGB
#' @export

hex2hcl <- function(palette){
  p <- palette
  p[is.na(p)] <- "#00000000"
  p.hcl <- methods::as(colorspace::hex2RGB(p), "polarLUV") # import classes?
  p.hcl <- as.data.frame(p.hcl@coords)
  p.hcl[is.na(palette),] <- NA
  return(p.hcl)
}

#' Convert an HCL palette to hex RGB
#'
#' Converts an HCL color palette supplied as a dataframe or individual arguments to an RGB palette.
#' Returns a character vector of hex RGB colors.
#'
#' @param hcl Optional data frame with three columns: `$H`, `$C`, `$L`. (default = `NULL`)
#' @param H Numeric. Hue angle (mod 360).
#' @param C Numeric. Chroma.  Minimum value = 0.  Maximum possible depends on Hue and Lightness (about 100 to 150)
#' @param L Numeric. Lightness (0 to 100)
#' @return Character vector. A palette (vector of hex RGB colors).
#' @importFrom grDevices hcl
#' @export

hcl2hex <- function(hcl=NULL,H=NULL,C=NULL,L=NULL){
  if(!is.null(hcl)){
    palette <- grDevices::hcl(h=hcl$H,c=hcl$C,l=hcl$L,fixup=FALSE)
  } else {
    palette <- grDevices::hcl(h=H,c=C,l=L,fixup=FALSE)
  }
  return(palette)
}

#' Convert named colors to hex RGB
#'
#' Converts named colors to hex RGB colors. Returns a character vector of hex RGB colors.
#'
#' @param color Character vector. A palette (vector of named colors).
#' @return Character vector. A palette (vector of hex RGB colors).
#' @importFrom grDevices rgb col2rgb
#' @export

color2hex <- function(color){
  grDevices::rgb(t(grDevices::col2rgb(color)),maxColorValue = 255)
}

#' Convert named colors to HCL (polarLUV)
#'
#' Converts a hex color palette to HCL. Returns a dataframe. NA's in palette are preserved.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @return Data frame with three columns: `$H`, `$C`, `$L`
#' @export

color2hcl <- function(color){
  hex2hcl(color2hex(color))
}

#' Simulate colorblindness
#'
#' From color palette (character vector of hex RGB or RGBA color codes), simulates the palette as it
#' would appear under three common forms of colorblindness: deuteranopia, protanopia and tritanopia.
#' Returns a list of palettes. NA's in palette are preserved.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @return List of four hex color palettes: `$normal`, `$deuteranopia`, `$protanopia`, `$tritanopia`
#' @import dichromat methods
#' @importFrom colorspace desaturate
#' @export

colorblind <- function(palette) {
  p <- palette
  p[is.na(p)] <- "#00000000"
  p.mat <- cbind(
    normal = palette,
    deuteranopia = dichromat::dichromat(palette,type="deutan"),
    protanopia = dichromat::dichromat(palette,type="protan"),
    tritanopia = dichromat::dichromat(palette,type="tritan"),
    greyscale = colorspace::desaturate(palette)
  )
  p.mat[is.na(palette),] <- NA
  p.list <- split(p.mat, col(p.mat))
  names(p.list) <- colnames(p.mat)
  return(p.list)
}

#' Get built-in color names with Hex, RGB and HCL values
#'
#' Works like `grDevices::colors()` but returns a dataframe of Hex RGB codes, integer red, green, and blue
#' values, and the corresponding H, C, and L values (in HCL color space) for all color names.
#'
#' @param distinct logical indicating if the colors returned should all be distinct;
#' e.g., "snow" and "snow1" are synonyms with the same RGB values.
#' @return Data frame with 1 row per color and 7 variables:
#' \item{hex}{Hexidecimal RGB color string}
#' \item{red}{Red value (0 to 255)}
#' \item{green}{Green value (0 to 255)}
#' \item{blue}{Blue value (0 to 255)}
#' \item{L}{Lightness value (0.0 to 100.0)}
#' \item{C}{Chroma value (0.0 to ~150.0). Note: Maximum C depends on L and H.}
#' \item{H}{Hue angle (0.0 to 360.0)}
#'
#' @importFrom colorspace desaturate
#' @export

colorchart <- function(distinct = FALSE){
  color.names <- colors(distinct)
  color.rgb <- t(grDevices::col2rgb(color.names))
  color.rgb <- t(grDevices::col2rgb(color.names))
  color.hex <- grDevices::rgb(color.rgb,maxColorValue = 255)
  color.hcl <- hex2hcl(color.hex)

  color.df <- data.frame(hex=color.hex,stringsAsFactors = FALSE)
  color.df <- cbind(color.df,color.rgb,color.hcl)

  row.names(color.df) <- color.names
  return(color.df)
}
