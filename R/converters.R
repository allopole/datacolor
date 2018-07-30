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
  p.hcl <- as(colorspace::hex2RGB(p), "polarLUV") # import classes?
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
#' @param H Numeric. Lightness (0 to 100)
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

#' Simulate colorblindness
#'
#' From color palette (character vector of hex RGB or RGBA color codes), simulates the palette as it
#' would appear under three common forms of colorblindness: deuteranopia, protanopia and tritanopia.
#' Returns a list of palettes. NA's in palette are preserved.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @return List of four hex color palettes: `$normal`, `$deuteranopia`, `$protanopia`, `$tritanopia`
#' @import dichromat
#' @export

colorblind <- function(palette) {
  p <- palette
  p[is.na(p)] <- "#00000000"
  p.mat <- cbind(
    normal = palette,
    deuteranopia = dichromat::dichromat(palette,type="deutan"),
    protanopia = dichromat::dichromat(palette,type="protan"),
    tritanopia = dichromat::dichromat(palette,type="tritan")
  )
  p.mat[is.na(palette),] <- NA
  p.list <- split(p.mat, col(p.mat))
  names(p.list) <- colnames(p.mat)
  return(p.list)
}
