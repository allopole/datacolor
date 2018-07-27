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
