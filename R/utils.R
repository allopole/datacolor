#' Convert a hex palette to HCL (polarLUV)
#'
#' Converts a hex color palette to HCL.  Returns a dataframe. NA's are preserved.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @importFrom colorspace hex2RGB
#' @import dichromat

hex2hcl <- function(palette){
  p <- palette
  p[is.na(p)] <- "#00000000"
  p.hcl <- as(colorspace::hex2RGB(p), "polarLUV") # import classes?
  p.hcl <- as.data.frame(p.hcl@coords)
  p.hcl[is.na(palette),] <- NA
  # p.hcl@coords[is.na(palette),] <- NA
  return(p.hcl)
}

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
  # p.colorblind <- as.data.frame(p.colorblind,stringsAsFactors = FALSE)
  p.list <- split(p.mat, col(p.mat))
  names(p.list) <- colnames(p.mat)
  return(p.list)
  # return(as.data.frame(p.colorblind))
}

vpad <- function(x,n=length(x)+1,pad=x[length(x)]){
  o <- x
  o[(length(x)+1):n] <- pad
  return(o)
}
