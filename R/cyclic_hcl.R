# Depricated - this function has been replaced with helper functions
# cyclx(), rampx() and stepx() to be used with grDevices:hcl()
#
#' Cyclic HCL Color Palettes
#'
#' Depricated - this function has been replaced with helper functions
# cyclx(), rampx() and stepx() to be used with grDevices:hcl()
#'
#' `cyclic_hcl()` constructs palettes combining end-to-end linear interpolation with cycles in hue
#' (H), lightness (L) and or chroma (C).
#'
#' Palette characteristics:
#'
#' * perceptually linear H, C and L gradients
#' * Stepped H, C and/or L, with perceptually linear gradients within each step
#' * Good for data that combines cyclic and continuous aspects (like seasonal data), or for
#' increased visual differentiation among levels.
#'
#' @param n.cycles Numeric. Number of cycles, or bins. Default: `10`
#' @param cycle.lenth Integer. Number of colors per cycle or bin. Default: `100`
#' @param h Numeric. Hue (0 to 360); constant or 2-vector giving palette endpoints. Default:
#'   `c(0,-100)`
#' @param c Numeric. Chroma (0 to ~100); constant or 2-vector giving palette endpoints. Default:
#'   `100`
#' @param l Numeric. Lightness (0 to ~100); constant or 2-vector giving palette endpoints. Default:
#'   `100`
#' @param cycle.h Numeric. Hue offset (0 to 360); constant or 2-vector giving cycle endpoints.
#'   Default: `0`
#' @param cycle.c Numeric. Chroma; constant or 2-vector giving cycle endpoints. Default: `c(50,50)`
#' @param cycle.l Numeric. Lightness; constant or 2-vector giving cycle endpoints. Default:
#'   `c(75,25)`
#' @param alpha Numeric. Opacity; constant. Default: `1`. (Note: `alpha` applies to enire palette
#'   for now. In future releases, alpha cycling will be allowed.)
#' @param h.power Numeric. Default: `1`
#' @param c.power Numeric. Default: `1`
#' @param l.power Numeric. Default: `1`
#' @param cycle.h.power Numeric. Default: `1`
#' @param cycle.c.power Numeric. Default: `1`
#' @param cycle.l.power Numeric. Default: `0.5`
#' @param terminate Logical. If TRUE the palette is closed (left and right closed) by
#'   adding a single color at either the left or right end . If right = TRUE, then terminate = TRUE
#'   adds a color before the first cycle. If right = FALSE, then terminate = TRUE adds a color after
#'   the last cycle. If `terminate=FALSE` the palette remains open at one end. Default: `TRUE`
#' @param cycle.right Logical. If `cycle.right=TRUE`, the cycles are considered right-closed (left
#'   open) intervals. This only has a practical effect if `terminate = TRUE`. Default: `TRUE`.
#' @param stepped Logical. Not used. Default: `FALSE`. Future releases will allow for stepped vs
#'   cyclic transitions.
#' @param fixup Logical. Out-of-gammut colors are converted to valid RGB colors if `fixup=TRUE`, and
#'   `NA` strings if `fixup=FALSE`. Default: `FALSE`.
#'
#' @return Character vector. A vector of RGB colors specified as hex color codes (#RRGGBB).
#' @importFrom colorspace hex polarLUV
#' @importFrom utils tail
#' @examples
#' ## Palette with linear hue, cyclical lightness, constant chroma.
#' palette.linH.cycL  <- cyclic_hcl()
#' colorbar(palette.linH.cycL)
#'
#' @export

cyclic_hcl <- function(
  n.cycles = 10,
  cycle.lenth = 100,
  h = c(0,-100),
  c = 100,
  l = 100,
  cycle.h = 0,
  cycle.c = 50,
  cycle.l = c(75,25),
  alpha = 1,
  h.power = 1,
  c.power = 1,
  l.power = 1,
  cycle.h.power = 1,
  cycle.c.power = 1,
  cycle.l.power = .5,
  terminate = TRUE,
  cycle.right = TRUE,
  stepped = FALSE,
  fixup = FALSE
){
  # convert all to 2-vectors
  h <- rep(h, length.out = 2L)
  c <- rep(c, length.out = 2L)
  l <- rep(l, length.out = 2L)
  cycle.h <- rep(cycle.h, length.out = 2L)
  cycle.c <- rep(cycle.c, length.out = 2L)
  cycle.l <- rep(cycle.l, length.out = 2L)

  # Calculate H, C & L main sequences and cycle sequences
  palette.length <- (n.cycles*cycle.lenth)+terminate
  cycle.seq <- seq(0,1,len=cycle.lenth)
  palette.seq <- seq(0,1,len=palette.length)

  palette.c.seq.d <- .01*(c[2L] - diff(c)*rev(palette.seq)^c.power[1L])
  palette.l.seq.d <- .01*(l[2L] - .01*diff(l)*rev(palette.seq)^l.power[1L])
  palette.h.seq <- h[2L] - diff(h)*rev(palette.seq)^h.power[1L]

  cycle.c.seq.d <- .01*(cycle.c[2L] - diff(cycle.c)*rev(cycle.seq)^cycle.c.power[1L])
  cycle.l.seq.d <- .01*(cycle.l[2L] - diff(cycle.l)*rev(cycle.seq)^cycle.l.power[1L])
  cycle.h.seq <- cycle.h[2L] - diff(cycle.h)*rev(cycle.seq)^cycle.h.power[1L]

  c.seq.d <- rep(cycle.c.seq.d,n.cycles)
  l.seq.d <- rep(cycle.l.seq.d,n.cycles)
  h.seq <- rep(cycle.h.seq,n.cycles)

  # Terminate sequences
  if(terminate==TRUE){
    if(cycle.right==TRUE){
      c.seq.d <- c(utils::tail(c.seq.d,1),c.seq.d)
      l.seq.d <- c(utils::tail(l.seq.d,1),l.seq.d)
      h.seq <- c(utils::tail(h.seq,1),h.seq)
    }else{
      c.seq.d <- c(c.seq.d,c.seq.d[1])
      l.seq.d <- c(l.seq.d,l.seq.d[1])
      h.seq <- c(h.seq,h.seq[1])
    }
  }

  # Multiply main C & L sequences and cycles; Add main H sequence and H cycles.

  palette <- colorspace::hex(
    colorspace::polarLUV(
      L = 100*l.seq.d*palette.l.seq.d,
      C = 100*c.seq.d*palette.c.seq.d,
      H = h.seq+palette.h.seq),
    fixup = fixup)

  if (anyNA(palette)) {
    warning(cat("At least one out-of-gammut color has been converted to NA. Either set fixup=TRUE or adjust parameters manually. \n\n"))
  }
  if (fixup==TRUE) {
    warning(cat("Out-of-gammut colors may have been converted to in-gamut colors. Palette may have visible artifacts. Consider setting 'fixup=FALSE' and adjust HCL parameters until no warning is returned. \n\n"))
  }
  return(palette)
}





