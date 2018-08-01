#' Display a palette, simulate colorblindness and greyscale, plot Hue Chroma and Lightness
#'
#' `colorbar()` displays a palette as a horizontal colorbar. `colorplot()` displays a palette as a
#' horizontal colorbar and plots its Hue (H), Chroma (C) and Lightness (L). HCL is a perceptually
#' uniform colorspace.
#'
#' Both functions can also simulate greyscale printing and three forms of colorblindness.
#'
#' The input palette must be a character vector of colors as 'hex' RGB or RGBA strings
#' (e.g."#006633" or "#006633FF"), such as produced by most color functions in R.
#'
#' NA's may occur in a palette if it was produced with [grDevices::hcl()] using `fixup=FALSE`. NA's
#' produced by an HCL function are out-of-gamut colors (not possible to display in RGB space). NA's
#' are displayed as white gaps in the colorbar and black verticals in the plots of H, C and L.
#'
#' Although the HCL colorspace is often used to create perceptually uniform color palettes, these
#' palettes are converted to RGB colorspace for display on screen, which introduces artifacts
#' arising from the conversion from high precision (float) to the 8-bit precision of RGB. Since
#' `colorplot()` analyzes the palette *after* it has been converted to RGB for display, it
#' accurately represents the palette as it will be displayed on screen, with any artifacts.
#'
#' @param palette Character vector. A palette (vector of hex RGB or RGBA colors).
#' @param dots Logical. If `dots=TRUE`, a row of circles (dots) will be drawn instead of a colorbar.
#' @param colorblind Logical. If `colorblind=TRUE`, colorbars (or dots) for three common types of
#' colorblindness (deuteranopia, protanopia and tritanopia) and greyscale will also be displayed.
#' @import graphics
#' @import dichromat
#' @importFrom colorspace hex2RGB
#' @importFrom grDevices hcl grey
#' @examples
#' # Draw a colorbar or row of dots
#' pal <- grDevices::rainbow(36)
#' colorbar(pal)
#' colorbar(pal,dots=TRUE)
#' colorbar(pal,colorblind=TRUE)
#' colorbar(pal,dots=TRUE,colorblind=TRUE)
#'
#' # plot colobar (or dots) and HCL for the palette
#' colorplot(pal)
#' colorplot(pal,dots=TRUE)
#' colorplot(pal,colorblind=TRUE)
#' colorplot(pal,dots=TRUE,colorblind=TRUE)
#'
#' @export

# colorbar <- function(palette, dots=FALSE, colorblind=FALSE) {
#   p <- palette
#   if(colorblind){p.concat <- unlist(colorblind(palette), use.names = FALSE)}
#   L <- length(p)
#   par(mar=c(1, 4, 0, 2) + 0.1, oma=c(4,0,0,2))
#
#   if(dots==FALSE){
#     graphics::plot.default(0,0, type="n", axes=FALSE, ann=FALSE,
#                            xlim=c(.5,L+.5),ylim = c(-.5,4.5) #, ylim=c(-.5,.5)
#                            )
#     if(colorblind){
#       graphics::image(1:L,1:4,matrix(1:(L*4),L,4), col=p.concat, add=TRUE)
#       axis(2,las=1,lwd=0,pos=.5,at=1:4,labels = c("normal","deuter.","prot.","trit."))
#     } else {
#       graphics::image(1:L,1,as.matrix(1:L), col=p, add=TRUE)
#     }
#   } else {
#     graphics::plot.default(seq_along(p), type="n",axes=FALSE, ann=FALSE, asp=1,
#                            xlim=c(.5,L+.5), ylim=c(-.5,colorblind*3+0.5)
#                            )
#     if(colorblind){
#       for(i in 0:3){
#         graphics::symbols(add=TRUE,x = 1:L,y = rep_len(i,L),
#                           circles = rep_len(.5,L), inches=FALSE,fg="#00000000",
#                           bg=p.concat[i*L+1:(L+(L*i))])
#       }
#       axis(2,las=1,lwd=0,pos=.5,at=0:3,labels = c("normal","deuter.","prot.","trit."))
#     } else {
#       graphics::symbols(add=TRUE,x = 1:L,y = rep_len(0,L),
#                         circles = rep_len(.5,L), inches=FALSE,fg="#00000000",
#                         bg=p)
#     }
#   }
#   if(anyNA(palette)){warning("Palette contains at least one NA.")}
#   if(length(unique(palette))<length(palette)){warning("Palette may contain duplicate colors.")}
# }

colorbar <- function(palette, dots=FALSE, colorblind=FALSE) {
  p <- palette
  L <- length(p)
  np <- 1

  if(colorblind){
    p.list <- colorblind(p)
    p.concat <- unlist(p.list, use.names = FALSE)
    np <- length(p.list)
  }

  par(mar=c(1, 5, 0, 2) + 0.1, oma=c(4,0,0,2))

  if(dots==FALSE){
    graphics::plot.default(0,0, type="n", axes=FALSE, ann=FALSE,
                           xlim=c(.5,L+.5),ylim = c(-.5,np+.5))
    if(colorblind){
      graphics::image(1:L,1:np,matrix(1:(L*np),L,np), col=p.concat, add=TRUE)
      axis(2,las=1,lwd=0,pos=.5,at=1:np,labels = names(p.list),cex.axis=.8)
    } else {
      graphics::image(1:L,1,as.matrix(1:L), col=p, add=TRUE)
    }
  } else {
    graphics::plot.default(seq_along(p), type="n",axes=FALSE, ann=FALSE, asp=1,
                           xlim=c(.5,L+.5), ylim=c(-.5,colorblind*(np-1)+0.5)) # or (np)
    if(colorblind){
      for(i in 0:(np-1)){
        graphics::symbols(add=TRUE,x = 1:L,y = rep_len(i,L),
                          circles = rep_len(.5,L), inches=FALSE,fg="#00000000",
                          bg=p.concat[i*L+1:(L+(L*i))])
      }
      axis(2,las=1,lwd=0,pos=.5,at=0:(np-1),labels = names(p.list),cex.axis=.8)
    } else {
      graphics::symbols(add=TRUE,x = 1:L,y = rep_len(0,L),
                        circles = rep_len(.5,L), inches=FALSE,fg="#00000000",
                        bg=p)
    }
  }
  if(anyNA(palette)){warning("Palette contains at least one NA.")}
  if(length(unique(palette))<length(palette)){warning("Palette may contain duplicate colors.")}
}

#' @rdname colorbar
#' @export

colorplot <- function(palette,dots=FALSE,colorblind=FALSE) {
  p <- palette
  p.hcl <- hex2hcl(p)
  na.i <- which(is.na(p))
  L <- length(p)
  l.max <- max(p.hcl$L,na.rm=TRUE)
  c.max <- max(p.hcl$C,na.rm=TRUE)

  if(colorblind){
    p.list <- colorblind(p)
    p.deuter.hcl <- hex2hcl(p.list$deuteranopia)
    p.prot.hcl <- hex2hcl(p.list$protanopia)
    p.trit.hcl <- hex2hcl(p.list$tritanopia)
    p.grey.hcl <- hex2hcl(p.list$greyscale)
  }

  # visual variables
  l.inc <- c.inc <- 25
  h.inc <- 120
  l.ticks <- seq(from = 0,
                 to = max(100,l.inc*ceiling(l.max/l.inc)),
                 by = l.inc)
  c.ticks <- seq(from = 0,
                 to = max(100,c.inc*ceiling(c.max/c.inc)),
                 by = c.inc)
  h.ticks <- seq(-360,360,h.inc)
  bg <- grDevices::grey(.98)
  na.col <- "#000000DD"
  pscale <- max(1,16/L)
  xaxis <- seq(1,L,2^max(floor(log2(L))-4,0))
  if(colorblind){legend <- names(p.list)}
  lwd <- c(2,1.5,1.5,1.5,2)
  #lty <- c(1,1,"44","1484")
  lty <- c(1,1,2,4,3)
  line.col <- c("#000000","#00AA0060","#0000FF60","#FF000060","#00000060")
  grid.lwd <- .25
  grid.col <- grDevices::grey(.75)
  grid.lty <- 1

  # init figure
  par(mfrow=c(4,1),mar=c(1, 4, 0, 2) + 0.1, oma=c(4,0,0,2),xaxs="i",yaxs="i")

  # plot colorbar
  colorbar(p,dots = dots,colorblind=colorblind)

  # plot of Lightness
  graphics::plot(seq_len(L+1),ylim=range(l.ticks),type="n",axes=FALSE,ylab="Lightness (L)")
  graphics::rect(1, min(l.ticks), L+1, max(l.ticks), col = bg, border=NA)
  graphics::axis(2,at=l.ticks,labels = l.ticks,las=1)
  graphics::abline(h=l.ticks,lty=grid.lty,lwd=grid.lwd,col=grid.col)
  graphics::abline(v=xaxis,lty=grid.lty,lwd=grid.lwd,col=grid.col)

  graphics::lines(vpad(p.hcl$L),type="s",col=line.col[1],lwd=lwd[1],lty=lty[1])
  # if(dots==TRUE || L<32){graphics::points(p.hcl$L,cex=pscale)}
  if(colorblind==TRUE){
    graphics::lines(vpad(p.deuter.hcl$L),type="s",col=line.col[2],lwd=lwd[2],lty=lty[2])
    graphics::lines(vpad(p.prot.hcl$L),type="s",col=line.col[3],lwd=lwd[3],lty=lty[3])
    graphics::lines(vpad(p.trit.hcl$L),type="s",col=line.col[4],lwd=lwd[4],lty=lty[4])
    graphics::lines(vpad(p.grey.hcl$L),type="s",col=line.col[5],lwd=lwd[5],lty=lty[5])
    graphics::legend("top",horiz=TRUE,xpd=NA,inset=-.2,bty="n",legend = legend,col=line.col,lwd=lwd,lty=lty)
  }
  if(anyNA(palette)){graphics::rect(na.i,min(l.ticks),na.i+1,max(l.ticks),col=na.col,border=NA)}

  # plot of Chroma
  graphics::plot(seq_len(L+1),ylim=range(c.ticks),type="n",axes=FALSE,ylab="Chroma (C)")
  graphics::rect(1, min(c.ticks), L+1, max(c.ticks), col = bg, border=NA)
  graphics::axis(2,at=c.ticks,labels = c.ticks,las=1)
  graphics::abline(h=c.ticks,lty=grid.lty,lwd=grid.lwd,col=grid.col)
  graphics::abline(v=xaxis,lty=grid.lty,lwd=grid.lwd,col=grid.col)

  graphics::lines(vpad(p.hcl$C),type="s",col=line.col[1],lwd=lwd[1],lty=lty[1])
  # if(dots==TRUE || L<32){graphics::points(p.hcl$C,cex=pscale)}
  if(colorblind==TRUE){
    graphics::lines(vpad(p.deuter.hcl$C),type="s",col=line.col[2],lwd=lwd[2],lty=lty[2])
    graphics::lines(vpad(p.prot.hcl$C),type="s",col=line.col[3],lwd=lwd[3],lty=lty[3])
    graphics::lines(vpad(p.trit.hcl$C),type="s",col=line.col[4],lwd=lwd[4],lty=lty[4])
  }
  if(anyNA(palette)){graphics::rect(na.i,min(l.ticks),na.i+1,max(l.ticks),col=na.col,border=NA)}

  # plot of Hue
  graphics::plot(seq_len(L+1),ylim=range(h.ticks),type="n",axes=FALSE,ylab="Hue Angle (H)")
  graphics::rect(1, min(h.ticks), L+1, max(h.ticks), col = bg, border=NA)
  graphics::axis(2,at=h.ticks,labels = h.ticks,las=1)
  graphics::axis(1,at=xaxis,labels = xaxis,las=1,lwd=0,lwd.ticks=1)
  graphics::abline(h=h.ticks,lty=grid.lty,lwd=grid.lwd,col=grid.col)
  graphics::abline(v=xaxis,lty=grid.lty,lwd=grid.lwd,col=grid.col)
  pcolor <- grDevices::hcl(h=p.hcl$H,c=50,l=50)

  for(i in -2:2){graphics::rect(seq_len(L),p.hcl$H+360*i,seq_len(L)+1,p.hcl$H+360*i,border=pcolor,lwd=lwd[1]*2)}
  if(colorblind==TRUE){
    for(i in -2:2){graphics::lines(p.deuter.hcl$H+360*i,type="s",col=line.col[2],lwd=lwd[2],lty=lty[2])}
    for(i in -2:2){graphics::lines(p.prot.hcl$H+360*i,type="s",col=line.col[3],lwd=lwd[3],lty=lty[3])}
    for(i in -2:2){graphics::lines(p.trit.hcl$H+360*i,type="s",col=line.col[4],lwd=lwd[4],lty=lty[4])}
  }

  if(anyNA(palette)){graphics::rect(na.i,min(h.ticks),na.i+1,max(h.ticks),col=na.col,border=NA)}

  # Out-of-gamut warning
  if(anyNA(palette)){
    graphics::mtext(side=1,line=4,cex=0.6,
      text="Palette contains NA's (plotted as black verticals). These may be out-of-gamut colors
      produced by an HCL function with 'fixup=FALSE'. Adjust HCL parameters manually or use 'fixup=TRUE'.")
    }
}

