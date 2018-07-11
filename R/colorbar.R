#' Display a palette as a color bar.
#'
#' This function plots a palette as a horizontal colorbar.
#'
#' @param p A palette (vector of colors).
#' @param dots Logical. If `circles=TRUE`, a row of circles will be drawn instead of a bar.
#' @import graphics
#' @export
colorbar <- function(p, dots=FALSE) {
  L <- length(p)
  if(dots==FALSE){
    plot.default(0,0, type="n", axes=FALSE, ann=FALSE, asp=1, xlim=c(0.5,length(p)+.5), ylim=c(-.5,.5))
    image(1:L,1,as.matrix(1:L), axes=FALSE, ylab = "", xlab="", col=p)
  } else {
    plot.default(0,0, type="n", axes=FALSE, ann=FALSE, asp=1, xlim=c(0.5,length(p)+.5), ylim=c(-.5,.5))
    symbols(add=TRUE,
      x = 1:length(p),
      y = rep_len(0,length(p)),
      circles = rep_len(.5,length(p)), inches=FALSE,
      bg=p, fg=rgb(0,0,0,0)
    )
  }
}
