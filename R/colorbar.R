#' Display a palette as a color bar.
#'
#' This function plots a palette as a horizontal colorbar.
#'
#' @param p A palette (vector of colors).
#' @export
colorbar <- function(p) {
  L <- length(p)
  image(1:L,1,as.matrix(1:L),axes=FALSE, ylab = "", xlab="", col=p)
}
