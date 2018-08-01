# Unexported functions

#' pad a vector
#'
#' @param x A vector
#' @param n Integer. New length for vector.
#' @param pad Vector. Element or vector with which to pad original vector. Must be a multiple of n.
#'
#' @return vector

vpad <- function(x,n=length(x)+1,pad=x[length(x)]){
  if(n<=length(x)){stop("n must be greater than the length of x")}
  o <- x
  o[(length(x)+1):n] <- pad
  return(o)
}
