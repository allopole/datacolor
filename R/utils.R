# Unexported functions

# pad a vector
# @param x A vector
# @param n Integer. New length for vector.
# @param pad Character vector. A palette (vector of hex RGB or RGBA colors).

vpad <- function(x,n=length(x)+1,pad=x[length(x)]){
  if(n<=length(x)){error("n must be greater than the length of x")}
  o <- x
  o[(length(x)+1):n] <- pad
  return(o)
}
