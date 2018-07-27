#' Generate Power Function, Stepped, and Cyclic Sequences
#'
#' @param from Numeric. Number to interpolate from. For `cyclx()`, first number in the trend.
#' @param to Numeric. Number to interpolate to. For `cyclx()`, last number in the trend.
#' @param n Numeric. Length of sequence. Equvalent of `length.out` in [base::seq()].
#' @param step.n Numeric. Step length, in same units as `n`.
#' @param exponent Numeric. Exponent of power function used to interpolate betwwen `from` and `to`.
#' @param cyc.from Numeric. Number to interpolate from, for each cycle to be imposed on trend.
#' @param cyc.to Numeric. Number to interpolate to, for each cycle to be imposed on trend.
#' @param cyc.n Numeric. Length of cycle, in same units as `n`.
#' @param cyc.exponent Numeric. Exponent of power function used to interpolate betwwen `cyc.from`
#' and `cyc.to`.
#' @param operator Character. `"*"` or `"+"`. Operator used to impose cycles on trend. By default,
#' cycles and trend are multiplied ("*").
#'
#' @return Numeric vector
#'
#' @examples
#' s <- rampx(0,1,101)
#' stepped_s <- stepx(0,1,101,step.n = 10)
#' plot(s,type="l")
#' points(stepped_s,type="p",pch=20)
#'
#' s <- rampx(0,1,101,exp=2)
#' stepped_s <- stepx(0,1,101,step.n = 20,exp=2)
#' plot(s,type="l")
#' points(stepped_s,type="p",pch=20)
#'
#' s <- stepx(0,1,101,step.n=20)
#' cycled_s <- cyclx(0,1,101,step.n = 20,cyc.from=0.5,cyc.to=1,cyc.n=20,cyc.exponent = 1)
#' plot(cycled_s,type="p",pch=19,col="red",ylim=c(0,1))
#' points(s,type="p",pch=20)
#'
#' stepped_s <- stepx(0,1,101,step.n = 20,exp=.5)
#' cycled_s <- cyclx(0,1,101,step.n = 20,exp=.5,cyc.from=0,cyc.to=1,cyc.n=20,cyc.exponent=1)
#' plot(cycled_s,type="p",pch=19,col="red")
#' points(stepped_s,type="p",pch=20)
#'
#' stepped_s <- stepx(0,1,101,step.n=10,exp=2)
#' cycled_s <- cyclx(0,1,101,step.n=10,exp=2,cyc.from=0,cyc.to=1,cyc.n=10,cyc.exponent=2,operator="+")
#' plot(cycled_s,type="p",pch=19,col="red")
#' points(stepped_s,type="p",pch=20)
#'
#' @export
cyclx <- function(from=0,to=1,n=2,step.n=1,exponent=1,cyc.from=NULL,cyc.to=NULL,cyc.n=n,cyc.exponent=1,operator="*"){
  if(length(from)!=1 || length(to)!=1) stop('"from" and "to" must be of length 1')
  if(!operator %in% c("*","+")) stop('Operator must be "*" or "+".')
  if(operator=="*"){ cyc.x <- cyc.y <- 1 } else { cyc.x <- cyc.y <- 0 }
  if(!is.null(cyc.from)){cyc.x <- cyc.from}
  if(!is.null(cyc.to)){cyc.y <- cyc.to}

  o.step <- stepx(from=from,to=to,n=n,step.n=step.n,exponent=exponent)
  o.cyc <- rep(rampx(cyc.x,cyc.y,cyc.n,cyc.exponent),length.out=n)

  combine <- match.fun(FUN = operator)
  o <- combine(o.step,o.cyc)
  return(o)
}

#' @rdname cyclx
#' @export
rampx <- function(from=0,to=1,n=2,exponent=1){
  if(length(from)!=1 || length(to)!=1) stop('"from" and "to" must be of length 1')
  i <- seq(0,1,length.out=n)
  o <- (from + (to-from)*i^exponent[1L])
  return(o)
}

#' @rdname cyclx
#' @export
stepx <- function(from=0,to=1,n=2,step.n=1,exponent=1){
  if(length(from)!=1 || length(to)!=1) stop('"from" and "to" must be of length 1')
  o <- rep(rampx(from=from,to=to,n=ceiling(n/step.n),exponent=exponent),each=step.n)[1:n]
  return(o)
}
