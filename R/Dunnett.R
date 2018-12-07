#' @name DunnettDistribution
#' @rdname Dunnett
#' @title Cumulative distribution function and quantile function of the
#' Dunnett distribution
#' @description Cumulative distribution function and quantile function of
#' the Dunnett distribution.
#' @param p number between 0 and 1, the probability to achieve
#' @param q numeric, the quantile
#' @param n number of simulations
#' @param df postive number, the degrees of freedom
#' @param k integer greater than or equal to 2, the dimension
#' @param rho number between 0 and 1, correlation
#' @param tailed \code{"one"} or \code{"two"}, for the one-tailed Dunnett
#' dsitribution and the two-tailed Dunnett distribution, respectively
#' @param tol,maxiter parameters passed to \code{\link[stats]{uniroot}}
#' @return \code{pDunnett} returns the value of the cdf, \code{qDunnett}
#' returns the value of the inverse cdf, \code{rDunnett} samples the Dunnett
#' distribution.
#' @details See the vignette.
#' @importFrom mnormt sadmvt pmt rmt
#' @importFrom stats uniroot
#' @examples
#' qDunnett(0.95, df=3, k=2, rho=0.5)
#' pDunnett(3.87, df=3, k=2, rho=0.5)
NULL

#' @rdname Dunnett
#' @export
qDunnett <- function (p, df, k, rho,
                      tailed = c("one", "two"),
                      tol=.Machine$double.eps, maxiter=5000)
{
  if(floor(k) != k || k<2){
    stop("`k` must be a integer greater than 2")
  }
  if(p<0 || p>1){
    stop("`p` must be positive and lower than 1.")
  }
  if(p==1){
    return(Inf)
  }
  type <- match.arg(tailed)
  if(p==0){
    return(ifelse(type=="one", 0, -Inf))
  }
  alpha <- 1 - p
  if (type == "one") {
    alpha <- alpha/2
    fcn.to.min <- function(d, df, k, S, p) {
      (mnormt::sadmvt(df=df, lower=rep(-d,k), upper=rep(d,k),
                      mean=rep(0,k), S=S, maxpts=2000*k) - p)
    }
  } else {
    fcn.to.min <- function(d, df, k, S, p) {
      (mnormt::pmt(d, mean=rep(0,k), S=S, df=df, maxpts=2000*k) - p)
    }
  }
  V <- matrix(rho, nrow=k, ncol=k) + (1-rho)*diag(k)
  solve <- uniroot(fcn.to.min,
                   df = df, k = k, S = V, p = p,
                   lower=qt(1 - alpha, df),
                   upper=qt(1 - alpha/k, df),
                   tol=tol, maxiter=maxiter)
  root <- solve$root
  attr(root, "info") <- list(f.root = solve$f.root,
                             iter = solve$iter,
                             init.it = solve$init.it,
                             estim.prec <- solve$estim.prec)
  return(root)
}

#' @rdname Dunnett
#' @export
pDunnett <- function (q, df, k, rho,
                      tailed = c("one", "two"))
{
  if(floor(k) != k || k<2){
    stop("`k` must be a integer greater than 2.")
  }
  if(q==Inf){
    return(1)
  }
  type <- match.arg(tailed)
  if(type=="one" && q<=0){
    return(0)
  }
  if(q==-Inf && type=="two"){
    return(0)
  }
  V <- matrix(rho, nrow=k, ncol=k)
  diag(V) <- 1
  if (type == "one") {
    p <- mnormt::sadmvt(df=df,
                        lower=rep(-q,k), upper=rep(q,k),
                        mean=rep(0,k),
                        S=V,
                        maxpts=2000*k)
  }
  else {
    p <- mnormt::pmt(q, mean=rep(0,k), S=V, df=df, maxpts=2000*k)
  }
  return(p)
}

#' @rdname Dunnett
#' @export
rDunnett <- function (n, df, k, rho,
                      tailed = c("one", "two"))
{
  if(floor(k) != k || k<2){
    stop("`k` must be a integer greater than 2.")
  }
  type <- match.arg(tailed)
  V <- matrix(rho, nrow=k, ncol=k)
  diag(V) <- 1
  sims <- mnormt::rmt(n, mean=rep(0,k), S=V, df=df)
  if (type == "one") {
    return(apply(sims, 1L, max))
  } else {
    return(apply(abs(sims), 1L, max))
  }
}
