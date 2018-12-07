# Dunnett's K factor
#' @importFrom mnormt sadmvt pmt
Dunnetts_K <- function (n, df = n-1L, k, m=1, method = c("Bonferroni", "exact"),
                        type = c("two-sided", "upper", "lower"), conf.level = 0.95)
{
  method <- match.arg(method)
  type <- match.arg(type)
  alpha <- 1 - conf.level
  if (type == "two-sided") {
    alpha <- alpha/2
  }
  K <- qt(1 - alpha/k, df) * sqrt(1/m + 1/n)
  if (k > 1 && method == "exact") {
    V <- function(k, rho){
      matrix(rho, nrow=k, ncol=k) + (1-rho)*diag(k)
    }
    rho <- 1/((n/m) + 1)
    if (type == "two-sided") {
      # fcn.to.min <- function(d, df, k, S, conf.level) {
      #   (mnormt::sadmvt(df=df, lower=rep(-d,k), upper=rep(d,k),
      #                   mean=rep(0,k), S=S, maxpts=2000*k) - conf.level)
      # }
      d <- qDunnett(conf.level, df=df, k=k, rho=rho, tailed="one")
    }
    else {
      # fcn.to.min <- function(d, df, k, S, conf.level) {
      #   (mnormt::pmt(d, S=S, df=df) - conf.level)
      # }
      d <- qDunnett(conf.level, df=df, k=k, rho=rho, tailed="two")
    }
    # d <- uniroot(fcn.to.min,
    #              df = df, k = k, S = V(k,rho), conf.level = conf.level,
    #              lower=qt(1 - alpha, df),
    #              upper=qt(1 - alpha/k, df),
    #              tol=.Machine$double.eps, maxiter=5000)$root
    K <- d * sqrt(1/m + 1/n)
  }
  return(K)
}

# SLutils:::Dunnetts_K(10, k=3, method="exact")
# as.numeric(qDunnett(.95, df=10-1, k=3, rho=1/(10+1))) * sqrt(1+1/10)

# SLutils:::Dunnetts_K(10, k=3, method="exact", type="upper")
# as.numeric(qDunnett(.95, df=10-1, k=3, rho=1/(10+1), type="one-sided")) * sqrt(1+1/10)



#' @title Dunnett's K factor
#' @description Dunnett's K factor for exact prediction intervals.
#' @param n sample size
#' @param df degrees of freedom
#' @param k number of future observations
#' @param n.mean number of averages
#' @param method method
#' @param type interval type
#' @param conf.level confidence level
#' @return The factor K used by \code{\link{predictionIntervals}}.
#' @export
#' @details See the vignette.
#' @examples
#' predictionFactor_SL(10, k=3, method="exact")
predictionFactor <- function(n, df = n - 1, n.mean = 1, k = 1,
                             method = c("Bonferroni", "exact"),
                             type = "two-sided",
                             conf.level = 0.95){
  if (!is.vector(n, mode = "numeric") || length(n) != 1 ||
      !is.vector(df, mode = "numeric") || length(df) != 1 ||
      !is.vector(k, mode = "numeric") || length(k) != 1 ||
      !is.vector(n.mean, mode = "numeric") || length(n.mean) !=
      1 || !is.vector(conf.level, mode = "numeric") || length(conf.level) !=
      1)
    stop("'n', 'df', 'k', 'n.mean', and 'conf.level' must be numeric scalars")
  if (n < 2)
    stop("'n' must be greater than or equal to 2")
  if (df < 1)
    stop("'df' must be greater than or equal to 1")
  if (k < 1)
    stop("'k' must be greater than or equal to 1")
  if (n.mean < 1 || n.mean != trunc(n.mean))
    stop("'n.mean' must be an integer greater than 0")
  if (conf.level <= 0 || conf.level >= 1)
    stop("'conf.level' must be between 0 and 1")
  method <- match.arg(method, c("Bonferroni", "exact"))
  type <- match.arg(type, c("two-sided", "lower", "upper"))
  Dunnetts_K(n = n, df = df, k = k, m = n.mean, method = method,
             type = type, conf.level = conf.level)
}



#' @title Exact prediction interval for a Gaussian sample
#' @description Exact prediction interval for the next \code{k} observations
#' or next set of means. This is a port of the function \code{\link[EnvStats]{predIntNormK}}
#' od the \code{EnvStats} package.
#' @param x sample
#' @param n.mean positive integer, the sample size associated with the \code{k}
#' future averages; the default value \code{n.mean=1} means individual observations;
#' note that all future averages must be based on the same sample size
#' @param k positive integer, the number of future observations or averages
#' the prediction interval should contain with the given confidence level
#' @param method character string, the method to use if the number \code{k} of
#' future observations is greater than 1 (other this argument is ignored);
#' possible values are \code{"Bonferroni"} (approximate method based on the
#' Bonferonni inequality) and  \code{"exact"} (exact method due to Dunnett, 1955;
#' see the \strong{Details} section of \code{\link[EnvStats]{predIntNormK}} for
#' more information.
#' @param type interval type, \code{"two-sided"}, \code{"lower"} or \code{"upper"}
#' @param conf.level confidence level
#' @return A numeric vector of length two if \code{"two-sided"}, otherwise the bound
#' in the one-sided case.
#' @export
#' @details See the vignette.
#' @examples
#' predictionInterval(rnorm(10), k=3, method="exact")
predictionInterval <- function(x, n.mean = 1, k = 1, method = "Bonferroni",
                               type = "two-sided", conf.level = 0.95){
  K <- predictionFactor(n=length(x), n.mean=n.mean, k=k,
                        method=method, type=type,
                        conf.level = conf.level)
  s <- sd(x)
  if(type == "two-sided"){
    return(mean(x) + c(-1,1)*K*s)
  }else if (type == "lower"){
    return(mean(x) - K*s)
  }else {
    return(mean(x) + K*s)
  }
}


#' @title Coverage probability of Dunnett's exact prediction intervals
#' @description Estimates the coverage probability of Dunnett's
#' exact prediction intervals with the help of simulations
#' @param nsims positive integer, the number of simulations
#' @param type character, type of the interval
#' @param conf vector of numbers between 0 and 1, the confidence levels
#' @param n vector of integers greater than 1, the sample sizes
#' @param k vector of integers greater than 2, the numbers of future
#' observations
#' @return A datatable.
#' @import data.table
#' @importFrom stats rnorm
#' @export
#' @examples
#' CoverageEPI(10000, conf=c(0.8, 0.9), n=c(5,10), k=c(2,3))
CoverageEPI <- function(nsims, type="two-sided", conf, n, k){
  sims1 <- round(matrix(rnorm(nsims*max(n)), nrow=nsims), 1)
  sims2 <- round(matrix(rnorm(nsims*max(k)), nrow=nsims), 1)
  dat <- CJ(conf=conf, n=n, k=k)
  dat <- dat[, .(K=Dunnetts_K(n=`n`, k=`k`, type=type, method="exact", conf.level=`conf`)),
             by=.(conf,n,k)]
  dat[, `:=`(sims=list(sims1[,1:`n`])), by=.(n)]
  dat <- dat[, .(K=`K`,
                 mean = rowMeans(sims[[1]]),
                 sd = apply(sims[[1]], 1, sd)),
             by=.(conf,n,k)]
  dat <- dat[, .(lwr=`mean`-K*`sd`,
                 upr=`mean`+K*`sd`,
                 index=1:.N),
             by=.(conf,n,k)]
  setkeyv(dat, c(key(dat), "index"))
  dat[, `:=`(test_lwr=all(sims2[`index`, 1:`k`]>lwr),
             test_upr=all(sims2[`index`, 1:`k`]<upr)),
      by=.(index,conf,n,k)]
  if(type=="two-sided"){
    dat <- dat[, .(conf=`conf`,
                   n=`n`,
                   k=`k`,
                   test=`test_lwr`&`test_upr`)]
    return(dat[, .(coverage=mean(test)), by=.(conf,n,k)])
  }else{
    return(dat[, .(coverage_lwr=mean(test_lwr),
                   coverage_upr=mean(test_upr)),
               by=.(conf,n,k)])
  }
}
