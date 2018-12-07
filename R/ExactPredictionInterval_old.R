#' # Internal functions for Dunnett's exact prediction interval
#' # helper Dunnett's function to integrate - one-sided case
#' Dunnetts_KF1 <- function (x, k, rho)
#' {
#'   fcn.to.integrate <- function(y, x, k, rho) {
#'     exp(k*pnorm((x + sqrt(rho) * y)/sqrt(1 - rho), log.p=TRUE) +
#'           dnorm(y, log=TRUE))
#'   }
#'   integrate(fcn.to.integrate, lower = -Inf, upper = Inf, x = x,
#'             k = k, rho = rho)$value
#' }
#' # helper Dunnett's function to integrate - two-sided case
#' Dunnetts_KF2 <- function (x, k, rho)
#' {
#'   fcn.to.integrate <- function(y, x, k, rho) {
#'     (pnorm((x + sqrt(rho) * y)/sqrt(1 - rho)) -
#'        pnorm((-x + sqrt(rho) * y)/sqrt(1 - rho)))^k * dnorm(y)
#'   }
#'   integrate(fcn.to.integrate, lower = -Inf, upper = Inf, x = x,
#'             k = k, rho = rho)$value
#' }
#' # Dunnett's function to integrate
#' Dunnetts_integrand <- function (s, d, n, df, k, rho, whichone=c("1","2"))
#' {
#'   whichone <- match.arg(whichone)
#'   Dunnetts_KF <- switch(whichone,
#'                         "1" = Dunnetts_KF1,
#'                         "2" = Dunnetts_KF2)
#'   # EnvStats:::cbind.no.warn replaced with cbind
#'   arg.mat <- cbind(s = as.vector(s), d = as.vector(d),
#'                    n = as.vector(n), df = as.vector(df), k = as.vector(k),
#'                    rho = as.vector(rho))
#'   for (i in c("s", "d", "n", "df", "k", "rho")){
#'     assign(i, arg.mat[,i])
#'   }
#'   N <- length(s)
#'   ret.val <- numeric(N)
#'   for (i in 1:N) {
#'     # EnvStats::dchi replaced with dchisq
#'     x <- sqrt(df[i]) * s[i]
#'     ret.val[i] <- Dunnetts_KF(d[i] * s[i], k[i], rho[i]) *
#'       2*x*dchisq(x^2, df[i]) * sqrt(df[i])
#'   }
#'   ret.val
#' }
#'
#' # Dunnett's K factor
#' Dunnetts_K <- function (n, df = n-1L, k, m=1, method = c("Bonferroni", "exact"),
#'                         type = c("two-sided", "upper", "lower"), conf.level = 0.95)
#' {
#'   method <- match.arg(method)
#'   type <- match.arg(type)
#'   alpha <- 1 - conf.level
#'   if (type == "two-sided") {
#'     alpha <- alpha/2
#'   }
#'   K <- qt(1 - alpha/k, df) * sqrt(1/m + 1/n)
#'   if (k > 1 && method == "exact") {
#'     rho <- 1/((n/m) + 1)
#'     if (type == "two-sided") {
#'       fcn.to.min <- function(d, n, df, k, rho, conf.level) {
#'         (integrate(Dunnetts_integrand, lower = 0,
#'                    upper = Inf, d = d, n = n, df = df, k = k,
#'                    rho = rho, whichone="2")$value - conf.level)^2
#'       }
#'     }
#'     else {
#'       fcn.to.min <- function(d, n, df, k, rho, conf.level) {
#'         (integrate(Dunnetts_integrand, lower = 0,
#'                    upper = Inf, d = d, n = n, df = df, k = k,
#'                    rho = rho, whichone="1")$value - conf.level)^2
#'       }
#'     }
#'     d <- nlminb(start = K/sqrt(1/m + 1/n), objective = fcn.to.min,
#'                 n = n, df = df, k = k, rho = rho, conf.level = conf.level,
#'                 lower = .Machine$double.eps)$par
#'     K <- d * sqrt(1/m + 1/n)
#'   }
#'   return(K)
#' }
#'
#' #' @title Dunnett's K factor
#' #' @description Dunnett's K factor for exact prediction intervals.
#' #' @param n sample size
#' #' @param df degrees of freedom
#' #' @param k number of future observations
#' #' @param n.mean number of averages
#' #' @param method method
#' #' @param pi.type interval type
#' #' @param conf.level confidence level
#' #'
#' #' @return The factor K.
#' #' @export
#' #'
#' #' @examples
#' #' predictionFactor(10, k=3, method="exact")
#' predictionFactor <- function(n, df = n - 1, n.mean = 1, k = 1,
#'                              method = c("Bonferroni", "exact"),
#'                              pi.type = "two-sided",
#'                              conf.level = 0.95){
#'   if (!is.vector(n, mode = "numeric") || length(n) != 1 ||
#'       !is.vector(df, mode = "numeric") || length(df) != 1 ||
#'       !is.vector(k, mode = "numeric") || length(k) != 1 ||
#'       !is.vector(n.mean, mode = "numeric") || length(n.mean) !=
#'       1 || !is.vector(conf.level, mode = "numeric") || length(conf.level) !=
#'       1)
#'     stop("'n', 'df', 'k', 'n.mean', and 'conf.level' must be numeric scalars")
#'   if (n < 2)
#'     stop("'n' must be greater than or equal to 2")
#'   if (df < 1)
#'     stop("'df' must be greater than or equal to 1")
#'   if (k < 1)
#'     stop("'k' must be greater than or equal to 1")
#'   if (n.mean < 1 || n.mean != trunc(n.mean))
#'     stop("'n.mean' must be an integer greater than 0")
#'   if (conf.level <= 0 || conf.level >= 1)
#'     stop("'conf.level' must be between 0 and 1")
#'   method <- match.arg(method, c("Bonferroni", "exact"))
#'   pi.type <- match.arg(pi.type, c("two-sided", "lower", "upper"))
#'   Dunnetts_K(n = n, df = df, k = k, m = n.mean, method = method,
#'              type = pi.type, conf.level = conf.level)
#' }
#'
#' #'
#' #' #' @title Exact prediction interval for a Gaussian sample
#' #' #' @description Exact prediction interval for the next \code{k} observations
#' #' #' or next set of means. This is a port of the function \code{\link[EnvStats]{predIntNormK}}
#' #' #' od the \code{EnvStats} package.
#' #' #' @param x sample
#' #' #' @param n.mean positive integer, the sample size associated with the \code{k}
#' #' #' future averages; the default value \code{n.mean=1} means individual observations;
#' #' #' note that all future averages must be based on the same sample size
#' #' #' @param k positive integer, the number of future observations or averages
#' #' #' the prediction interval should contain with the given confidence level
#' #' #' @param method character string, the method to use if the number \code{k} of
#' #' #' future observations is greater than 1 (other this argument is ignored);
#' #' #' possible values are \code{"Bonferroni"} (approximate method based on the
#' #' #' Bonferonni inequality) and  \code{"exact"} (exact method due to Dunnett, 1955;
#' #' #' see the \strong{Details} section of \code{\link[EnvStats]{predIntNormK}} for
#' #' #' more information.
#' #' #' @param pi.type interval type, \code{"two-sided"}, \code{"lower"} or \code{"upper"}
#' #' #' @param conf.level confidence level
#' #' #'
#' #' #' @return A numeric vector of length two if \code{"two-sided"}, otherwise the bound
#' #' #' in the one-sided case.
#' #' #' @export
#' #' #'
#' #' #' @examples
#' #' #' predictionInterval(rnorm(10), k=3, method="exact")
#' #' predictionInterval <- function(x, n.mean = 1, k = 1, method = "Bonferroni",
#' #'                                pi.type = "two-sided", conf.level = 0.95){
#' #'   K <- predictionFactor(n=length(x), n.mean=n.mean, k=k,
#' #'                         method=method, pi.type=pi.type,
#' #'                         conf.level = conf.level)
#' #'   s <- sd(x)
#' #'   if(pi.type == "two-sided"){
#' #'     return(mean(x) + c(-1,1)*K*s)
#' #'   }else if (pi.type == "lower"){
#' #'     return(mean(x) - K*s)
#' #'   }else {
#' #'     return(mean(x) + K*s)
#' #'   }
#' #' }
#' #'
