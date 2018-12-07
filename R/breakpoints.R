#' @title Find breakpoints
#' @description Find breakpoints for a Gaussian linear model.
#'
#' @param formula model formula
#' @param dat dataframe
#' @param onebreak logical, whether to find only one breakpoint
#' @param nbreaks.max \code{NULL} for the optimal number of breakpoints, otherwise an integer for the maximal number of breakpoints
#' @param ... arguments passed to \code{\link[strucchange]{Fstats}}
#'
#' @return A dataframe listing the breakpoints.
#' @export
#' @importFrom strucchange breakpoints Fstats
#'
#' @examples
#' data(dataBP)
#' ( bp <- breakpoints(y~1, dat=dataBP, onebreak=FALSE, nbreaks.max=NULL) )
#' paste("Breakpoints:", paste(dataBP$date[bp$breakpoint], collapse=", "))
breakpoints <- function(formula, dat, onebreak=TRUE, nbreaks.max=NULL, ...){
  if(!onebreak){
    bp <- strucchange::breakpoints(formula, data=dat, breaks=nbreaks.max)
    bp$coef <- coef(bp)
  }else{
    bp <- strucchange::Fstats(formula, data=dat, ...)
    mf <- model.frame(formula, data=dat)
    y <- model.response(mf)
    X <- model.matrix(formula, data=dat)
    n <- nrow(X)
    i <- bp$breakp
    X1 <- as.matrix(X[(1:i), ])
    X2 <- as.matrix(X[((i + 1):n), ])
    fm1 <- lm.fit(X1, y[1:i])
    fm2 <- lm.fit(X2, y[((i + 1):n)])
    if(ncol(X)==1){
      bp$coef <- c(coef(fm1),coef(fm2))
    }else{
      bp$coef <- rbind(coef(fm1),coef(fm2))
    }
  }
  return(bp)
}
