#' @title Number of decimals
#' @description Number of decimals of a number.
#' @param x number
#' @return The number of decimals of \code{x}.
#' @export
#' @seealso \code{\link{nfloordigits}}
#' @examples ndecimals(13.451)
#' ndecimals(2)
#' ndecimals(-43.421)
#' ndecimals(pi)
#' ndecimals(1.23e-20)
ndecimals <- function(x){
  i <- 0L
  d <- match(TRUE, round(x, 0L) == x)
  while(is.na(d)){
    i <- i+1L
    d <- match(TRUE, round(x, i) == x)
  }
  return(i)
}

#' @title Number of digits of the integer part
#' @description Number of digits of the integer part of a number.
#' @param x number
#' @return The number of digits of the integer part of \code{x}.
#' @seealso \code{\link{ndecimals}}
#' @export
#' @examples nfloordigits(13.451)
#' nfloordigits(2)
#' nfloordigits(-43.421)
#' nfloordigits(23.15e6)
nfloordigits <- function(x){
  x <- abs(x)
  nfloor <- max(0L,as.integer(floor(log10(x+.Machine$double.eps))))+1L
  return(nfloor)
}

#' @title Format a number with a given number of characters
#' @description Format (and round) a number with a given maximal number of characters.
#' @param x number
#' @param digits maximal number of characters
#' @export
#' @examples prettyNumber(pi, 4)
#' prettyNumber(123456789, 7)
#' prettyNumber(12.5e9, 7)
#' prettyNumber(0.00000012, 7)
prettyNumber <- function(x, digits=9L){
  i <- 22L
  y <- prettyNum(x, digits=i)
  while(nchar(y)>digits && i>1L){
    i <- i-1L
    y <- prettyNum(x, digits=i)
  }
  return(y)
}
