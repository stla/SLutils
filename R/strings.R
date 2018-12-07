#' @title Separate characters of a string
#' @description Separate the characters of a character string.
#' @param string a character string
#' @return A character vector.
#' @examples
#' # this returns c("h", "e", "l", "l", "o"):
#' string2letters("hello")
#' @export
string2letters <- function(string){
  rawToChar(charToRaw(string), multiple = TRUE)
}


#' @title Generates 001, 002, ...
#' @description Generates consecutive integers with a given number of digits, with an optional prefix and suffix.
#' @param I integer
#' @param prefix character, to prepend
#' @param suffix character, to append
#' @param ndigits integer, the number of digits; default to no more digits than necessary
#' @return A character vector, see examples. An attribute \code{"string"} is attached, it is used in \code{tikz2png}.
#' @examples
#' charseq(11)
#' charseq(11, ndigits=3)
#' charseq(11, prefix="graph", suffix=".png")
#' @export
charseq <- function(I, prefix=NULL, suffix=NULL, ndigits=floor(log10(I))+1){
  out <- paste0(prefix, sprintf(paste0("%s", suffix), sprintf(paste0("%0", ndigits, "d"), 1:I)))
  attr(out, "string") <- sprintf("%s%s%s", prefix, paste0("%0", ndigits, "d"), suffix)
  return(out)
}


