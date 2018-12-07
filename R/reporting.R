#' @title Sentence enumerating the elements of a vector
#' @description Make a sentence enumerating the elements of a vector.
#' @param x character or numeric vector
#' @return A character string.
#' @export
#' @importFrom utils head tail
#' @examples
#' enumerate(letters[1])
#' enumerate(letters[1:2])
#' enumerate(letters[1:3])
enumerate <- function(x){
  if(length(x)==1){
    return(as.character(x))
  }else if(length(x)==2){
    return(paste0(x[1], " and ", x[2]))
  }else{
    return(paste0(paste0(head(x,-1), collapse=", "), " and ", tail(x,1)))
  }
}

#' @title Convert numbers to words
#' @description Convert numbers to their corresponding English words.
#' @param x vector of numbers
#' @return The vector of words.
#' @author \href{http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html}{John Fox}
#' and \href{https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r}{Tony Hirst}
#' @export
#' @examples
#' numbers2words(c(2, 17))
numbers2words <- function(x){
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  #Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}


#' @title Formatting a number for LaTeX
#' @description Formatting a number for LaTeX, handling the scientific notation
#' @param x number
#' @param ... arguments passed to \code{\link[base]{formatC}}
#' @return A character string ready to be surrounded between dollars for LaTeX.
#' @export
#' @examples
#' formatClatex(1.234567, digits=4)
#' formatClatex(1.234567e-12, digits=4)
formatClatex <- function(x, ...){
  x <- formatC(x, ...)
  if(grepl("e", x)){
    xx <- strsplit(x, "e", fixed=TRUE)[[1]]
    paste0(xx[1], sprintf("\\mathrm{e}{%s}", xx[2]))
  }else{
    return(x)
  }
}

