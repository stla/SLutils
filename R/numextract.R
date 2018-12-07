#' Numbers extraction - first number only
#'
#' Extract the first number contained in a character string.
#'
#' @param string a character vector
#' @param allow.negative logical, whether to allow negative numbers, or to ignore the minus sign
#' @return A character vector with the same length as \code{string}. A \code{NA} is returned when there is no number to extract from the character string.
#'
#' @seealso \code{\link{numsextract}} to extract all numbers in a character string, \code{\link{isNumerizable}}, which is based on \code{numextract}, and the similar function \code{readr::parse_number}.
#'
#' @note If the character string contains several numbers, this function extracts the first one only.
#'
#' @examples
#' numextract("30.5ml")
#' numextract(">2g")
#' numextract("between 5 and 6")
#' numextract(c("30.5ml", "37ml"))
#' numextract("-3C")
#' numextract("-3C", allow.negative=FALSE)
#' numextract(c("1", "b"))
#' numextract(c("1", NA))
#' numextract(c("1", "NA"))
#'
#' @importFrom stringr str_extract
#' @export
#'
numextract <- function(string, allow.negative=TRUE){
  if(allow.negative){
    return(stringr::str_extract(string, "\\-*\\d+\\.*\\d*"))
  }else{
    return(stringr::str_extract(string, "\\d+\\.*\\d*"))
  }
}

#' Numbers extraction
#'
#' Extract all numbers contained in a character string.
#'
#' @param string a character string
#' @param allow.negative logical, whether to allow negative numbers, or to ignore the minus sign
#' @return A character vector, the numbers contained in \code{string}.
#'
#' @examples
#' numsextract("between 2ml and 3.5ml")
#' numsextract("between -4.1 and -2")
#' numsextract("between -4.1 and -2", allow.negative=FALSE)
#'
#' @seealso \code{\link{numextract}} to extract the first number only.
#'
#' @export
numsextract <- function(string, allow.negative=TRUE){
  if(allow.negative){
    return(unlist(regmatches(string, gregexpr("\\-*[[:digit:]]+\\.*[[:digit:]]*",string))))
  }else{
    return(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string))))
  }
}

#' Test whether a vector contains numbers only
#'
#' Test whether a vector contains numbers only and then can be faithfully converted to a numeric vector.
#'
#' @param string a vector or a factor
#' @return A logical value. When the result is \code{FALSE}, an attribute \code{"unnumerizable"} is set to this value, it indicates which elements are not numerizable.
#'
#'
#' @note When the result is \code{TRUE}, then \code{string} can be faithfully converted to a numeric vector with \code{\link{Numerize}}.
#' \strong{Warning:} If \code{string} is a factor and the result is \code{TRUE}, then \code{as.numeric(string)} does not convert to the expected numeric vector (use \code{Numerize(string)} or \code{as.numeric(as.character(string))}).
#'
#' @seealso \code{\link{Numerize}}, which is based on \code{isNumerizable}.
#'
#' @examples
#' isNumerizable(c(1, 2, 3))
#' isNumerizable(c("a","b","c"))
#' isNumerizable(c("1", "2", "3"))
#' isNumerizable(c("1", "b", "c"))
#' isNumerizable(c("1.5", "  3.2 "))
#' isNumerizable(factor(c("3.5", "4")))
#'
#' @importFrom stringr str_trim
#' @export
#'
isNumerizable <- function(string){
  if(is.factor(string)) string <- as.character(string)
  s <- string[which(!is.na(string))]
  x <- numextract(s)
  if(any(is.na(x))){
    out <- FALSE
  }else{
    out <- all(x==stringr::str_trim(s))
  }
  #   notNA <- which(!is.na(x))
  #   out <- ifelse(length(notNA)==0, FALSE, all(x[notNA]==str_trim(string[notNA])))
  if(out==FALSE){
    x <- numextract(string)
    unnumerizables <- which((is.na(x) & !is.na(string)) | (x!=stringr::str_trim(string)))
    names(unnumerizables) <- string[unnumerizables]
    attr(out, "unnumerizable") <- unnumerizables
  }
  return(out)
}


#' Test whether columns of a dataframe are numerizable
#'
#' Tests which columns of a dataframe can be faithfully converted to a numeric vector.
#'
#' @param dat dataframe
#' @return A named logical vector, indicating for each column whether this column is numerizable.
#'
#' @note This function is based on \code{\link{isNumerizable}}.
#' @seealso \code{\link{coltypes}}, which returns the types of the columns of a dataframe, and indicates which ones are numerizable.
#'
#' @examples
#' dat <- data.frame(x=1:4,
#'                   y=c("a","b","c","d"),
#'                   z=c("1", "1.4", "-3", "   0"))
#' isNumerizable.df(dat)
#'
#' @importFrom purrr map_lgl
#' @export
#'
isNumerizable.df <- function(dat){
  #numcols <- vapply(dat, function(col) isNumerizable(col), logical(1))
  numcols <- purrr::map_lgl(dat, isNumerizable)
  return(numcols)
}

#' Column types
#'
#' Returns the types of the columns of a dataframe, and indicates which columns can be faithfully converted
#' to numeric mode.
#'
#' @param dat dataframe
#' @return A dataframe with three columns: \code{column}, the column names; \code{class}, the classes of the columns; \code{numerizable} (logical), whether the columns is numerizable.
#'
#' @note This functions uses \code{\link{isNumerizable.df}}.
#'
#' @examples
#' dat <- data.frame(x=1:4,
#'                   y=c("a","b","c","d"),
#'                   z=c("1", "1.4", "-3", "   0"))
#' coltypes(dat)
#'
#' @importFrom purrr map
#' @export
#'
coltypes <- function(dat){
  #classes <- sapply(dat, class)
  classes <- as.character(purrr::map(dat, class))
  numerizable <- isNumerizable.df(dat)
  return(data.frame(column=names(dat), class=classes, numerizable=numerizable, stringsAsFactors=FALSE))
}


#' Numerize a dataframe
#'
#' Given a dataframe of character columns, returns the same dataframe with numeric columns when possible.
#'
#' @param dat a dataframe whose is each column class is character or factor
#' @param factors logical indicating to include factor columns too
#' @param colnames the names of the columns to (attempt to) numerize
#' @return A dataframe, the same as \code{dat} after transforming its columns to numeric mode when possible. When some columns are not numerizable, their names are given in the attribute \code{"unnumerizable"}.
#'
#' @note This function is based on \code{\link{isNumerizable}}.
#'
#' @seealso \code{\link{Numerize}} to numerize a vector.
#'
#' @examples
#' dat <- data.frame(x=1:4,
#'                   y=c("a","b","c","d"),
#'                   z=c("1", "1.4", "-3", "   0"))
#' ( numdat <- Numerize.df(dat) )
#' attr(numdat, "unnumerizable")
#'
#' @importFrom purrr map_lgl map_chr map_at
#' @importFrom utils head tail
#' @export
#'
Numerize.df <- function(dat, factors=TRUE, colnames=names(dat)){ #
  #factorcols <- vapply(dat, function(col) is.factor(col), logical(1))
  factorcols <- purrr::map_lgl(dat, is.factor)
  modes <- purrr::map_chr(dat, mode)
  #modes[which(factorcols)] <- "factor"
  columns <- intersect(colnames, names(if(factors) which((modes != "numeric") | factorcols) else which((modes != "numeric") & !factorcols)))
  if(length(columns)==0){
    return(dat)
  }else{
    numcols <- columns[which(purrr::map_lgl(dat[,columns], isNumerizable))]
    if(length(numcols)>0){
      dat <- data.frame(purrr::map_at(dat, numcols, function(x) as.numeric(as.character(x))), stringsAsFactors=FALSE)
    }
    numerics <- purrr::map_lgl(dat[,columns], function(x) mode(x)=="numeric" && !is.factor(x))
    if(!all(numerics)){
      unnumerizables <- names(which(!numerics))
      attr(dat, "unnumerizable") <- unnumerizables
      warn <- ifelse(length(unnumerizables)==1L, sprintf("Column `%s` is not numerizable.", unnumerizables), sprintf("Columns %s and `%s` are not numerizable.", paste0(paste0("`", head(unnumerizables, -1), "`"), collapse=", "), tail(unnumerizables, 1)))
      warning(warn, call.=FALSE)
    }
    return(dat)
  }
  #if(factors) dat[,factorcols] <- data.frame(sapply(dat[,factorcols], as.character, simplify=FALSE), stringsAsFactors=FALSE)
  #if(factors) dat <- data.frame(purrr::map_at(dat, which(factorcols), as.character), stringsAsFactors=FALSE)
  #factorcols <-  if(factors) FALSE else factorcols
  #numcols <- intersect(colnames, names(dat)[which(vapply(dat, function(col) isNumerizable(col), logical(1)) & !factorcols)])
  #numcols <- intersect(colnames, names(dat)[which(purrr::map_lgl(dat, isNumerizable) & !factorcols)])
  #sapply(numcols, function(col) invisible(dat[[col]] <<- as.numeric(as.character(dat[[col]]))))
  #test <- all(sapply(dat[,colnames], is.numeric))
}

#' Numerize a character/factor vector
#'
#' Given a character or factor vector, returns the same in numeric mode if possible.
#'
#' @param x character or factor vector to be numerized
#' @return A numeric vector if possible, otherwise an error.
#'
#' @note This function is based on \code{\link{isNumerizable}}.
#'
#' @seealso \code{\link{Numerize.df}} to numerize a dataframe, and the function of the same kind \code{\link[utils]{type.convert}}.
#'
#' @examples
#' Numerize(c("1", "1.4", "-3", "   0"))
#' \dontrun{
#' Numerize(c("a","b","c","d"))}
#'
#' @export
#'
Numerize <- function(x){
  if(!is.factor(x) && class(x)!="Date" && mode(x)=="numeric"){
    return(x)
  }else if(!isNumerizable(x)){
    stop(sprintf("not a numerizable %s", ifelse(is.factor(x), "factor", ifelse(length(x)>1, "vector", "character string"))))
  }else{
    return(as.numeric(as.character(x)))
  }
}
