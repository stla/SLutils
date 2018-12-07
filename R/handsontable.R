#' @title Find the type for handsontable
#' @export
hotType <- function(col){
  isDate <- guessDateFormat(col)
  if(identical(isDate, FALSE)){
    cl <- ifelse(any(c("character", "factor") %in% class(col)),
                 "text",
                 "other")
    if(cl != "text"){
      cl <- ifelse(any(c("Date", "POSIXt") %in% class(col)),
                   "date",
                   ifelse("logical" %in% class(col),
                          "checkbox",
                          "mynumeric"))
    }
    return(cl)
  }else{
    return("date")
  }
}

#' @title Create columns setting for handsontable
#' @export
#' @importFrom purrr map_chr
hotColumns <- function(dat){
  hotTypes <- unname(purrr::map_chr(dat, hotType))
  dateFormat <- purrr::map_chr(hotTypes, ~ ifelse(.x=="date", "YYYY-MM-DD", NA))
  numFormat <- purrr::map_chr(hotTypes, ~ ifelse(.x=="mynumeric", "0.[00000000000000]", NA))
  return(data.frame(data=colnames(dat),
                    type=hotTypes,
                    dateFormat=dateFormat,
                    format=numFormat,
                    stringsAsFactors = FALSE))
}

#' @title Format dates for handsontable
#' @export
#' @importFrom purrr map_if
hdatify <- function(dat){
  #dat <- purrr::dmap_if(dat, is.factor, as.character)
  data.frame(purrr::map_if(dat, ~ hotType(.x)=="date", ~ as.Date(.x)))#  as.Date(format(.x, "%d/%m/%Y"), "%d/%m/%Y")))
}

#' @title Create handsontable columns setting from given types
#' @param dftypes dataframe as returned by \code{\link{XLSXtypes}}
#' @export
#' @importFrom purrr map_chr
hotColumnsFromTypes <- function(dftypes){
  dict <- c(number="mynumeric", text="text", richtext="text", date="date", boolean="checkbox")
  hotTypes <- unname(purrr::map_chr(dftypes, function(col){
    if(length(unique(na.omit(col)))==1L){
      return(dict[col[1]])
    }else if("number" %in% col){
      return("mynumeric")
    }else{
      return("text")
    }
  }))
  dateFormat <- purrr::map_chr(hotTypes,
                               ~ ifelse(.x=="date", "YYYY-MM-DD", NA))
  numFormat <- purrr::map_chr(hotTypes,
                              ~ ifelse(.x=="mynumeric", "0.[00000000000000]", NA))
  return(data.frame(data=colnames(dftypes),
                    type=hotTypes,
                    dateFormat=dateFormat,
                    format=numFormat,
                    stringsAsFactors = FALSE))
}
