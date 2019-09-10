scalarize <- function(x){
  class(x) <- "scalar"
  return(x)
}

NA_scalar <- scalarize(NA)

cellValue <- function(values, c, r){
  x <- values[.(c,r), -c(1L,2L)]
  if(length(index <- which(!is.na(x)))){
    return(x[[index]])
  }else{
    return(NA)
  }
}

#' @title Get an Excel sheet
#' @description Get the contents of an Excel sheet cell by cell.
#' @param xlsxFile path to a xlsx file
#' @param sheet sheet index or name
#' @return A keyed datatable.
#' @import data.table
#' @import tidyxl
#' @export
#' @note This function is intended to be used for \code{\link{sheetToJSON}} and \code{\link{treadXLSX}}.
#' @examples
#' xlsxFile <- system.file("extdata", "xlsx", "xlsx_small.xlsx", package="SLutils")
#' sheet <- getExcelSheet(xlsxFile, sheet="Sheet2")
#' library(data.table)
#' key(sheet)
#' sheet[.(2,4), .(data_type, character, comment)]
getExcelSheet <- function(xlsxFile, sheet=1L){
  DT <- subset(
    as.data.table(xlsx_cells(xlsxFile, sheets=sheet)),#$data[[1L]]),
    !is_blank | !is.na(comment),
    select = c("col", "row", "data_type", "logical", "numeric", "date", "character", "comment")
  )
  setkeyv(DT, c("col", "row"))
  return(DT)
}

#' @title Read an Excel sheet
#' @description Convert an Excel sheet (output of \code{\link{getExcelSheet}}) to
#' a list of three R objects or JSON strings: values, comments, and types.
#' @param sheet an Excel sheet, like the output of \code{\link{getExcelSheet}}
#' @param header logical, whether considering the first row as column headers
#' @param firstRow the first row to be read, an integer or \code{NULL} to guess it
#' @param output \code{"list"} to get a list of three R objects
#' (\code{"data"}: the list of columns in the worksheet,
#' \code{"comments"}: a tibble containing the comments of the cells,
#' \code{"types"}: a tibble containing the comments of the cells),
#' \code{"JSON"} to get a list of these three objects encoded to JSON strings,
#' or \code{"both"} to get both.
#'
#' @return A list of three R objects or three JSON strings, or both.
#' @import data.table
#' @importFrom stats setNames
#' @importFrom jsonlite toJSON
#' @importFrom purrr map_chr map_df
#' @export
#' @seealso \code{\link{getExcelSheet}}, \code{\link{hreadXLSX}}, a similar function,
#' and \code{\link{sheetToJSON}}, a shortcut to \code{output="JSON"}.
#' @examples
#' xlsxFile <- system.file("extdata", "xlsx", "xlsx_mixed.xlsx", package="SLutils")
#' sheet <- getExcelSheet(xlsxFile)
#' treadXLSX(sheet)
#' identical(treadXLSX(sheet, output="JSON"), sheetToJSON(sheet))
#' treadXLSX(sheet, header=FALSE)$data
#' treadXLSX(sheet, header=FALSE, firstRow=3)$data$X2
treadXLSX <- function(sheet, header=TRUE, firstRow=NULL, output=c("list", "JSON", "both")){

  output <- match.arg(output)

  firstRow <- ifelse(is.null(firstRow), min(sheet$row), firstRow)

  values <- sheet[row >= firstRow, .(col, row, logical, numeric, date, character)]
  values[, date:=as.character(as.Date(date))]

  colRange <- unique(values$col)

  if(header){
    colHeaders <- purrr::map_chr(colRange, function(c){
      ifelse(is.na(value <- cellValue(values,c, firstRow)),
             paste0("X",c),
             as.character(value))
    })
    values <- values[row>firstRow]
  }else{
    colHeaders <- paste0("X", colRange)
  }

  #dat <- values[, lapply(.SD, scalarize)]
  dat <- values
  rowRange <- min(dat$row):max(dat$row)
  nrows <- length(rowRange)

  # can be improved with data.table ?
  flatList <- setNames(lapply(colRange, function(c){
    if(c %in% dat$col){
      column <- dat[.(c), -1L]
      setkeyv(column, "row")
      lapply(rowRange, function(r){
        if(! r %in% column$row){
          #return(NA_scalar)
          return(NA)
        }else{
          x <- column[.(r), -1L]
          if(length(index <- which.min(is.na(x)))){
            return(x[[index]])
          }else{
            #return(NA_scalar)
            return(NA)
          }
        }
      })
    }else{
      #return(as.list(rep(NA_scalar, nrows)))
      return(as.list(rep(NA, nrows)))
    }
  }), colHeaders)

  if(output != "JSON"){
    #list_data <- sapply(flatList, unlist, simplify=FALSE, USE.NAMES=TRUE)
    list_data <- flatList
  }
  if(output != "list"){
    json_data <- jsonlite::toJSON(rapply(flatList,
                                         f=scalarize, how="replace"),
                                  digits=14L)
  }

  # comments
  comments <- sheet[row >= firstRow+header, .(col, row, comment)]
  if(output != "list") comments <- comments[, lapply(.SD, scalarize)]
  list_comments <- purrr::map_df(
    setNames(colRange,colHeaders),
    function(c){
      purrr::map_chr(rowRange, function(r){
        comments[.(c,r)]$comment
      })
    })

  if(output != "list"){
    if(all(is.na(comments$comment))){
      json_comments <- "null"
    }else{
      json_comments <- jsonlite::toJSON(list_comments, dataframe="columns")
    }
  }

  # types
  types <- sheet[row >= firstRow+header, .(col, row, data_type)]
  if(output != "list") types <- types[, lapply(.SD, scalarize)]
  list_types <- purrr::map_df(
    setNames(colRange,colHeaders),
    function(c){
      purrr::map_chr(rowRange, function(r){
        types[.(c,r)]$data_type
      })
    })
  if(output != "list"){
    json_types <- jsonlite::toJSON(list_types, dataframe="columns")
  }
  # output
  if(output %in% c("JSON", "both")){
    json <- list(data=json_data, comments=json_comments, types=json_types)
    if(output == "JSON"){
      return(json)
    }
  }
  if(output %in% c("list", "both")){
    List <- list(data=list_data, comments=list_comments, types=list_types)
    if(output == "list"){
      return(List)
    }
  }
  return(list(json=json, R=List))
}

#' @title Excel sheet to JSON strings
#' @description Convert an Excel sheet (output of \code{\link{getExcelSheet}}) to
#' a list of three JSON strings: values, comments, and types.
#' @param sheet an Excel sheet, like the output of \code{\link{getExcelSheet}}
#' @param header logical, whether considering the first row as column headers
#' @param firstRow the first row to be read, an integer or \code{NULL} to guess it
#' @return A list of three JSON strings.
#' @import data.table
#' @importFrom stats setNames
#' @importFrom jsonlite toJSON
#' @importFrom purrr map_chr map_df
#' @export
#' @note This function is a shortcut to \code{\link{treadXLSX}} with option
#' \code{output="JSON"}.
#' @seealso \code{\link{getExcelSheet}}, \code{\link{treadXLSX}}.
#' @examples
#' xlsxFile <- system.file("extdata", "xlsx", "xlsx_mixed.xlsx", package="SLutils")
#' sheet <- getExcelSheet(xlsxFile)
#' sheetToJSON(sheet)
#' sheetToJSON(sheet, header=FALSE)$data
#' sheetToJSON(sheet, header=FALSE, firstRow=3)$data
sheetToJSON <- function(sheet, header=TRUE, firstRow=NULL){
  #treadXLSX(sheet=sheet, header=header, firstRow=firstRow, output="JSON")

  firstRow <- ifelse(is.null(firstRow), min(sheet$row), firstRow)

  values <- sheet[row >= firstRow, .(col, row, logical, numeric, date, character)]
  values[, date:=as.character(as.Date(date))]

  colRange <- unique(values$col)

  if(header){
    colHeaders <- purrr::map_chr(colRange, function(c){
      ifelse(is.na(value <- cellValue(values,c, firstRow)),
             paste0("X",c),
             as.character(value))
    })
    values <- values[row>firstRow]
  }else{
    colHeaders <- paste0("X", colRange)
  }

  dat <- values[, lapply(.SD, scalarize)]
  rowRange <- min(dat$row):max(dat$row)
  nrows <- length(rowRange)

  # can be improved with data.table ?
  flatList <- setNames(lapply(colRange, function(c){
    if(c %in% dat$col){
      column <- dat[.(c), -1L]
      setkeyv(column, "row")
      lapply(rowRange, function(r){
        if(! r %in% column$row){
          return(NA_scalar)
        }else{
          x <- column[.(r), -1L]
          if(length(index <- which.min(is.na(x)))){
            return(x[[index]])
          }else{
            return(NA_scalar)
          }
        }
      })
    }else{
      return(as.list(rep(NA_scalar, nrows)))
    }
  }), colHeaders)

  json_data <- jsonlite::toJSON(flatList, digits=14L)

  # comments
  # comments <- sheet[row >= firstRow+header, .(col, row, comment)]
  # comments <- comments[, lapply(.SD, scalarize)]
  comments <- sheet[row >= firstRow+header, .(col, row, comment), lapply(.SD, scalarize)]
  list_comments <- purrr::map_df(
    setNames(colRange,colHeaders),
    function(c){
      purrr::map_chr(rowRange, function(r){
        comments[.(c,r)]$comment
      })
    })

    if(all(is.na(comments$comment))){
      json_comments <- "null"
    }else{
      json_comments <- jsonlite::toJSON(list_comments, dataframe="columns")
    }

  # types
  types <- sheet[row >= firstRow+header, .(col, row, data_type), lapply(.SD, scalarize)]
  list_types <- purrr::map_df(
    setNames(colRange,colHeaders),
    function(c){
      purrr::map_chr(rowRange, function(r){
        types[.(c,r)]$data_type
      })
    })

  json_types <- jsonlite::toJSON(list_types, dataframe="columns")

  return(list(data=json_data, comments=json_comments, types=json_types))
}
