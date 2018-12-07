# .hreadXLSX <- function(file, sheet, what, header){
#   .C("xlsx2jsonR", file=file, sheet=sheet, what=what,
#      header=as.integer(header), result="", NACK=TRUE)$result
# }

#' Read a XLSX file
#'
#' @param file name of the output xlsx file
#' @param sheet sheet name
#' @param what what to read; must be one among \code{"data"}, \code{"comments"},
#' \code{"types"}, \code{"formats"}, or comma-separated combination of these choices (e.g. "data,comments")
#' @param header whether to consider the first row as column headers
#' @param firstRow integer or \code{NULL}, the starting row (guessed if \code{NULL})
#' @param lastRow integer or \code{NULL}, the ending row (guessed if \code{NULL})
#' @param output the desired output; see \strong{Value}
#' @return The possible output is:
#' \itemize{
#'      \item If only one "what" is requested:
#'      a "flat list" if \code{output="flatlist"},
#' a list if \code{output="list"}, a dataframe if \code{output="dataframe"},
#' a JSON string if \code{output="JSON"}.
#'      \item Otherwise, a list of the previous kind of outputs.
#' }
#' @export
# #' @useDynLib xlsx2jsonR
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_trim str_detect
#' @examples
#' xlsx <- system.file("extdata/xlsx/xlsx_comments.xlsx", package = "SLutils")
#' hreadXLSX(xlsx, sheet="Sheet1")
hreadXLSX <- function(file, sheet="Sheet1", what="data", header=TRUE,
                      firstRow=NULL, lastRow=NULL,
                      output=c("flatlist", "list", "dataframe", "JSON"), stringsAsFactors=TRUE){
  ### .hreadXLSX(file, sheet, what, header)
  if(!file.exists(file)){
    stop(sprintf("File `%s` not found.", file))
  }
  # check the `what` argument
  what <- stringr::str_trim(strsplit(what, ",")[[1]])
  if(any(!is.element(what, c("data", "comments", "types", "formats")))){
    stop("Invalid argument `what`. See `?hreadXLSX`.")
  }
  onewhat <- length(what) == 1L
  what <- paste0(what, collapse=",")
  #
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("xlsx2json", exe_ext), package="SLutils")
  # command <- sprintf("%s -f %s -s %s -w %s%s%s%s",
  #                    exe, shQuote(file), shQuote(sheet), what,
  #                    ifelse(header, " -H", ""),
  #                    ifelse(is.null(firstRow), "", paste0(" -F ", firstRow)),
  #                    ifelse(is.null(lastRow), "", paste0(" -L ", lastRow)))
  #json <- paste0(system(command, intern=TRUE), collapse="") # paste because long output is split
  args <- sprintf("-f %s -s %s -w %s%s%s%s",
                  shQuote(file), shQuote(sheet), what,
                  ifelse(header, " -H", ""),
                  ifelse(is.null(firstRow), "", paste0(" -F ", firstRow)),
                  ifelse(is.null(lastRow), "", paste0(" -L ", lastRow)))
  json <- tryCatch({
    #system(command, intern=TRUE)
    system2(exe, args, stdout=TRUE, stderr=TRUE)
  }, warning = function(w){
    stop(sprintf("%s%s",
                 w$message,
                 ifelse(is.null(firstRow), "",
                        sprintf("\nPossible cause: there is nothing below row %s and you supplied `firstRow=%s`.",
                                firstRow-1, firstRow))))
  }, error = function(e){
    stop(e$message)
  })
  json <- paste0(json, collapse="")
  #jsonR <- jsonlite::fromJSON(json) # perte de temps !!
  if(!is.null(sheet) && !stringr::str_detect(json, "\\[")){
    stop(sprintf("No sheet or empty sheet `%s` in `%s` (%s).",
                 sheet, basename(file), jsonlite::fromJSON(json)))
  }
  Encoding(json) <- "UTF-8"
  #
  output <- match.arg(output)
  if(output=="dataframe" || output=="list" || output=="flatlist"){
    out <- jsonlite::fromJSON(json, simplifyVector = output != "flatlist")
    if(output=="dataframe"){
      if(onewhat){
        out <- as.data.frame(out, stringsAsFactors=stringsAsFactors)
      }else{
        out <- lapply(out, function(x) as.data.frame(x, stringsAsFactors=stringsAsFactors))
      }
    }
  }else{
    out <- json
  }
  return(out)
}

#' Write a XLSX file
#'
#' @param dat dataframe or JSON string
#' @param file name of the output xlsx file
#' @param comments optional dataframe or JSON string
#' @param commentsAuthor option string
#' @param base64 logical, whether to get the file as a base64 string
#' @param input \code{"dataframe"} if \code{dat} and \code{comments} are given as dataframes, \code{"JSON"} if they are given as JSON strings
#'
#' @return No value returned if \code{base64=FALSE}, otherwise a base64 string.
#' @export
#' @importFrom jsonlite toJSON
#' @examples \dontrun{
#' dat <- data.frame(x=1:2, y=3:4)
#' comments <- data.frame(x=c(NA,NA), y=c(NA, "hello!"))
#' XLSXwrite(dat, "myfile.xlsx", comments)}
XLSXwrite <- function(dat, file, comments=NULL, commentsAuthor=NULL, base64=FALSE, input=c("dataframe", "JSON")){
  input <- match.arg(input)
  if(input == "dataframe"){
    rownames(dat) <- NULL
    jsondat <- jsonlite::toJSON(dat, dataframe="columns",
                                digits=14, na="null",
                                row.names=FALSE)
  }else{
    #cat(dat)
    #jsondat <- stringi::stri_escape_unicode(dat)
    #Encoding(dat) <- "UTF-8"
    #jsondat <- dat
    #jsondat <- gsub("\\\"", "\\\\\\\"\\1", dat)
    if(.Platform$OS.type == "windows"){
      jsondat <- gsub("\\\"", "\\\\\"", dat, fixed=TRUE)
    }else{
      jsondat <- dat #gsub("\\\"", "\\\\\\\"", dat, fixed=TRUE)
    }
    #cat(jsondat)
  }
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("json2xlsx", exe_ext), package="SLutils")
  if(is.null(comments)){
    command <- sprintf("%s -d %s --header -o %s%s",
                       exe, shQuote(as.character(jsondat)), file,
                       ifelse(base64, " --base64", ""))
  }else{
    if(input == "dataframe"){
      rownames(comments) <- NULL
      jsoncomments <- jsonlite::toJSON(comments, dataframe="columns",
                                       na="null",
                                       row.names=FALSE)
    }else{
      #Encoding(comments) <- "UTF-8"
      #      jsoncomments <- comments
      #jsoncomments <- gsub("\\\"", "\\\\\\\\\\1", comments)
      if(.Platform$OS.type == "windows"){
        jsoncomments <- gsub("\\\"", "\\\\\"", comments, fixed=TRUE)
      }else{
        jsoncomments <- comments
      }
    }
    if(is.null(commentsAuthor)) commentsAuthor <- Sys.getenv("USERNAME")
    command <- sprintf("%s -d %s --header -c %s -a %s -o %s%s",
                       exe,
                       shQuote(as.character(jsondat)),
                       shQuote(as.character(jsoncomments)),
                       shQuote(commentsAuthor),
                       file,
                       ifelse(base64, " --base64", ""))
    #Encoding(command) <- "UTF-8"
    #cat(command)
  }
  if(base64){
    return(system(command, intern=TRUE))
  }else{
    system(command)
    return(invisible())
  }
}


#' @title Read a XLSX file
#' @description Read a sheet or all sheets in a XLSX file.
#' @param file xlsx file to be read
#' @param sheet name of the sheet to be read, or \code{NULL} to read all available sheets
#' @param header logical, whether the table has column headers
#' @param comments logical, whether to read the comments
#' @param firstRow integer, the starting row (guessed if \code{NULL}); ignored when \code{sheet=NULL}
#' @param lastRow integer, the ending row (guessed if \code{NULL}); ignored when \code{sheet=NULL}
#' @param output \code{"dataframe"} or \code{"JSON"}; see \strong{Value}
#' @return If \code{comments=FALSE} and \code{output="dataframe"}: a dataframe if \code{sheet} is not \code{NULL}, a list of dataframes if \code{sheet} is \code{NULL}.
#' If \code{comments=TRUE} and \code{output="dataframe"}: a list of two dataframes if \code{sheet} is not \code{NULL}, a list of list of two dataframes if \code{sheet} is \code{NULL}.
#' If \code{output="JSON"}: a JSON string.
#' @export
#' @importFrom jsonlite fromJSON
#' @examples \dontrun{
#' xlsx <- system.file("extdata/datasets.xlsx", package = "readxl")
#' # try to read an unavailable sheet:
#' XLSXread(xlsx, sheet="Sheet1")
#' # read an available sheet:
#' dat <- XLSXread(xlsx, sheet="iris")
#' # read all available sheets:
#' allsheets <- XLSXread(xlsx)
#' names(allsheets)}
XLSXread <- function(file, sheet=NULL, header=TRUE, comments=FALSE, firstRow=NULL, lastRow=NULL, output=c("dataframe", "JSON")){
  if(!is.null(sheet) && !is.null(lastRow)){
    if(lastRow<1 || (header && lastRow<2)){
      stop("`lastRow` must be >0 when `header=FALSE` and >1 when `header=TRUE`.")
    }
  }
  output <- match.arg(output)
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("xlsx2json2", exe_ext), package="SLutils")
  if(is.null(sheet)){
    if(!is.null(c(firstRow, lastRow))){
      warning("Reading all sheets (`sheet=NULL`), ignoring `firstRow` and `lastRow`.")
    }
    command <- sprintf("%s -f %s%s%s",
                       exe,
                       shQuote(file),
                       ifelse(header, " --header", ""),
                       ifelse(comments, " -c", ""))
  }else{
    command <- sprintf("%s -f %s%s -s %s%s%s%s",
                       exe,
                       shQuote(file),
                       ifelse(header, " --header", ""),
                       shQuote(sheet),
                       ifelse(comments, " -c", ""),
                       ifelse(is.null(firstRow), "", paste0(" -F ", firstRow)),
                       ifelse(is.null(lastRow), "", paste0(" -L ", lastRow))
    )
  }
  json <- paste0(system(command, intern=TRUE), collapse="") # paste because long output is split
  Encoding(json) <- "UTF-8"
  # if(comments){
  #   if(output=="dataframe"){
  #     return(jsonlite::fromJSON(json))
  #   }else{
  #     return(json)
  #   }
  # }else{
  if(output=="dataframe"){
    out <- jsonlite::fromJSON(json)
    if(!is.null(sheet) && substr(json, 1, 2) == "\"A"){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), out))
      return(invisible())
    }else{
      return(out)
    }
  }else{
    out <- json
    if(!is.null(sheet) && substr(json, 1, 2) == "\"A"){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), jsonlite::fromJSON(out)))
      return(invisible())
    }else{
      return(out)
    }
  }
  #  }
}

#' @title Read the comments in a XLSX file
#' @description Read the comments in the sheet of a XLSX file.
#' @param file xlsx file to be read
#' @param sheet name of the sheet to be read
#' @param header logical, whether the table has column headers
#' @param firstRow integer, the starting row (guessed if \code{NULL})
#' @param lastRow integer, the ending row (guessed if \code{NULL})
#' @param output \code{"dataframe"} or \code{"JSON"}; see \strong{Value}
#' @return A dataframe if \code{output="dataframe"}, a JSON string if \code{output="JSON"}.
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#' xlsx <- system.file("extdata/xlsx/xlsx_comments.xlsx", package = "SLutils")
#' XLSXcomments(xlsx, sheet="Sheet1")
XLSXcomments <- function(file, sheet="Sheet1", header=TRUE, firstRow=NULL, lastRow=NULL, output=c("dataframe", "JSON")){
  if(!is.null(lastRow)){
    if(lastRow<1 || (header && lastRow<2)){
      stop("`lastRow` must be >0 when `header=FALSE` and >1 when `header=TRUE`.")
    }
  }
  output <- match.arg(output)
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("getXLSXcomments", exe_ext), package="SLutils")
  command <- sprintf("%s -f %s%s -s %s%s%s",
                     exe,
                     shQuote(file),
                     ifelse(header, " --header", ""),
                     shQuote(sheet),
                     ifelse(is.null(firstRow), "", paste0(" -F ", firstRow)),
                     ifelse(is.null(lastRow), "", paste0(" -L ", lastRow))
  )
  json <- paste0(system(command, intern=TRUE), collapse="") # paste because long output is split
  Encoding(json) <- "UTF-8"
  if(output=="dataframe"){
    out <- jsonlite::fromJSON(json)
    if(!is.null(sheet) && !is.data.frame(out)){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), out))
      return(invisible())
    }else{
      return(out)
    }
  }else{
    out <- json
    if(!is.null(sheet) && substr(json, 1, 1) != "["){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), jsonlite::fromJSON(out)))
      return(invisible())
    }else{
      return(out)
    }
  }
}

#' @title Cell types in a XLSX sheet
#' @description Read the cell types in the sheet of a XLSX file.
#' @param file xlsx file to be read
#' @param sheet name of the sheet to be read
#' @param header logical, whether the table has column headers
#' @param firstRow integer, the starting row (guessed if \code{NULL})
#' @param lastRow integer, the ending row (guessed if \code{NULL})
#' @param output \code{"dataframe"} or \code{"JSON"}; see \strong{Value}
#' @return A dataframe if \code{output="dataframe"}, a JSON string if \code{output="JSON"}.
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#' xlsx <- system.file("extdata/xlsx/xlsx_comments.xlsx", package = "SLutils")
#' XLSXtypes(xlsx, sheet="Sheet1")
XLSXtypes <- function(file, sheet="Sheet1", header=TRUE, firstRow=NULL, lastRow=NULL, output=c("dataframe", "JSON")){
  if(!is.null(lastRow)){
    if(lastRow<1 || (header && lastRow<2)){
      stop("`lastRow` must be >0 when `header=FALSE` and >1 when `header=TRUE`.")
    }
  }
  output <- match.arg(output)
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("getXLSXtypes", exe_ext), package="SLutils")
  command <- sprintf("%s -f %s%s -s %s%s%s",
                     exe,
                     shQuote(file),
                     ifelse(header, " --header", ""),
                     shQuote(sheet),
                     ifelse(is.null(firstRow), "", paste0(" -F ", firstRow)),
                     ifelse(is.null(lastRow), "", paste0(" -L ", lastRow))
  )
  json <- paste0(system(command, intern=TRUE), collapse="") # paste because long output is split
  Encoding(json) <- "UTF-8"
  if(output=="dataframe"){
    out <- jsonlite::fromJSON(json)
    if(!is.null(sheet) && !is.data.frame(out)){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), out))
      return(invisible())
    }else{
      return(out)
    }
  }else{
    out <- json
    if(!is.null(sheet) && substr(json, 1, 1) != "["){
      warning(sprintf("No sheet or empty sheet `%s` in `%s` (%s).", sheet, basename(file), jsonlite::fromJSON(out)))
      return(invisible())
    }else{
      return(out)
    }
  }
}


#' @title Sheets in a XLSX file
#' @description Read the sheet names of a XLSX file.
#' @param file xlsx file to be read
#' @param output \code{"vector"} or \code{"JSON"}; see \strong{Value}
#' @return A character vector if \code{output="vector"}, a JSON string if \code{output="JSON"}.
#' @export
#' @importFrom jsonlite fromJSON
#' @examples
#' xlsx <- system.file("extdata/xlsx/xlsx_comments.xlsx", package = "SLutils")
#' XLSXsheets(xlsx)
XLSXsheets <- function(file, output=c("vector", "JSON")){
  output <- match.arg(output)
  exe_ext <- ifelse(.Platform$OS.type == "windows", ".exe", "")
  exe <- system.file("bin", .Platform$OS.type, paste0("getXLSXsheets", exe_ext), package="SLutils")
  command <- sprintf("%s -f %s",
                     exe,
                     shQuote(file))
  json <- paste0(system(command, intern=TRUE), collapse="") # paste because long output is split
  Encoding(json) <- "UTF-8"
  if(output=="vector"){
    out <- jsonlite::fromJSON(json)
  }else{
    out <- json
  }
  return(out)
}
