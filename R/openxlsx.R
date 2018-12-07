

#' @title Write a XLSX file from a flat list
#' @description Write a XLSX file from a flat list, allowing different types within columns
#' @param flatlist a list with structure like the output
#' of \code{\link[jsonlite]{fromJSON}} with option \code{simplifyVector=FALSE}
#' @param file name of the output xlsx file
#' @param sheetname name of the sheet to be written
#' @param comments comments, structure must be similar to \code{flatlist}
#' @param commentsAuthor author name of the comments
#' @param guessDates whether to guess dates and then write with these cells with date format
#' @param base64 logical, whether to get the file as a base64 string
#'
#' @return No value if \code{base64=FALSE}, otherwise a base64 string.
#' @export
#' @import openxlsx
#' @importFrom base64enc base64encode
#'
#' @examples \dontrun{
#' flatlist <- jsonlite::fromJSON("{\"COL1\":[1,null,2],\"COL2\":[\"c\",4],\"Date\":[\"2017-02-12\"]}", simplifyVector=FALSE)
#' comments <- jsonlite::fromJSON("{\"COL1\":[\"hello\",null,null],\"COL2\":[null,\"bye\"],\"Date\":[null]}", simplifyVector=FALSE)
#' writeXLSX(flatlist, file="xlsx.xlsx", comments=comments)}
writeXLSX <- function(flatlist, file, sheetname="Sheet1", comments=NULL, commentsAuthor=NULL, guessDates=TRUE, base64=FALSE){
  if(guessDates){
    dateStyle <- createStyle(numFmt = "yyyy/mm/dd")
    flatlist <- rapply(flatlist,
                     f=function(x) if(identical(Date <- guessDateFormat(x, returnDates=TRUE), FALSE)) x else as.Date(Date),
                     how="replace")
  }
  headerStyle <- createStyle(textDecoration = "Bold", border="Bottom")
  wb <- createWorkbook()
  addWorksheet(wb=wb, sheetName=sheetname)
  addStyle(wb, sheet=sheetname, style=headerStyle, rows=1L, cols=seq_along(flatlist))
  for(j in seq_along(flatlist)){
    # write column header
    writeData(wb, sheet=sheetname, x=names(flatlist)[j], startRow=1L, startCol=j)
    column <- flatlist[[j]]
    for(i in seq_along(column)){
      if(is_date(x <- column[[i]])){
        addStyle(wb, sheet=sheetname, style=dateStyle, rows=i+1L, cols=j)
      }
      writeData(wb, sheet=sheetname, x=x, startRow=i+1L, startCol=j)
      if(!is.null(comments) && !is.null(comment <- comments[[j]][[i]])){
        writeComment(wb, sheet=sheetname, col=j, row=i+1L,
                     comment=createComment(comment=comment, visible=FALSE,
                                           author=ifelse(is.null(commentsAuthor),
                                                         Sys.getenv("USERNAME"),
                                                         commentsAuthor)
                             )
        )
      }
    }
  }
  setColWidths(wb, sheet=sheetname, cols=seq_along(flatlist), widths=10)
  saveWorkbook(wb, file, overwrite=TRUE)
  if(base64){
    return(paste0("data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,", base64enc::base64encode(file)))
  }else{
    return(invisible())
  }
}
