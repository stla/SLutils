excelIndex <- function(n){
  if(n <= 26){
    return(LETTERS[n])
  }else{
    return(paste0(excelIndex(floor((n-1)/26)), excelIndex((n-1)%%26+1)))
  }
}

myPrettyNum <- Vectorize(function(x){
  i <- 9L
  y <- prettyNum(x, digits=i)
  while(nchar(y)>9L){
    i <- i-1L
    y <- prettyNum(x, digits=i)
  }
  return(y)
})

#' @importFrom purrr map_chr
#' @importFrom stats na.omit
numFmtId <- function(col){
  col <- na.omit(col)
  x <- myPrettyNum(col)
  if(all(as.numeric(x)==col)){
    return("General")
  }else if(any(grepl("e", x))){ #scientific
    x <- x[which(grepl("e", x))]
    x <- x[which(nchar(x)==max(nchar(x)))]
    if(any(!grepl(".", x))){
      return("0E+00")
    }
    d <- min(nchar(purrr::map_chr(regmatches(x, gregexpr("[[:digit:]]+", x)), `[`, 2L)))
    return(paste0("0.", paste0(rep("0",d),collapse=""), "E+00"))
  }else{
    x <- x[which(nchar(x)==max(nchar(x)))]
    if(any(!grepl("\\.", x))){
      return("0")
    }
    d <- min(nchar(purrr::map_chr(regmatches(x, gregexpr("[[:digit:]]+", x)), `[`, 2L)))
    return(paste0("0.", paste0(rep("0",d),collapse="")))
  }
}

#' @importFrom stats setNames na.omit
makeList <- function(col, comments=NULL, excelCol, name){
  cl0 <- class(col)
  cl <- ifelse(any(c("character", "factor") %in% cl0),
               "character",
               "other")
  if(cl != "character"){
    cl <- ifelse(any(c("Date", "POSIXt") %in% cl0),
                 "Date",
                 ifelse("logical" %in% cl0,
                        "logical",
                        "number"))
  }
  t <- ifelse(cl == "character",
              "s",
              ifelse(cl == "Date",
                     "d",
                     ifelse(cl == "logical",
                            "b",
                            "n")))
  dd <- data.frame(cell = paste0(excelCol, seq_along(col)+1L),
                   t = t,
                   stringsAsFactors = FALSE)
  if(cl == "character"){
    dd$v <- as.character(col)
  }else if(cl == "Date"){
    # commented below requires t = "n"
    # see ?as.Date for date origin in Excel
    #dd$v <- as.integer(difftime(as.Date(col), as.Date("1899-12-30"), units = "days"))+1L
    #dd$z <- "m/d/yy" # bizarre mais ça marche comme ça alors que ça rend d/m/yy
    # essayer d-mmm-yy
    dd$v <- as.character(col)
  }else if (cl == "logical"){
    dd$v <- col
  }else{
    dd$v <- col
    dd$z <- numFmtId(col)
  }
  if(!is.null(comments)){
    dd$c <- comments
  }
  dd <- subset(dd, !is.na(`v`))
  ll <- setNames(vector("list", nrow(dd)), dd$cell)
  for(i in seq_along(ll)){
    ll[[i]] <- as.list(dd[i, -1L])
    if("c" %in% names(ll[[i]])){
      ll[[i]]$c <- list(list(a="JOHN", t=ll[[i]]$c))
      # et virer ici si NA
    }
  }
  ll1 <- setNames(list(list(t="s", v=name)), paste0(excelCol,1))
  ll <- c(ll1, ll)
  return(ll)
}

#' @importFrom purrr pmap map_chr
makeSheet <- function(dat){
  excelCols <- purrr::map_chr(seq_along(dat), excelIndex)
  dat_comments <- attr(dat, "comments")
  if(is.null(dat_comments)){
    dat_comments <- vector("list", ncol(dat))
  }
  sheet <- do.call(c,
                   unname(purrr::pmap(list(col=dat,
                                           comments=dat_comments,
                                           excelCol=excelCols,
                                           name=colnames(dat)
                                           ),
                                      makeList))
                   )
  sheet <- c(list(`!ref` = paste0(excelCols[1], 1, ":", excelCols[length(dat)], nrow(dat)+1L)),
             sheet)
  sheet <- rapply(sheet, how="replace", f = jsonlite::unbox)
  sheet <- c(sheet, list(`!cols` = data.frame(wch=rep(10,ncol(dat)))))
  # widths: http://stackoverflow.com/questions/24395693/how-to-set-cell-width-when-export-xlsx-files-with-js-xlsx
  return(sheet)
}

#' @importFrom jsonlite unbox toJSON
makeWorkbook <- function(..., sheetNames=NULL, author="", title=""){
  dats <- list(...)
  if(is.null(sheetNames)){
    sheetNames <- paste0("Sheet", seq_along(dats))
  }
  sheets <- setNames(lapply(dats, makeSheet), sheetNames)
  Props <- lapply(list(Author=author, Title=title), jsonlite::unbox)
  wb <- list(Sheets=sheets, SheetNames=sheetNames, Props=Props)
  wbJSON <- jsonlite::toJSON(wb, digits=14) # 14 maxi ("caveeat" ...)
  return(wbJSON)
}

#' Write XLSX file
#'
#' @param ... dataframes to be written (one dataframe per sheet)
#' @param file output xlsx file
#' @param sheetNames vector of sheet names; if \code{NULL}, default to \code{Sheet1}, \code{Sheet2}, ...
#' @param author author name
#' @param title title
#' @export
#' @examples \dontrun{
#' XLSXwrite0(iris[1:20,], mtcars, file="test.xlsx",
#'           author="John Doe", title="MyTitle")
#' }
#' @note Current caveats:
#' \itemize{
#'      \item The number of digits is limited to 14.
#' }
#'
XLSXwrite0 <- function(..., file, sheetNames=NULL, author="", title=""){
  if(.Platform$OS.type=="windows"){
    wbJSON <- makeWorkbook(..., sheetNames=sheetNames, author=author, title=title)
    tmpJSON <- tempfile(fileext = ".json")
    cat(wbJSON, file=tmpJSON)
    exe <- system.file("bin", "windows", "XLSXwrite.exe", package="SLutils")
    command <- sprintf("%s %s %s",
                       exe, tmpJSON, file)
    system(command, intern = TRUE)
    return(invisible())
  }else{
    stop("This function is supported on Windows only.", call.=FALSE)
  }
}
