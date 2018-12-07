#' @title Find files before/after a given data with size lower/bigger than a given size
#' @description Find files before/after a given data with size lower/bigger than a given size.
#' @param path the path to search in
#' @param date the reference date
#' @param before logical, whether to search files before the reference date
#' @param size reference size
#' @param bigger logical, whether to search file bigger than the reference size
#' @param ... arguments passed to \code{\link[base]{list.files}}
#' @return The found files, in a vector.
#' @export
findFiles <- function(path=".", date=Sys.Date(), before=TRUE, size=0, bigger=TRUE, ...){
  files <- list.files(path=path, include.dirs=FALSE, no..=TRUE, ...)
  infos <- file.info(files)
  #files <- files[!infos$isdir]
  infos <- subset(infos, subset = !`isdir`)
  infos$Date <- as.Date(infos$mtime)
  if(before){
    infos <- subset(infos, subset = `Date`<=date)
  }else{
    infos <- subset(infos, subset = `Date`>date)
  }
  if(bigger){
    infos <- infos[infos$size >= size, ]
  }else{
    infos <- infos[infos$size < size, ]
  }
  return(rownames(infos))
}
