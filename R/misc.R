surround <- function(x, with) {
  if (!length(x)) return(character()) # ?
  paste0(with, x, with)
}

peq <- function(x, y) paste(x, y, sep = " = ")

sans_ext <- function(x) sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)

#' @title A modification of \code{Sys.which}
#' @description Find an executable without including the Windows system directories before \code{PATH}.
#' @param cmd name of possible executable to search
#' @return The path of the executable, if found, otherwise the empty string.
#' @note Credit to \href{http://stackoverflow.com/q/34030087/1100107}{Henrik Bengtsson}.
#' @export
#' @examples Sys.which("convert")
#' Sys.which2("convert")
Sys.which2 <- function(cmd) {
  stopifnot(length(cmd) == 1)
  if (.Platform$OS.type == "windows") {
    suppressWarnings({
      pathname <- shell(sprintf("where %s 2> NUL", cmd), intern=TRUE)[1]
    })
    if (!is.na(pathname)) return(setNames(pathname, cmd))
  }
  Sys.which(cmd)
}
