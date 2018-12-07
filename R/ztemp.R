#' test
#' @export
ff <- function(x){
  system(sprintf("echo %s", x), intern=TRUE)
}
