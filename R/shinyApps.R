.SLenv <- new.env()
assign("dat", readRDS(system.file("shinyApps", "dat01.rds", package = "SLutils")), envir = .SLenv)


#' @title Shiny Reshape app
#' @export
#' @import shiny
#' @import readxl
#' @importFrom purrr map_lgl
#' @importFrom jsonlite fromJSON
#' @importFrom knitr knit2html
shinyReshape <- function(dat=NULL){
  appDir <- system.file("shinyApps", "shinyReshape", package = "SLutils")
  if(is.null(dat)){
    dat <- readRDS(system.file("shinyApps", "dat01.rds", package = "SLutils"))
    # dat <-
    #   structure(
    #     list(
    #       Test = structure(
    #         c(1L, 1L, 1L, 2L, 2L, 2L),
    #         .Label = c("A", "B"),
    #         class = "factor"
    #       ),
    #       Batch = structure(
    #         c(1L, 2L, 3L, 1L, 2L, 3L),
    #         .Label = c("batch1", "batch2", "batch3"),
    #         class = "factor"
    #       ),
    #       Result1 = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
    #       Result2 = c(10.1, 10.2, 10.3, 10.4, 10.5, 10.6),
    #       Spec = c(2L, 2L, 2L, 20L, 20L, 30L)
    #     ),
    #     .Names = c("Test", "Batch", "Result1", "Result2", "Spec"),
    #     class = "data.frame",
    #     row.names = c(NA,-6L)
    #   )
  }
  assign("dat", dat, envir = .SLenv)
  app <- shinyAppDir(appDir) # options onStart plutôt que .SLenv ?
  #print(app)
  shiny::runApp(app, display.mode = "normal", launch.browser = TRUE)
}


#' #' @title Shiny Table app
#' #' @export
#' #' @import shiny
#' #' @import shinythemes
#' #' @import readxl
#' #' @importFrom purrr map_lgl
#' #' @importFrom base64enc base64encode
#' shinyTable <- function(dat=NULL){
#'   appDir <- system.file("shinyApps", "shinyTable", package = "SLutils")
#'   if(is.null(dat)){
#'     dat <-
#'       structure(
#'         list(
#'           Test = structure(
#'             c(1L, 1L, 1L, 2L, 2L, 2L),
#'             .Label = c("A", "B"),
#'             class = "factor"
#'           ),
#'           Batch = structure(
#'             c(1L, 2L, 3L, 1L, 2L, 3L),
#'             .Label = c("batch1", "batch2", "batch3"),
#'             class = "factor"
#'           ),
#'           Result1 = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
#'           Result2 = c(10.1, 10.2, 10.3, 10.4, 10.5, 10.6),
#'           Spec = c(2L, 2L, 2L, 20L, 20L, 30L)
#'         ),
#'         .Names = c("Test", "Batch", "Result1", "Result2", "Spec"),
#'         class = "data.frame",
#'         row.names = c(NA,-6L)
#'       )
#'   }
#'   app <- shinyAppDir(appDir) # options onStart plutôt que .SLenv ?
#'   assign("dat", dat, envir = .SLenv)
#'   print(app)
#'   #shiny::runApp(app, display.mode = "normal")
#' }
#'
