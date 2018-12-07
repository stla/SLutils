#' Create html and js files for Scianimator
#'
#' @param name name of the html file (without extension)
#' @param imgs a list as the output of \code{\link{tikz2png}}: a field \code{files} containg the paths of the files, and a field \code{json} containing a JSON string
#' @param theme the theme, one of \code{""}, \code{"light"}, \code{"blue"}, \code{"dark"}
#' @param outdir output directory
#' @return No value. This creates the html file and its dependencies.
#' @importFrom stringr str_detect
#' @export
scianim <- function(name, imgs, theme="", outdir=getwd()){
  choices <- c("", "blue", "light", "dark")
  theme <- choices[charmatch(theme, table=choices)]
  if(is.na(theme)){
    stop('`theme` must be one of "", "blue", "light", "dark".')
  }
  # read and fill templates
  html <- readLines(system.file("scianimator", "index.html", package="SLutils"))
  js <- readLines(system.file("scianimator", "index.js", package="SLutils"))
  line <- which(stringr::str_detect(html, "%s"))
  html[line] <- sprintf(html[line], name)
  lines <- which(stringr::str_detect(js, "%s"))
  js[lines[1]] <- sprintf(js[lines[1]], imgs$json)
  js[lines[2]] <- sprintf(js[lines[2]], sprintf("'%s'", theme))
  # write
  owd <- setwd(outdir); on.exit(setwd(owd))
  htmlFile <- sprintf("%s.html", name)
  if(file.exists(htmlFile)){
    stop(sprintf("File %s found.", htmlFile))
  }
  writeLines(html, htmlFile)
  # copy css files
  cssFiles <- list.files(system.file("scianimator", "assets", "css", package="SLutils"), full.names=TRUE)
  to <- file.path(outdir, "assets", "css", basename(cssFiles))
  dir.create(file.path(outdir, "assets", "css"), recursive=TRUE)
  file.copy(cssFiles, to)
  # copy js files
  jsFiles <- list.files(system.file("scianimator", "assets", "js", package="SLutils"), full.names=TRUE)
  to <- file.path(outdir, "assets", "js", basename(jsFiles))
  dir.create(file.path(outdir, "assets", "js"), recursive=TRUE)
  file.copy(jsFiles, to)
  writeLines(js, file.path(outdir, "assets", "js", sprintf("%s.js", name)))
  # copy png files
  pngFiles <- normalizePath(imgs$files, winslash = .Platform$file.sep)
  to <- file.path(outdir, "assets", "images", basename(pngFiles))
  dir.create(file.path(outdir, "assets", "images"), recursive=TRUE)
  file.copy(pngFiles, to)
  #
  return(invisible())
}
