#' @title Enclose between dollars
#' @description Returns a function surrounding its input between \code{$}'s.
#' @return A function surrounding its input between \code{$}'s.
#' @export
#' @details This function is useful to be used in the \code{labels} argument of the \code{ggplot2} function \code{scale_[x/y]_continuous} (see the example of \code{\link{datify}}).
#' @note This function is useful for using \code{\link{plot2tikz}}.
#' @examples curve(x^2, from=-1, to=1, asp=1, axes=FALSE, xlab="$x$", ylab=NA)
#' xlabs_at <- seq(-1, 1, by=0.5)
#' axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
#' ylabs_at <- seq(0, 1, by=0.2)
#' axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
#' text(0.65, 0.8, "$f(x)=x\\\\^2$") # note the four backslashes!
dollarify <- function(){
  function(x) surround(x, with="$")
}

#' @title Format dates for LaTeX
#' @description Format dates to be used in LaTeX with \code{\\usepackage[ddmmyyyy]{datetime}}.
#' @details This function is useful to be used in the \code{labels} argument of the \code{ggplot2} function \code{\link[ggplot2]{scale_x_date}} (see the example).
#' @export
#' @importFrom stringr str_split_fixed
#' @note See \url{http://stackoverflow.com/questions/20326946/how-to-put-ggplot2-ticks-labels-between-dollars} for possible additional choices of the format.
#' @examples datify()(Sys.Date() + 0:1)
#' \dontrun{
#' # a base plot example
#' plotCode <- function(){
#'   par(mar=c(7,4,4,2)+0.1)
#'   plot(1:8, pch=19, xaxt="n", yaxt="n", xlab=NA, ylab="$y$")
#'   axis(2, at=1:8, labels=dollarify()(1:8))
#'   axis(1, labels=FALSE)
#'   labels <- datify()(Sys.Date()-1:8)
#'   text(1:8, par("usr")[3] - 0.5, srt=45, adj=1,
#'        labels=labels, xpd=TRUE)
#'   mtext(1, text ="\\textsf{date}", line = 5)
#' }
#' # create TikZ plot
#' plot2tikz(plotCode, compile=FALSE, outdir=tempdir(),  overwrite = TRUE,
#'           packages=c("\\usepackage[ddmmyyyy]{datetime}\n"),
#'           documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'           width=7, height=5)}
#' \dontrun{
#' # a ggplot2 example
#' library(ggplot2)
#' DF <- data.frame(date=Sys.Date()+1:20, y=rpois(20, lambda=30))
#' gg <- ggplot(DF, aes(x=date, y=y)) +
#'   geom_point(size=4, color="yellow", shape=21, fill="green") +
#'   scale_y_continuous(labels=dollarify()) +
#'   scale_x_date(date_breaks="2 days", labels=datify()) +
#'   xlab("\\large\\textsf{date}") +
#'   ylab("\\Large $y$") +
#'   theme(axis.text.x = element_text(face="bold", angle=45, vjust=0.5, size=12, lineheight=0.9),
#'         axis.text.y = element_text(face="bold", size=16, lineheight=0.9, hjust=1),
#'         axis.ticks = element_line(size=0.2),
#'         axis.title.x = element_text(size=20, vjust=0),
#'         axis.title.y = element_text(face="bold", size=16, vjust=1),
#'         axis.ticks.length = unit(0.3, "lines"),
#'         axis.text = element_text(margin=unit(0.5, "lines"))
#'   )
#' # create TikZ plot
#' plot2tikz(function() print(gg), compile=FALSE, outdir=tempdir(), overwrite = TRUE,
#'           packages=c("\\usepackage[ddmmyyyy]{datetime}\n"),
#'           documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'           width=7, height=5)}
datify <- function(){
  function(x){
    split <- stringr::str_split_fixed(as.character(x),"-",3)
    out <- character(nrow(split))
    for(i in 1:length(out)){
      out[i] <- paste0("\\formatdate{", split[i,3], "}{", split[i,2], "}{", split[i,1], "}")
    }
    out
  }
}

#' @title Convert a plot to a TikZ figure
#' @description Convert a R plot to a TikZ figure
#' @param code a function without argument generating the plot
#' @param filename the name of the tex file, without extension
#' @param outdir output directory
#' @param overwrite logical, whether to overwrite if the file exists
#' @param compile logical, whether to compile the tex file; automatically \code{TRUE} if \code{format="eps"}
#' @param format the output format, one of \code{"pdf"}, \code{"ps"}, or \code{"eps"}
#' @param lua logical, wheter to use \code{lualatex}
#' @param packages \code{NULL} to use default packages, otherwise see examples
#' @param addDefaultTikZoptions logical; for \code{format="pdf"} only and if \code{packages} is not \code{NULL}, then this adds the default packages given in \code{getOption("tikzLatexPackages")}
#' @param clean logical, whether to remove the auxiliary files
#' @param ... arguments passed to \code{\link[tikzDevice]{tikz}}, such as \code{documentDeclaration}, \code{width} and \code{height}.
#' @return No value, only prints some messages.
#' @importFrom tikzDevice tikz
#' @importFrom tools texi2dvi
#' @importFrom stringr str_replace
#' @importFrom grDevices dev.off
#' @export
#' @seealso Examples in \code{\link{datify}}.
#' @examples \dontrun{
#' # first write a function creating the plot
#' plotCode <- function(){
#'   curve(x^2, from=-1, to=1, asp=1, axes=FALSE, xlab="$x$", ylab=NA)
#'   xlabs_at <- seq(-1, 1, by=0.5)
#'   axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
#'   ylabs_at <- seq(0, 1, by=0.2)
#'   axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
#'   text(0.65, 0.8, "$f(x)=x\\\\^2$") # note the four backslashes!
#'   return(invisible())
#' }
#' # try the plot:
#' plotCode()
#' # create the LaTeX file with default preamble for pdflatex
#' plot2tikz(plotCode, outdir=tempdir(), compile=FALSE,
#'   documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'   width=7, height=5)
#' # add some packages, or other commands in the preamble
#' plot2tikz(plotCode, compile=FALSE, outdir=tempdir(), overwrite = TRUE,
#'           packages=c("\\usepackage[active,tightpage,psfixbb]{preview}\n",
#'                      "\\PreviewEnvironment{pgfpicture}\n",
#'                      "\\setlength\\PreviewBorder{10pt}\n",
#'                      "\\usepackage{amssymb}\n"),
#'           documentDeclaration ="\\documentclass[12pt]{standalone}\n",
#'           width=7, height=5)}
plot2tikz <- function(code, filename="Rplot", outdir=getwd(),
                      overwrite=FALSE, format="pdf", lua=FALSE,
                      packages=NULL, addDefaultTikZoptions=TRUE,
                      compile=TRUE, clean=FALSE, ...){
  format <- match.arg(format, choices = c("pdf", "ps", "eps"))
  texfile <- paste0(filename, ".tex")
  owd <- setwd(outdir); on.exit(setwd(owd))
  if(overwrite || !file.exists(texfile)){
    #     if(!"packages" %in% names(list(...))){
    #       packages <- getOption("tikzLatexPackages")
    #       extra.args <- list(...)
    #     } else {
    #       extra.args0 <- list(...)
    #       extra.args <- extra.args0[!names(extra.args0) %in% "packages"]
    #       packages <- extra.args0$packages
    #     }
    #     do.call(function(...) tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...), extra.args)
    if(is.null(packages)){
      if(format=="pdf") packages <- getOption("tikzLatexPackages")
      if(format %in% c("ps", "eps")) packages <- c("\\thispagestyle{empty}\n", "\\usepackage{tikz}\n")
    } else {
      if(!"\\usepackage{tikz}\n" %in% packages){
        packages <- c("\\usepackage{tikz}\n", packages)
        if(format=="pdf" && addDefaultTikZoptions){
          packages <- union(packages, getOption("tikzLatexPackages"))
        }
      }
    }
    tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...)
    code()
    grDevices::dev.off()
  }
  if(compile || format=="eps"){
    message("Compilation...")
    if(format=="pdf"){
      # pdf compilation
      pdffile <- stringr::str_replace(texfile, ".tex", ".pdf")
      if(overwrite || !file.exists(pdffile)){
        if(lua){
          command <- sprintf("lualatex %s", texfile)
          system(command)
        }else{
          tools::texi2dvi(texfile, pdf=TRUE, clean=clean)
        }
        message(sprintf("Output pdf file: %s.pdf", filename))
      }
    } else if(format %in% c("ps", "eps")){
      psfile <- stringr::str_replace(texfile, ".tex", ".ps")
      if(overwrite || !file.exists(psfile)){
        tools::texi2dvi(texfile, pdf=FALSE, clean=clean)
        command <- sprintf("dvips %s.dvi", filename)
        system(command)
        message(sprintf("Output ps file: %s.ps", filename))
        if(format=="eps"){
          command <- sprintf("ps2epsi %s.ps %s.epi", filename, filename)
          system(command)
          file.rename(sprintf("%s.epi", filename), sprintf("%s.eps", filename))
          message(sprintf("Output eps file: %s.eps", filename))
        }
      }
    }
  }
  #
  message(sprintf("Output tex file: %s", normalizePath(texfile, winslash=.Platform$file.sep)))
  return(invisible())
}


#' @title Generate LaTeX figures in png or gif format
#' @description Generate a series of TikZ figures in png or gif format
#' @param code a function with an integer argument, generating the plot
#' @param nplots an integer vector, each value is applied to the \code{code} function
#' @param prefix the prefix for the generated files
#' @param format the desired output format, \code{"png"} or \code{"gif"}
#' @param outdir output directory
#' @param folder this is intended to be used with \code{\link{scianim}}
#' @param ... arguments passed to \code{\link[tikzDevice]{tikz}}
#' @return A list with two components, the paths of the generated files and a JSON string intended to be used with \code{\link{scianim}}.
#' @importFrom tikzDevice tikz
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_replace
#' @importFrom grDevices dev.off
#' @export
#' @examples \dontrun{
#' plotCode <- function(n){
#'   curve(x^n, from=-1, to=1, axes=FALSE,
#'         ylab=NA, xlab="$x$",
#'         main=sprintf("$f(x)=x\\\\^%s$", n))
#'   xlabs_at <- seq(-1, 1, by=0.5)
#'   axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
#'   ylabs_at <- seq(ifelse(n%%2==0, 0, -1), 1, by=0.2)
#'   axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
#'   return(invisible())
#' }
#' tikz2png(plotCode, 1:3, outdir = tempdir())}
tikz2png <- function(code, nplots, prefix="Rplot", format="png", outdir=getwd(), folder="assets/images/", ...){
  convert <- Sys.which2("convert")
  if(!nzchar(convert)){
    stop("The `convert` command of ImageMagick was not found.")
  }
  if(!nzchar(Sys.which("gswin32c"))){
    stop("The `gswin32c` command of Ghostscript was not found.")
  }
  texfiles <- charseq(length(nplots), prefix=prefix, suffix=".tex")
  owd <- setwd(outdir); on.exit(setwd(owd))
  if(!all(file.exists(texfiles))){
    tikzDevice::tikz(attr(texfiles, "string"), standAlone=TRUE, onefile=FALSE, ...)
    for(i in nplots){
      code(i)
    }
    grDevices::dev.off()
  }
  # pdf compilation
  pdffiles <- stringr::str_replace(texfiles, ".tex", ".pdf")
  if(!all(file.exists(pdffiles))){
    sapply(texfiles, function(tex) tools::texi2dvi(tex, pdf=TRUE, clean=TRUE))
  }
  # conversion png ImageMagick
  pngfiles <- stringr::str_replace(texfiles, ".tex", paste0(".",format))
  for(i in nplots){
    command <- sprintf("%s -density 300 -resize %s %s %s", convert, "30%", pdffiles[i], pngfiles[i])
    system(command)
  }
  return(list(files=file.path(outdir, pngfiles), json=jsonlite::toJSON(paste0(folder,pngfiles)) ))
}

