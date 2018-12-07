#' @title Convert (e)ps to png/gif
#' @description Convert a (e)ps file to png or gif
#' @param infile input file
#' @param format output format
#' @param moreopts more options to \code{convert} of ImageMagick
#' @param ps2eps apply \code{ps2eps} before converting, for a ps input file
#' @return Nothing, this only creates the file.
#' @export
#' @importFrom tools file_ext
#' @examples \dontrun{
#' pdf("Rplot.pdf")
#' curve(x^2, from=-1, to=1, axes=FALSE, ylab=NA, col="red")
#' xlabs_at <- seq(-1, 1, by=0.5)
#' axis(1, at=xlabs_at)
#' ylabs_at <- seq(0, 1, by=0.2)
#' axis(2, at=ylabs_at)
#' text(0.65, 0.8, expression(f(x)==x^2))
#' dev.off()
#' pspdf2img("Rplot.pdf", format="png", moreopts="-trim -resize 50%")}
pspdf2img <- function(infile, format="png", moreopts="", ps2eps=FALSE){
  convert <- Sys.which2("convert")
  if(!nzchar(convert)){
    stop("The `convert` command of ImageMagick was not found.")
  }
  if(!nzchar(Sys.which("gswin32c"))){
    stop("The `gswin32c` command of Ghostscript was not found.")
  }
  outfile <- paste0(sans_ext(infile), ".", format)
  if(ps2eps && tools::file_ext(infile)=="ps"){
    command <- sprintf("ps2epsi %s %s.epi", infile, sans_ext(infile))
    system(command)
    file.rename(sprintf("%s.epi", sans_ext(infile)), sprintf("%s.eps", sans_ext(infile)))
    infile <- paste0(sans_ext(infile), ".eps")
  }
  command <- sprintf("%s -transparent white -density 300 %s %s %s", convert, moreopts, infile, outfile)
  #   command <- ifelse(format=="gif", sprintf("convert -trim %s %s", psfile, pngfile),
  #                     sprintf("gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
  #                     #sprintf("gs -r300 -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
  #                     )
  # ça coupe le bas, finalement: convert -density 300 in.eps out.png
  # aussi -resize 600x400 -transparent white
  system(command)
  return(invisible())
}

