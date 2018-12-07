# .onLoad <- function(libname, pkgname){
#   #> tools:::file_path_as_absolute
#   file_path_as_absolute <- function (x)
#   {
#     if (length(x) != 1L)
#       stop("'x' must be a single character string")
#     if (!file.exists(epath <- path.expand(x)))
#       stop(gettextf("file '%s' does not exist", x), domain = NA)
#     normalizePath(epath, "/", TRUE)
#   }
#   #require(tools)
#   #R.utils:::reassignInPackage("file_path_as_absolute", "tools", file_path_as_absolute)
#   #assignInNamespace("file_path_as_absolute", file_path_as_absolute, ns="tools")
#   assignInMyNamespace("file_path_as_absolute", file_path_as_absolute)
# }
# trace: https://groups.google.com/forum/#!msg/r-help-archive/MrtK6injoZY/LoZvp4Tw1i0J
# essayer file_path_as.. <<- ?.. nop..
# ET SI JE METS TOOLS EN DEPENDS ???

# .onLoad <- function(libname, pkgname) {
#   library.dynam("xlsx2jsonR", pkgname, libname, now=TRUE)
#   .C("HsStart")
#   invisible()
# }
#
# .onUnLoad <- function(libpath) {
#   library.dynam.unload("xlsx2jsonR", libpath)
#   invisible()
# }
#
# setwd("C:/HaskellProjects/jsonxlsxR"); dyn.load("HSdll.dll")
# .C("HsStart"); xx <- .C("readBS", file="z.txt", result="")$result
#.C("xlsxjsonR", bs=xx, sheet="Sheet1", what="data,c", header=1L, result="")$result
#.C("f", file="../jsonxlsx/tests_XLSXfiles/simpleExcel.xlsx", sheet="Sheet1", what="data,c", header=1L, result="")$result

