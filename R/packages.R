#' Install some packages in "vanilla" mode
#' @description Install some packages with option \code{--vanilla}.
#' @param pkgs vector of packages to be installed
#' @param ... passed to \code{\link[utils]{install.packages}}
#' @examples \dontrun{install.packages.vanilla2("abind"))}
#' @export
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
install.packages.vanilla2 <- function(pkgs, ...){

  # arguments <- list(...)
  #
  # names_args <- names(arguments)
  # # gérer le cas où le user n'a pas mis pkgs="xxx"
  #   # 3 possibilités : - length=1 et names = NULL
  #   # length > 1 et names=NULL
  #   # length >1 et "" dans names
  # # if(length(names_args)>1){
  # #   m <- which(names_args=="")

  # ou comme ça (avec pkgs en argument) :
  arguments <- as.list(match.call())[-1]

  if(!"repos" %in% names(arguments)){
    arguments$repos <- getOption("repos")
  }
  names_args <- names(arguments)

  installArgs <- purrr::map_chr(seq_along(arguments),
                                function(i){
                                  # value <- arguments[[i]]
                                  # if(FALSE && length(value)<2){
                                  #   peq(names(arguments)[i],
                                  #       ifelse(is.null(value) || is.logical(value), deparse(value), surround(arguments[[i]], with="'")))
                                  # }else{
                                  #   peq(names(arguments)[i], deparse(value))
                                  # }
                                  # peq(names_args[i], deparse(arguments[[i]]))
                                  value <- arguments[[i]]
                                  peq(names_args[i], ifelse(length(value)<2, deparse(value), as.character(list(value))))
                                })
  installArgs <- stringr::str_replace_all(installArgs, "\"", "'")
  installCmd <- paste0("utils::install.packages(",
                       paste(installArgs, collapse = ", "),
                       ")")
  fullCmd <- paste(
    surround(file.path(R.home("bin"), "R"), with = "\""),
    "--vanilla",
    "--slave",
    "-e",
    surround(installCmd, with = "\"")
  )

  #return(fullCmd)

  system(fullCmd)
  return(invisible())
}


#' @title Install a package in "vanilla" mode
#' @description Install a package with option \code{--vanilla}.
#' @param pkgSource the path (absolute or relative) to the tar.gz bundle
#' @param lib character, the library directory where to install the package; if missing, defaults to the first element of \code{.libPaths()}
#' @return The output of the system command.
#' @export
install.package.vanilla <- function(pkgSource, lib){

  pkgSource <- normalizePath(pkgSource, winslash = .Platform$file.sep)
  if(missing(lib)) lib <- .libPaths()[1]

  installArgs <- c(
    peq("pkgs", surround(pkgSource, with = "'")),
    peq("lib", surround(lib, with = "'")),
    peq("repos", "NULL"),
    peq("type", surround("source", with = "'"))
  )
  installCmd <- paste0("utils::install.packages(",
                      paste(installArgs, collapse = ", "),
                      ")")
  fullCmd <- paste(
    surround(file.path(R.home("bin"), "R"), with = "\""),
    "--vanilla",
    "--slave",
    "-e",
    surround(installCmd, with = "\"")
  )

  return(system(fullCmd, intern=TRUE))
}

#' Create a new folder
#' @description Create a new folder.
#' @param folder path to the folder to create (absolute or relative)
#' @param overwrite logical, whether to overwrite if the folder already exists
#' @return The path of the created folder, if it is created.
#' @export
#' @importFrom withr with_options
createFolder <- function(folder, overwrite=FALSE){
  #path <- suppressWarnings(normalizePath(folder, winslash = .Platform$file.sep))
  path <- suppressWarnings(path.expand(folder))
  path <- sub(path, pattern="/$", replacement="\\1") # for unlink()
  if(!file.exists(path)){
    message(sprintf("Creating new folder %s", path))
    withr::with_options(c(warn=2), dir.create(path, recursive=TRUE))
  }else{
    if(!file.info(path)$isdir){
      stop("The specified folder exists but is not a directory.")
    }else if(length(list.files(path, all.files=TRUE, no..=TRUE))){
     if(!overwrite){
        stop("The specified folder already exists and is not empty.")
      }else{
        message(sprintf("Deleting current folder %s", path))
        unlink(path, recursive=TRUE)
        # ça crash si le user est dedans, deux niveaux plus bas!
          # faire un try catch ?
        message(sprintf("Creating new folder %s", path))
        withr::with_options(c(warn=2), dir.create(path, recursive=TRUE))
      }
    }
  }
  return(path)
}

getRprefix <- function(Rversion){
  paste(Rversion$major, Rversion$minor, sep=".")
}

#' Paths of the CRAN-like subfolders
#'Return the structure of a CRAN-like repo.
#' @note This function is rather for internal use and is not exported.
#' @param repopath path to the repo (not necessarily an existing one)
#' @return The structure of the repo.
subfoldersCRANlike <- function(repopath){
  #path <- suppressWarnings(normalizePath(folder, winslash = .Platform$file.sep))
  path <- repopath # anyway I call this function with an absolute path
  contribDir <- file.path(path, "src", "contrib")
  ArchiveDir <- file.path(contribDir, "Archive")
  MetaDir <- file.path(contribDir, "Meta")
  winBindir <- file.path(path, "bin", "windows", "contrib")
  # Binary paths are versioned based on R -- we'll create a path
  # for our current version of R, but leave it empty.
  rVersion <- getRprefix(getRversion())
  #rVersion <- paste(unlist(getRversion())[1:2], collapse = ".")
  binPaths <- list(
    win.binary = file.path("bin/windows/contrib", rVersion),
    mac.binary = file.path("bin/macosx/contrib", rVersion),
    mac.binary.mavericks = file.path("bin/macosx/mavericks/contrib", rVersion),
    mac.binary.leopard = file.path("bin/macosx/leopard/contrib", rVersion),
    Archive = file.path("bin/windows/contrib", rVersion, "Archive"),
    Meta = file.path("bin/windows/contrib", rVersion, "Meta")
  )
  binPaths <- lapply(binPaths, function(x) file.path(path, x))

  return(list(winBindir=winBindir, contribDir=contribDir, ArchiveDir=ArchiveDir, MetaDir=MetaDir, binPaths=binPaths))
}

#' Create a CRAN-like repo
#'
#' @param repopath path to the repo (absolute or relative)
#' @param name name of the repo
#' @param overwrite logical, whether to overwrite if the repo already exists
#' @return The full path of the repo.
#' @export
makeRepo <- function(repopath, name, overwrite=FALSE){
  path <- createFolder(folder=repopath, overwrite=overwrite)
  message("Creating repo structure")
  subfolders <- subfoldersCRANlike(path)
  binPaths <- subfolders$binPaths
  dir.create(subfolders$contribDir, recursive = TRUE)
  dir.create(subfolders$ArchiveDir)
  dir.create(subfolders$MetaDir)
  invisible(lapply(binPaths, function(x) {
    dir.create(x, recursive = TRUE)
  }))
  # create name file
  invisible(file.create(file.path(path, paste0(name, ".name"))))
  #
  message(sprintf("Repo `%s` created in %s.", name, path))
  return(path)
}

#' @title Get packages versions in a repo
#' @description Return the packages in a repo and their version.
#' @note This function is rather for internal use and is not exported.
#' @param repopath path to the repo
#' @param pkgs the packages to look for
#' @param type type of the packages to archive, one of \code{"source"} or \code{"binary"}
#' @return The packages and their version in a dataframe.
#' @importFrom purrr map_chr
#' @export
repoStatus <- function (repopath, pkgs, type="source")
{
  type <- match.arg(type, choices = c("source", "binary"))
  if(type=="source"){
    contribDir <- file.path(repopath, "src", "contrib")
  }else{
    rVersion <- getRprefix(getRversion())
    contribDir <- file.path(repopath, "bin", "windows", "contrib", rVersion)
  }
  ext <- ifelse(type=="source", "\\.tar\\..*$", "\\.zip$")
  files <- list.files(contribDir, pattern=ext, full.names=FALSE,
                      recursive=(type != "source"))
  noextfiles <- gsub(ext, "", files)
  found_pkgs <- purrr::map_chr(strsplit(files, "_", fixed = TRUE), `[`, 1L)
  verstxt <- gsub("[a-zA-Z0-9\\.]*_", "", noextfiles)
  vers <- package_version(verstxt)
  df <- data.frame(file = files, package = found_pkgs, version = vers,
                   stringsAsFactors = FALSE)
  df <- df[order(df$package, df$version, decreasing = TRUE),
           ]
  df$newest <- !duplicated(df$package)
  df <- df[order(df$package, df$version, decreasing = FALSE),
           ]
  df <- df[df$package %in% pkgs, ]

  return(df)
}

#' @title Archive packages
#' @description Archive older versions of packages found in a repo.
#' @note This function is rather for internal use and is not exported.
#' @param repopath path to the repo
#' @param pkgs the packages to be archived
#' @param type type of the packages to archive, one of \code{"source"} or \code{"binary"}
#' @return An invisible logical, indicating whether archiving has occured.
archivePackages <- function(repopath, pkgs, type="source") {

  type <- match.arg(type, choices = c("source", "binary"))
  if(type=="source"){
    contribDir <- file.path(repopath, "src", "contrib")
  }else{
    rVersion <- getRprefix(getRversion())
    contribDir <- file.path(repopath, "bin", "windows", "contrib", rVersion)
  }

  mkArchive <- function(x) {
    parchive <- file.path(contribDir, "Archive", x)
    if (!file.exists(parchive)) {
      if (!dir.create(parchive, recursive = TRUE)) {
        stop("Package archive directory for ", x," not found and couldn't be created\n", call.=FALSE)
      }else{
        return(invisible())
      }
    }else{
      return(invisible())
    }
  }

  old <- repoStatus(repopath = repopath, pkgs = pkgs, type=type)
  old <- old[!old[,"newest"] & old[,"package"] %in% pkgs, ]
  if(nrow(old)){
    message(paste0("Archiving older version", rep("s", nrow(old)>1)))
    sapply(unique(old$package), mkArchive)
    invisible(file.rename(file.path(contribDir, old$file), file.path(contribDir, "Archive", old$package, old$file)))
    return(invisible(TRUE))
  }else{
    message("No old version to archive")
    invisible(FALSE)
  }

}

#' @title Make the archive.rds file of a repo
#' @description Make the archive.rds file of a CRAN-like repo.
#' @note This function is rather for internal use and is not exported.
#' @param repopath path to the repo
#' @param type type of the packages to archive, one of \code{"source"} or \code{"binary"}
#' @return Nothing.
#' @importFrom purrr map_chr
makeArchiveRDS <- function(repopath, type="source"){
  type <- match.arg(type, choices = c("source", "binary"))
  if(type=="source"){
    contribDir <- file.path(repopath, "src", "contrib")
  }else{
    rVersion <- getRprefix(getRversion())
    contribDir <- file.path(repopath, "bin", "windows", "contrib", rVersion)
  }
  pkgs_paths <- list.files(file.path(contribDir, "Archive"), full.names=TRUE, recursive=TRUE, pattern="\\.tar\\..*$")
  pkgs_dirs <- list.dirs(file.path(contribDir, "Archive"))[-1]
  pkgs <- purrr::map_chr(strsplit(pkgs_dirs, paste0("Archive", .Platform$file.sep), fixed=TRUE), `[`, 2L)
  names(pkgs_dirs) <- pkgs
  ext <- ifelse(type=="source", "\\.tar\\..*$", "\\.zip$")
  archive <- purrr::map(pkgs_dirs, function(d){
    x <- file.info(list.files(d, full.names=TRUE, pattern=ext))
    rownames(x) <- purrr::map_chr(strsplit(rownames(x), paste0("Archive", .Platform$file.sep), fixed=TRUE), `[`, 2L)
    return(x)
  })
  saveRDS(archive, file=file.path(contribDir, "Meta", "archive.rds"))
  return(invisible())
}

#' @title Add package to a CRAN-like repo
#' @description Add a single package to an existing CRAN-like repo.
#' @param repopath path to the repo
#' @param bundle package bundle (tar.gz)
#' @param binary logical, whether to put the binary source in the repo
#' @param overwrite logical, whether to overwrite if the package bundle already exists
#' @param testbuild logical, whether to test the package can be built
#' @param multiarch logical, whether to test 32bit and 64bit
#' @return Nothing, just prints some messages.
#' @export
#' @seealso \code{\link{writePACKAGESandArchive}} to scan a repo and update the repo.
#' @importFrom devtools build find_rtools
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom withr with_dir
#' @importFrom tools write_PACKAGES
#' @importFrom utils untar tar
addPackage <- function(repopath, bundle, binary=TRUE, oldeRversion=-1L, overwrite=FALSE, testbuild=TRUE, multiarch=TRUE){
  # check repo existence
  multiarch <- switch(multiarch, "TRUE"=NULL, "FALSE"="--no-multiarch")
  path <- normalizePath(repopath, winslash=.Platform$file.sep)
  if(!dir.exists(path)){
    stop(sprintf("Folder %s does not exist."))
  }
  # check repo structure
  subfolders <- subfoldersCRANlike(path)
  contribDir <- subfolders$contribDir
  binPaths <- subfolders$binPaths
  if(!all(union(c(contribDir, subfolders$ArchiveDir, subfolders$MetaDir), unlist(binPaths)) %in% list.dirs(path))){
    stop(sprintf("%s is not a CRAN-like repo.", path))
  }
  #
  if(basename(bundle) %in% list.files(contribDir)){
    if(!overwrite){
      stop(sprintf("Bundle %s exists in the repo (use overwrite=TRUE to replace it).", basename(bundle)))
    }else{
      if(file.exists(file.path(contribDir, basename(bundle)))){
        message(sprintf("Deleting current %s from the repo", basename(bundle)))
        file.remove(file.path(contribDir, basename(bundle)))
      }
    }
  }
  # name
  name <- sans_ext(list.files(path, pattern=".name"))
  # TODO check if no name
  #
  message(sprintf("Repo name found: `%s`.", name))
  # untar package
  tmpDir <- tempdir()
  message(sprintf("Temporary working directory: %s", tmpDir))
  pkg <- untar(bundle, exdir=tmpDir, list=TRUE)
  untar(bundle, exdir=tmpDir) ## ??? SL POURQUOI DEUX FOIS
  locateDESCRIPTION <- which(stringr::str_detect(pkg, "/DESCRIPTION$"))
  if(!length(locateDESCRIPTION)){
    stop(sprintf("No DESCRIPTION file found in %s.", bundle))
  }
  pkg <- stringr::str_split_fixed(pkg[locateDESCRIPTION], .Platform$file.sep, n=2)[1,1]
  pkgDir <- file.path(tmpDir, pkg)
  DESCRIPTIONpath <- file.path(pkgDir, "DESCRIPTION")
  # add Repository field
  DESCRIPTIONlines <- readLines(DESCRIPTIONpath)
  locateRepository <- which(stringr::str_detect(DESCRIPTIONlines, "^Repository:"))
  if(length(locateRepository)){
    currentName <- sub("^Repository: ", "\\1", DESCRIPTIONlines[locateRepository])
    if(currentName != name){
      message("Replacing repository field (this may take a while...)")
      DESCRIPTIONlines[locateRepository] <- sprintf("Repository: %s", name)
      writeLines(DESCRIPTIONlines, DESCRIPTIONpath)
    }
  }else{
    message("Adding repository field (this may take a while...)")
    cat(sprintf("Repository: %s", name), file = DESCRIPTIONpath, append = TRUE, sep = "\n")
  }
  # taring
  pkgTargz <- file.path(tmpDir, basename(bundle))
  withr::with_dir(tmpDir, tar(pkgTargz, files=pkg, compression="gzip"))
  # test build bundle
    # mais là tu n'essayes pas de build le tar.gz.... !
    # faire plutôt install.packages.vanilla dans une lib bidon
    # avec withr::with_temp_libpaths - oui mais inclure ça dans vanilla...
    # bof c'est déjà pas mal... d'ailleurs build teste si c ok je pense
  if(testbuild){
    message("Testing whether the package can be build...")
    suppressWarnings(dir.create(tmpDirBuild <- file.path(tmpDir, "testbuild")))
    ### je peux mettre args="--vanilla" ? non, unknown option, bizarre
    invisible(devtools::find_rtools())
    invisible(withr::with_dir(tmpDir, devtools::build(pkg, path=tmpDirBuild, args=multiarch)))
  }
  # copy to repo
  invisible(file.copy(
    from = pkgTargz,
    to = file.path(contribDir, basename(pkgTargz))
  ))
  # archive package
  archiving <- archivePackages(repopath=path, pkgs=pkg, type="source")
  if(archiving) makeArchiveRDS(repopath=path, type="source")
    # pourquoi ne pas plutot faire makeArchiveRDS dans archivePackages ?
  # write the PACKAGES and PACKAGES.gz files
  message("Write files PACKAGES and PACKAGES.gz to the repo")
  tools::write_PACKAGES(contribDir, type="source", latestOnly=FALSE,
                        addFiles=TRUE)
  # # copy them in the binPaths
  # invisible(lapply(binPaths, function(path) {
  #   file.copy(
  #     file.path(contribDir, "PACKAGES"),
  #     file.path(path, "PACKAGES"),
  #     overwrite = TRUE
  #   )
  #   file.copy(
  #     file.path(contribDir, "PACKAGES.gz"),
  #     file.path(path, "PACKAGES.gz"),
  #     overwrite=TRUE
  #   )
  # }))
  # add binary
  if(binary){
    pkgZip <- file.path(binPaths$win.binary, sub("\\.tar\\..*$", "\\1.zip", basename(pkgTargz)))
    if(file.exists(pkgZip)) file.remove(pkgZip)
    message("Building binary source.")
    invisible(devtools::find_rtools())
    pkgZip <- withr::with_dir(tmpDir, devtools::build(pkg, binary=TRUE, args=multiarch))
    invisible(file.copy(from = pkgZip, to = binPaths$win.binary))
    ### v 1.4.13 : COMMENTED BELOW ##
    # # archive package
    # archiving <- archivePackages(repopath=path, pkgs=pkg, type="binary")
    # if(archiving) makeArchiveRDS(repopath=path, type="binary")
    # write PACKAGES files
    ##
    message("Write files PACKAGES and PACKAGES.gz to the bin repo")
    tools::write_PACKAGES(binPaths$win.binary, type="win.binary",
                          latestOnly=FALSE, verbose=TRUE, addFiles=TRUE)
    #### v 1.4.14 ####
    ## copy to other contrib dirs
    if(!is.null(oldeRversion)){
      # "bin" plutôt que "contrib" meilleur nom
      winBindir <- subfolders$winBindir
      Rversion <- getRversion()
      Rmajor <- Rversion$major
      Rminor <- Rversion$minor
      for(i in (-1L):oldeRversion){
        version <- paste(Rmajor, Rminor+i, sep=".")
        contribdir <- file.path(winBindir, version, sep=.Platform$file.sep)
        if(!dir.exists(contribdir)){
          # TODO message create
          dir.create(contribdir)
        }
        invisible(file.copy(from = pkgZip, to = contribdir))
        ##
        message(sprintf("Write files PACKAGES and PACKAGES.gz to the `%s` bin repo", version))
        tools::write_PACKAGES(contribdir, type="win.binary",
                              latestOnly=FALSE, verbose=TRUE, addFiles=TRUE)
      }
    }
  }
  # done
  message(sprintf("Done: %s added to repo `%s`.", basename(bundle), name))
  message(sprintf("Refer to this repo with:\n %s", sprintf('c(%s = "file:///%s") or c(%s = "file:%s")', name, path, name, path)))
  return(invisible())
}

#' @title Write PACKAGES files and archive in a repo.
#' @description Scan a CRAN-like repo and update it.
#' @param repopath the path to the repo folder
#' @return Invisibly returns two logical values, indicating whether an archiving occurs for source packages and binary packages.
#' @export
#' @seealso \code{\link{addPackage}} to add a single package to a repo.
#' @importFrom tools write_PACKAGES
#' @examples \dontrun{repopath <- "~/myrepo"
#' repopath <- makeRepo(repopath)
#' # put some tarballs in contrib/src ...
#' writePACKAGESandArchive(repopath)}
writePACKAGESandArchive <- function(repopath){
  #repopath <- normalizePath(repopath, winslash = .Platform$file.sep)
  repopath <- path.expand(repopath)
  subfolders <- subfoldersCRANlike(repopath)
  contribDir <- subfolders$contribDir
  binPaths <- subfolders$binPaths
  # check repo structure
  if(!all(union(c(contribDir, subfolders$ArchiveDir, subfolders$MetaDir), unlist(binPaths)) %in% list.dirs(repopath))){
    stop(sprintf("%s is not a CRAN-like repo.", repopath))
  }
  # list all present packages
  pkgs_tarballs <- list.files(contribDir, full.names=FALSE, recursive=FALSE, pattern="\\.tar\\..*$")
  pkgs <- unique(gsub("*_|-?[?0-9\\.]", "", sub("\\.tar\\..*$", "\\1", pkgs_tarballs)))
  # archive
  message("Archiving...")
  archiving <- archivePackages(repopath, pkgs)
  if(archiving){
    message("Write Meta...")
    makeArchiveRDS(repopath)
  }else{
    message("Nothing to archive")
  }
  # write the PACKAGES and PACKAGES.gz files
  message("Write files PACKAGES and PACKAGES.gz to the repo")
  tools::write_PACKAGES(contribDir, type="source", latestOnly=FALSE,
                        verbose=TRUE, addFiles=TRUE)
  # # copy them in the binPaths
  # invisible(lapply(binPaths, function(path) {
  #   file.copy(
  #     file.path(contribDir, "PACKAGES"),
  #     file.path(path, "PACKAGES"),
  #     overwrite = TRUE
  #   )
  #   file.copy(
  #     file.path(contribDir, "PACKAGES.gz"),
  #     file.path(path, "PACKAGES.gz"),
  #     overwrite=TRUE
  #   )
  # }))
  ### v 1.4.13 : COMMENTED BELOW ##
  # list all present packages
  # pkgs_zips <- list.files(binPaths$win.binary, full.names=FALSE, recursive=FALSE, pattern="\\.zip$")
  # pkgs <- unique(gsub("*_|-?[?0-9\\.]", "", sub("\\.zip$", "\\1", pkgs_zips)))
  # # archive
  # archiving_bin <- archivePackages(repopath, pkgs, type="binary")
  # if(archiving_bin) makeArchiveRDS(repopath, type="binary")
  # write the PACKAGES and PACKAGES.gz files
  ### v 1.4.14 ###
  winBindir <- subfolders$winBindir
  Rdirs <- unique(dirname(list.files(winBindir, pattern="\\.zip$", full.names=TRUE, recursive=TRUE)))
  versions <- basename(Rdirs)
  for(i in seq_along(Rdirs)){
    message(sprintf("Write files PACKAGES and PACKAGES.gz to the `%s` bin repo", versions[i]))
    tools::write_PACKAGES(Rdirs[i], latestOnly=FALSE, type="win.binary",
                          verbose=TRUE, addFiles=TRUE)
  }
  #
  #return(invisible(c(archiving, archiving_bin)))
  return(invisible(archiving))
}


