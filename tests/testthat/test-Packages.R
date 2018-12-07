context("Packages utilities")

test_that("CRAN-like structure", {
  x <- subfoldersCRANlike(tempdir())
  expect_identical(names(x), c("winBindir", "contribDir", "ArchiveDir", "MetaDir", "binPaths"))
  expect_identical(names(x$binPaths), c("win.binary", "mac.binary", "mac.binary.mavericks", "mac.binary.leopard", "Archive", "Meta"))
  expect_length(x$contribDir, 1L)
})

test_that("Create CRAN-like repo", {
  unlink(tempdir(), recursive=TRUE)
  expect_message(makeRepo(tempdir(), name="repo"), "Creating new folder", "Creating repo structure", "Repo `repo` created in")
})

test_that("Install package vanilla mode", {
  # must set the repo
  # ça va pas ça installe dans lib !
  #x <- withr::with_temp_libpaths(install.packages.vanilla2(pkgs="abind", repos=c(CRAN="https://cran.rstudio.com/"), quiet=TRUE))
  #expect_true("package 'abind' successfully unpacked and MD5 sums checked" %in% x)
})
