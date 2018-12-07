context("XLSX JS")

check_os <- function() {
  if (.Platform$OS.type != "windows") {
    skip("Windows only")
  }
}

test_that("Excel columns", {
  excelCols <- purrr::map_chr(c(1,2,27,52,53,703,704), excelIndex)
  expect_identical(excelCols, c("A","B","AA","AZ","BA","AAA","AAB"))
})

test_that("Write XLSX - test 1", {
  check_os()
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(COL1 = 1:2,
                    COL2 = factor(c("a", "b")),
                    COL3 = as.POSIXct(strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC"), tz="UTC"),
                    COL4 = c(3.24, 1.5),
                    COL5 = c(TRUE, FALSE),
                    COL6 = c(pi+100, 1.5),
                    COL7 = c(9876543211.4, 102192.38),
                    COL8 = c("AA", "BB"),
                    COL9 = c(0.00000313030227123, 0.000013),
                    COL10 = c(1.5e-12, 1.321e-11),
                    COL11 = c(12341.144325, 321445.66666), # je n'aime pas le résultat
                    COL12 = c(0.002317, 0.0009913),
                    COL13 = c(3.5, NA),
                    COL14 = c(Sys.Date(), Sys.Date()),
                    stringsAsFactors = FALSE
  )
  XLSXwrite0(dat, iris[1:100,], file = tmpFile, author="John Doe", title="Hello")
  dd <- readxl::read_excel(tmpFile)
  dd$COL14 <- as.Date(dd$COL14)
  dd$COL2 <- as.factor(dd$COL2)
  expect_equal(dat[,-5], dd[,-5], check.attributes=FALSE)
})

test_that("Write XLSX - test 2", {
  check_os()
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- structure(
    list(a = c(1L, 2L, 3L, NA, 5L),
         b = structure(1:5, .Label = c("A", "B", "C", "D", "E"), class = "factor"),
         c = c(TRUE, TRUE, TRUE, TRUE, TRUE)),
    .Names = c("a", "b", "c"),
    class = "data.frame",
    row.names = c("1", "2", "3", "4", "5"))
  XLSXwrite0(dat, file = tmpFile, author="John Doe", title="Hello")
  dd <- readxl::read_excel(tmpFile)
  dd$c <- as.logical(dd$c)
  dd$b <- as.factor(dd$b)
  expect_equal(dat, dd, check.attributes=FALSE)
})

test_that("XLSX with comments (does not work due to js-xlsx)", {
  check_os()
  dat <- mtcars[1:2,1:2]
  attr(dat, "comments") <- purrr::map_df(dat, as.character)
  tmpFile <- tempfile(fileext = ".xlsx")
  XLSXwrite0(dat, file = tmpFile)
})
