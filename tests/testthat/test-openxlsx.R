context("openxlsx")

check_os <- function() {
  if (.Platform$OS.type != "windows") {
    skip("Windows only")
  }
}

test_that("Write XLSX - test 1", {
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(COL1 = 1:2,
                    COL2 = factor(c("a", "b")),
                    COL3 = as.POSIXct(strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC"), tz="UTC"),
                    COL4 = c(3.24, 1.5),
                    COL5 = c(TRUE, FALSE),
                    COL6 = c(pi+100, 1.5),
                    COL7 = c(9876543211.4, 102192.38),
                    COL8 = c("AAé", "BBµ"),
                    COL9 = c(0.00000313030227123, 0.000013),
                    COL10 = c(1.5e-12, 1.321e-11),
                    COL11 = c(12341.144325, 321445.66666),
                    COL12 = c(0.002317, 0.0009913),
                    COL13 = c(3.5, NA),
                    COL14 = c(Sys.Date(), Sys.Date()),
                    stringsAsFactors = FALSE
  )
  json <- jsonlite::fromJSON(jsonlite::toJSON(dat, dataframe="columns", digits=14, na="null"), simplifyVector=FALSE)
  writeXLSX(json, file = tmpFile)
  dd <- readxl::read_excel(tmpFile)
  dd$COL14 <- as.Date(dd$COL14)
  dd$COL2 <- as.factor(dd$COL2)
  #dat$COL3 <- as.character(dat$COL3)
  dd$COL5 <- as.logical(dd$COL5)
  expect_equal(dat, dd, check.attributes=FALSE)
})

test_that("Write XLSX - test 2", {
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- structure(
    list(a = c(1L, 2L, 3L, NA, 5L),
         b = structure(1:5, .Label = c("A", "B", "C", "D", "E"), class = "factor"),
         c = c(TRUE, TRUE, TRUE, TRUE, TRUE)),
    .Names = c("a", "b", "c"),
    class = "data.frame",
    row.names = c("1", "2", "3", "4", "5"))
  json <- jsonlite::fromJSON(jsonlite::toJSON(dat, dataframe="columns", digits=14, na="null"), simplifyVector=FALSE)
  writeXLSX(json, file = tmpFile)
  dd <- readxl::read_excel(tmpFile)
  dd$c <- as.logical(dd$c)
  dd$b <- as.factor(dd$b)
  expect_equal(dat, dd, check.attributes=FALSE)
})

test_that("Write XLSX with comments, and get them", {
  dat <- mtcars[1:2,1:2]
  rownames(dat) <- NULL
  comments <- purrr::map_df(dat, as.character)
  tmpFile <- tempfile(fileext = ".xlsx")
  json <- jsonlite::fromJSON(jsonlite::toJSON(dat, dataframe="columns", digits=14, na="null"), simplifyVector=FALSE)
  jsoncomments <- jsonlite::fromJSON(jsonlite::toJSON(comments, dataframe="columns", na="null"), simplifyVector=FALSE)
  writeXLSX(json, file = tmpFile, comments=jsoncomments)
  getComments <- XLSXcomments(tmpFile, "Sheet1")
  expect_equal(comments, getComments)
})

test_that("Write XLSX - base64", {
  check_os()
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(A=1:2, B=c("a","b"))
  json <- jsonlite::fromJSON(jsonlite::toJSON(dat, dataframe="columns", digits=14, na="null"), simplifyVector=FALSE)
  base64 <- writeXLSX(json, file = tmpFile, base64=TRUE)
  #expect_equal(nchar(base64), 14606L) # or 14610 ?? is it due to the filename ?
  nchars <- nchar(base64)
  expect_true(nchars>14605L && nchars<14611L)
})
