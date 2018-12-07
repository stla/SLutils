context("XLSX Haskell")

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
  XLSXwrite(dat, file = tmpFile)
  dd <- readxl::read_excel(tmpFile)
  dd$COL14 <- as.Date(dd$COL14)
  dd$COL2 <- as.factor(dd$COL2)
  dat$COL3 <- as.character(dat$COL3)
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
  XLSXwrite(dat, file = tmpFile)
  dd <- readxl::read_excel(tmpFile)
  dd$c <- as.logical(dd$c)
  dd$b <- as.factor(dd$b)
  expect_equal(dat, dd, check.attributes=FALSE)
})

test_that("Write XLSX with comments, and get them", {
  dat <- mtcars[1:2,1:2]
  comments <- purrr::map_df(dat, as.character)
  tmpFile <- tempfile(fileext = ".xlsx")
  XLSXwrite(dat, file = tmpFile, comments=comments)
  getComments <- XLSXcomments(tmpFile, "Sheet1")
  expect_equal(comments, getComments)
  expect_warning(XLSXcomments(tmpFile, "X"),
                 sprintf("No sheet or empty sheet `X` in `%s` (Available sheet: Sheet1).",
                         basename(tmpFile)),
                 fixed=TRUE)
})

test_that("Write XLSX - base64", {
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(A=1:2, B=c("a","b"))
  base64 <- XLSXwrite(dat, file = tmpFile, base64=TRUE)
  n <- nchar(base64)
  expect_true(n >= 4198L && n <= 4202L)
  # str <- strsplit(base64, "base64,")[[1]][2]
  # bd <- base64enc::base64decode(str)
  # bd <- jsonlite::base64_dec(str)
})

test_that("Read XLSX", {
  xlsx <- system.file("extdata/datasets.xlsx", package = "readxl")
  # try to read an unavailable sheet:
  expect_warning(XLSXread(xlsx, sheet="X"),
                 sprintf("No sheet or empty sheet `X` in `%s` (Available sheets: chickwts, iris, mtcars, quakes).",
                         basename(xlsx)),
                 fixed=TRUE)
  # read an available sheet:
  expect_silent(dat <- XLSXread(xlsx, sheet="iris"))
  # read all available sheets:
  expect_silent(allsheets <- XLSXread(xlsx))
  expect_equal(names(allsheets), c("chickwts", "iris", "mtcars", "quakes"))
})

test_that("Read XLSX - firstRow and lastRow", {
  xlsx <- system.file("extdata/datasets.xlsx", package="readxl")
  dat0 <- readxl::read_excel(xlsx, sheet=3)
  dat1 <- XLSXread(xlsx, sheet="chickwts", firstRow=1)
  expect_identical(dim(dat0), dim(dat1))
  dat2 <- XLSXread(xlsx, sheet="chickwts", firstRow=2)
  expect_true(nrow(dat2)==nrow(dat0)-1L)
  dat3 <- XLSXread(xlsx, sheet="chickwts", lastRow=10)
  expect_true(nrow(dat3)==9L)
  dat4 <- XLSXread(xlsx, sheet="chickwts", header=FALSE, lastRow=10)
  expect_true(nrow(dat4)==10L)
  dat5 <- XLSXread(xlsx, sheet="chickwts", lastRow=10000L)
  expect_identical(dim(dat0), dim(dat5))
})

test_that("Read XLSX - firstRow and lastRow", {
  xlsx <- system.file("extdata/datasets.xlsx", package = "readxl")
  sheets <- XLSXsheets(xlsx)
  expect_true(all(c("iris", "mtcars", "chickwts", "quakes") %in% sheets))
})


test_that("Read XLSX - JSON output", {
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(A=1:2, B=c("a","b"))
  XLSXwrite(dat, file = tmpFile)
  # valid sheet
  json <- XLSXread(tmpFile, "Sheet1", output="JSON")
  expect_equal(json, as.character(jsonlite::toJSON(dat)))
  # unvalid sheet
  expect_warning(XLSXread(tmpFile, sheet="X", output="JSON"),
                 sprintf("No sheet or empty sheet `X` in `%s` (Available sheet: Sheet1).",
                         basename(tmpFile)),
                 fixed=TRUE)
  })

test_that("Read XLSX with comments", {
  xlsx <- system.file("extdata/xlsx/xlsx_comments.xlsx", package = "SLutils")
  # try to read an unavailable sheet:
  expect_warning(XLSXread(xlsx, sheet="X", comments=TRUE),
                 sprintf("No sheet or empty sheet `X` in `%s` (Available sheets: Sheet1, Sheet2).",
                         basename(xlsx)),
                 fixed=TRUE)
  # read an available sheet:
  expect_silent(dat <- XLSXread(xlsx, sheet="Sheet1", comments=TRUE))
  expect_identical(attributes(dat$data), attributes(dat$comments))
  # read all available sheets:
  expect_silent(allsheets <- XLSXread(xlsx, comments=TRUE))
  expect_equal(names(allsheets), c("Sheet1", "Sheet2"))
  expect_identical(attributes(allsheets$Sheet1$data), attributes(allsheets$Sheet1$comments))
  expect_identical(dim(allsheets$Sheet2$comments), NULL)
})
