context("hreadXLSX")

test_that("error messages", {
  # wrong file
  expect_error(hreadXLSX("zarma.xlsx"),
                 "File `zarma.xlsx` not found.",
                 fixed=TRUE)
  # wrong sheet
  xlsxFile <<- system.file("extdata", "xlsx", "xlsx_small.xlsx", package="SLutils")
  expect_error(hreadXLSX(xlsxFile, sheet="zarma"),
                 sprintf("No sheet or empty sheet `zarma` in `%s` (Available sheets: Sheet1, Sheet2).",
                         basename(xlsxFile)),
                 fixed=TRUE)
  # wrong `what` argument
  expect_error(hreadXLSX(xlsxFile, sheet="Sheet1", what="zarma"),
               "Invalid argument `what`. See `?hreadXLSX`.",
               fixed=TRUE)
})

test_that("read as JSON", {
  # with headers
  json <- hreadXLSX(xlsxFile, sheet="Sheet1", output="JSON")
  expect_equal(json, "{\"Col1\":[1,2],\"Col2\":[\"a\",\"b\"],\"Col3\":[\"2017-02-18\",\"2017-02-19\"]}")
  # without headers
})

test_that("read as flatlist", {
  # - with headers
  dat1 <- hreadXLSX(xlsxFile, sheet="Sheet1")
  expect_equal(names(dat1), c("Col1","Col2","Col3"))
  expect_equal(unlist(dat1$Col1), 1:2)
  expect_equal(unlist(dat1$Col2), c("a","b"))
  expect_equal(unlist(dat1$Col3), c("2017-02-18", "2017-02-19"))
  # - without headers
  dat2 <<- hreadXLSX(xlsxFile, sheet="Sheet1", header=FALSE)
  expect_equal(names(dat2), c("X2","X3","X4"))
})

test_that("read as dataframe", {
  # - with headers
  dat3 <- hreadXLSX(xlsxFile, sheet="Sheet1", output="dataframe")
  expect_true(is.data.frame(dat3))
  expect_equal(names(dat3), c("Col1","Col2","Col3"))
  expect_equal(dat3$Col1, 1:2)
  expect_true(is.factor(dat3$Col2))
  expect_equal(as.character(dat3$Col2), c("a","b"))
  expect_equal(as.character(dat3$Col3), c("2017-02-18", "2017-02-19"))
  # - without headers
  dat4 <- hreadXLSX(xlsxFile, sheet="Sheet1", header=FALSE, output="dataframe")
  expect_equal(names(dat4), c("X2","X3","X4"))
  expect_identical(dat4, as.data.frame(lapply(dat2, unlist)))
})

test_that("read as list", {
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", output="list")
  expect_true(is.list(dat))
})

test_that("read comments", {
  # sheet with comments
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_comments.xlsx", package="SLutils")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="comments",
                   output="dataframe", stringsAsFactors=FALSE)
  expect_identical(dat,
                   data.frame(Col1 = c("a comment", NA_character_, NA_character_),
                              Col2 = c(NA_character_, NA_character_, "another comment"),
                              stringsAsFactors=FALSE))
  # sheet without comments
  dat <- hreadXLSX(xlsxFile, sheet="Sheet2", what="comments", output="dataframe")
  expect_true(all(is.na(dat)))
})

test_that("read types", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_comments.xlsx", package="SLutils")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="types", output="list")
  expect_identical(dat$Col1, rep("number", 3L))
  expect_identical(dat$Col2, rep("date", 3L))
})

test_that("read formats", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_comments.xlsx", package="SLutils")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="formats", output="list")
  expect_true(all(is.na(dat$Col1)))
  expect_identical(dat$Col2, rep("NfMmDdYy", 3L))
})

test_that("read data and types", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_mixed.xlsx", package="SLutils")
  # as json
  json <- hreadXLSX(xlsxFile, sheet="Sheet1", what="data, types", output="JSON")
  # as flatlist
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="data, types")
  expect_equal(names(dat), c("data", "types"))
  expect_identical(dat, setNames(lapply(c("data","types"), function(what){
    hreadXLSX(xlsxFile, sheet="Sheet1", what=what)
  }), names(dat)))
  # as list
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="data, types", output="list")
  expect_equal(names(dat), c("data", "types"))
  expect_identical(dat, setNames(lapply(c("data","types"), function(what){
    hreadXLSX(xlsxFile, sheet="Sheet1", what=what, output="list")
  }), names(dat)))
  # as dataframe
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what="data, types", output="dataframe")
  expect_equal(names(dat), c("data", "types"))
  expect_identical(dat, setNames(lapply(c("data","types"), function(what){
    hreadXLSX(xlsxFile, sheet="Sheet1", what=what, output="dataframe")
  }), names(dat)))
})

test_that("duplicated headers", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_doubleheaders.xlsx", package="SLutils")
  # as list
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", what=" data,  types  ", output="list")
  expect_equal(names(dat$data), c("A_1","A_2","B_1","C","B_2","A_3"))
})

test_that("first row and last row", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_tenrows.xlsx", package="SLutils")
  # complete
  dat0 <- hreadXLSX(xlsxFile, sheet="Sheet1", output="JSON")
  expect_equal(dat0, "{\"B\":[1,2,3,4,5,6,7,8,9]}")
  # first row
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=FALSE,
                   firstRow=2, output="JSON")
  expect_equal(dat, "{\"X2\":[1,2,3,4,5,6,7,8,9]}")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=TRUE,
                   firstRow=10, output="JSON")
  expect_equal(dat, "{\"9\":[]}")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=FALSE,
                   firstRow=10, output="JSON")
  expect_equal(dat, "{\"X2\":[9]}")
  # TODO there's a crash when firstrow > 10
  # last row
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=TRUE,
                   lastRow=5, output="JSON")
  expect_equal(dat, "{\"B\":[1,2,3,4]}")
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=TRUE,
                   lastRow=15, output="JSON")
  expect_equal(dat, dat0)
  # first row and last row
  dat <- hreadXLSX(xlsxFile, sheet="Sheet1", header=FALSE,
                   firstRow=2, lastRow=5, output="JSON")
  expect_equal(dat, "{\"X2\":[1,2,3,4]}")
})
