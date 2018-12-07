context("tidyxl")

test_that("getExcelSheet", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_small.xlsx", package="SLutils")
  sheet <- getExcelSheet(xlsxFile)
  expect_equal(key(sheet), c("col", "row"))
  row <- as.list(sheet[.(2,4), .(data_type, numeric, comment)])
  expect_identical(row, list(data_type="numeric", numeric=2, comment="a comment"))
})

test_that("sheetToJSON", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_small.xlsx", package="SLutils")
  sheet <- getExcelSheet(xlsxFile)
  json <- sheetToJSON(sheet)
  expect_equal(as.character(json$data),
               "{\"Col1\":[1,2],\"Col2\":[\"a\",\"b\"],\"Col3\":[\"2017-02-18\",\"2017-02-19\"]}",
               check.attributes = FALSE)
  expect_equal(as.character(json$comments), "{\"Col1\":[null,\"a comment\"],\"Col2\":[\"another comment\",null],\"Col3\":[null,null]}")
  expect_equal(as.character(json$types), "{\"Col1\":[\"numeric\",\"numeric\"],\"Col2\":[\"character\",\"character\"],\"Col3\":[\"date\",\"date\"]}")
  expect_equal(as.character(sheetToJSON(sheet, header=FALSE)$data),
               "{\"X2\":[\"Col1\",1,2],\"X3\":[\"Col2\",\"a\",\"b\"],\"X4\":[\"Col3\",\"2017-02-18\",\"2017-02-19\"]}")
  expect_equal(as.character(sheetToJSON(sheet, header=FALSE, firstRow=3)$data),
               "{\"X2\":[1,2],\"X3\":[\"a\",\"b\"],\"X4\":[\"2017-02-18\",\"2017-02-19\"]}")
})

test_that("sheetToJSON", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_mixed.xlsx", package="SLutils")
  sheet <- getExcelSheet(xlsxFile)
  jsons <- sheetToJSON(sheet)
  expect_identical(treadXLSX(sheet, output="JSON"), jsons)
  x <- treadXLSX(sheet, output="both")
  expect_equal(names(x), c("json", "R"))
  expect_identical(x$json, jsons)
  expect_true(is.list(x$R))
  expect_equal(names(x$R), c("data","comments","types"))
  expect_equal(class(x$R$data), "list")
  expect_equal(class(x$R$comments), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(x$R$types), c("tbl_df", "tbl", "data.frame"))
  expect_equal(unlist(x$R$data$A), c(NA, 2))
  expect_equal(unlist(x$R$data$B), c("3", "x"))
})
