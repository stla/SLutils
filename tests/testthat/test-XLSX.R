context("Coherence XLSX readers/writer")

test_that("Compare writeXLSX, treadXLSX and hreadXLSX", {
  tmpFile <- tempfile(fileext = ".xlsx")
  dat <- data.frame(A = 1:2,
                    B = c("x","y"))
  json <- jsonlite::fromJSON(jsonlite::toJSON(dat, dataframe="columns", digits=14, na="null"),
                             simplifyVector=FALSE)
  # write
  writeXLSX(json, file=tmpFile)
  # tread
  sheet <- getExcelSheet(tmpFile)
  tdat <- treadXLSX(sheet, output="list")
  # hread
  hdat <- hreadXLSX(tmpFile, output="flatlist")
  # compare
  expect_equal(tdat$data$A, hdat$A)
  expect_equal(tdat$data$B, hdat$B)
  expect_true(is.integer(unlist(hdat$A)))
  expect_false(is.integer(unlist(tdat$A)))
  # tread JSON
  tjson <- sheetToJSON(sheet)
  # hread JSON
  hjson_data <- hreadXLSX(tmpFile, output="JSON")
  hjson_types <- hreadXLSX(tmpFile, output="JSON", what="types")
  hjson_comments <- hreadXLSX(tmpFile, output="JSON", what="comments")
  # compare
  expect_equal(class(tjson$data), "json")
  expect_equal(as.character(tjson$data), hjson_data)
  expect_equal(tjson$comments, "null")
})

test_that("Compare comments", {
  xlsxFile <- system.file("extdata", "xlsx", "xlsx_comments.xlsx", package="SLutils")
  # tread
  sheet <- getExcelSheet(xlsxFile)
  tdat <- treadXLSX(sheet, output="list")
  # hread
  hdat <- hreadXLSX(xlsxFile, what="comments", output="flatlist")
  # compare
  expect_equal(na.omit(tdat$comments$Col1), unlist(hdat$Col1), check.attributes=FALSE)
  expect_equal(na.omit(tdat$comments$Col2), unlist(hdat$Col2), check.attributes=FALSE)
  # tread JSON
  tjson <- sheetToJSON(sheet)
  # hread JSON
  hjson_comments <- hreadXLSX(xlsxFile, output="JSON", what="comments")
  # compare
  expect_equal(class(tjson$comments), "json")
  expect_equal(as.character(tjson$comments), hjson_comments)
})
