context("Dates")

test_that("is_date", {
  x <- Sys.Date()
  expect_true(is_date(x))
  x <- strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC")
  expect_false(is_date(x))
  x <- as.POSIXct(x, tz="UTC")
  expect_true(is_date(x))
})

test_that("possibly_date", {
  x <- Sys.Date()
  expect_true(possibly_date(x))
  x <- strftime(c("2016-06-21", "2016-11-22"), usetz=TRUE, tz="UTC")
  expect_true(possibly_date(x))
  x <- as.POSIXct(x, tz="UTC")
  expect_true(possibly_date(x))
  x <- Sys.time()
  expect_true(possibly_date(x))
  x <- "2016/11/11"
  expect_true(possibly_date(x))
  x <- "a"
  expect_false(possibly_date(x))
  x <- "11nov1980"
  expect_false(possibly_date(x))
})
