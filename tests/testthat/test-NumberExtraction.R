context("Extraction of numbers")

test_that("numextract - positive integers", {
  expect_identical(numextract("a36"), "36")
  expect_identical(numextract(" 12 "), "12")
  expect_identical(numextract(c("a0", "1b", "c2c")), c("0", "1", "2"))
})

test_that("numextract - negative integers", {
  expect_identical(numextract("a-36"), "-36")
  expect_identical(numextract("a-36", allow.negative=FALSE), "36")
  expect_identical(numextract(" -12 "), "-12")
  expect_false(numextract("-a36")=="-36")
})

test_that("numextract - decimal numbers", {
  expect_identical(numextract(c("30.5ml", " 37.6ml")), c("30.5", "37.6"))
})

test_that("numextract - no number to extract", {
  expect_true(is.na(numextract("hello")))
})

test_that("numsextract function", {
  expect_identical(numsextract("between 2ml and 3.5ml"), c("2", "3.5"))
  expect_identical(numsextract("between -4.1 and -2"), c("-4.1", "-2"))
  expect_identical(numsextract("between -4.1 and -2", allow.negative=FALSE), c("4.1", "2"))
  expect_identical(numsextract("hello"), character(0))
})

