context("Numerization")

test_that("isNumerizable function", {
  expect_true(isNumerizable(c("1.5", "  3.2 ")))
  x <- isNumerizable(c("1", "b", "c"))
  expect_false(x)
  expect_identical(attr(x, "unnumerizable"), c(b=2L, c=3L))
  expect_true(isNumerizable(factor(c("3.5", "4"))))
  expect_false(isNumerizable(factor(c("a", "b"))))
})

test_that("isNumerizable.df function", {
  dat <- data.frame(x=1:4,
                    y=c("a","b","c","d"),
                    z=c("1", "1.4", "-3", "   0"))
  expect_identical(isNumerizable.df(dat), c(x=TRUE, y=FALSE, z=TRUE))
})

test_that("coltypes function", {
  dat <- data.frame(x=1:4,
                    y=c("a","b","c","d"),
                    z=c("1", "1.4", "-3", "   0"))
  x <- coltypes(dat)
  expect_identical(x$column, colnames(dat))
  expect_identical(x$class, c("integer", "factor", "factor"))
  expect_identical(x$numerizable, c(TRUE, FALSE, TRUE))
})

test_that("Numerize function", {
  expect_identical(Numerize(c("1", "1.4", "-3", "   0")), c(1, 1.4, -3, 0))
  expect_error(Numerize(c("a","b","c","d")), 'not a numerizable vector')
  expect_identical(Numerize(factor(c("3.5", "4"))), c(3.5, 4))
})

test_that("Numerize.df function", {
  dat <- data.frame(x=1:4,
                    y=c("a","b","c","d"),
                    z=c("1", "1.4", "-3", "   0"))
  expect_warning(Numerize.df(dat))
  numdat <- suppressWarnings(Numerize.df(dat))
  expect_identical(numdat$x, dat$x)
  expect_identical(numdat$y, dat$y)
  expect_identical(numdat$z, c(1, 1.4, -3, 0))
  expect_identical(attr(numdat, "unnumerizable"), "y")
  expect_identical(Numerize.df(mtcars), mtcars)
  expect_warning(Numerize.df(iris))
  numiris <- suppressWarnings(Numerize.df(iris))
  attr(numiris, "unnumerizable") <- NULL
  expect_identical(numiris, iris)
  expect_identical(Numerize.df(airquality), airquality)
})
