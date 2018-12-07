## ----packages, include=FALSE---------------------------------------------
library(SLutils)
breakpoints <- SLutils::breakpoints
library(magrittr)
library(ggplot2)
library(car)
library(jsonlite)
library(microbenchmark)

## ------------------------------------------------------------------------
numextract("I am born in 1975 and I am 41 years old.")

## ------------------------------------------------------------------------
numextract(c("3.5 ml", "7 ml"))

## ------------------------------------------------------------------------
numextract(factor(c("Batch 101", "Batch 217")))

## ------------------------------------------------------------------------
numsextract("I am born in 1975 and I am 41 years old.")

## ---- error=TRUE---------------------------------------------------------
Numerize(" -3.5")
Numerize("Batch 101")

## ---- error=TRUE---------------------------------------------------------
Numerize(c("-3.5", "42"))
Numerize(c("-3.5", "fourty-two"))

## ------------------------------------------------------------------------
Numerize(factor(c("00", "01")))

## ------------------------------------------------------------------------
dat <- data.frame(u = 1:4,
                  v = c(2.5, -4, 0, 10),
                  w = c("a","b","c","d"),
                  x = factor(c("100", "100", "101", "110")),
                  y = factor(c("Test A", "Test A", "Test B", "Test C")),
                  z = c("1", "1.4", "-3", "   0"),
                  stringsAsFactors = FALSE)
str(dat)

## ------------------------------------------------------------------------
Numerize.df(dat) %>% str

## ------------------------------------------------------------------------
Numerize.df(dat, factors=FALSE) %>% str

