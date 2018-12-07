setwd("~/MyPackages/SLutils/inst/simulations/ExactPredictionInterval")
library(SLutils)
library(data.table)

nsims <- 100
type <- "lower" # two-sided"
conf <- c(80,95)/100
k <- c(2, 3, 4, 5)
n <- c(5, 10)

sims1 <- round(matrix(rnorm(nsims*max(n)), nrow=nsims), 1)
sims2 <- round(matrix(rnorm(nsims*max(k)), nrow=nsims), 1)

dat <- CJ(conf=conf, n=n, k=k)
dat <- dat[, .(K=predictionFactor(n=`n`, k=`k`, type=type, method="exact", conf.level=`conf`)),
           by=.(conf,n,k)]
dat[, `:=`(sims=list(sims1[,1:`n`])), by=.(n)]
dat <- dat[, .(K=`K`,
        mean = rowMeans(sims[[1]]),
        sd = apply(sims[[1]], 1, sd)),
    by=.(conf,n,k)]
dat <- dat[, .(lwr=`mean`-K*`sd`,
               upr=`mean`+K*`sd`,
               index=1:.N),
           by=.(conf,n,k)]
setkeyv(dat, c(key(dat), "index"))
dat[, `:=`(test_lwr=all(sims2[`index`, 1:`k`]>lwr),
           test_upr=all(sims2[`index`, 1:`k`]<upr)),
    by=.(index,conf,n,k)]
if(type=="two-sided"){
  dat <- dat[, .(conf=`conf`,
                 n=`n`,
                 k=`k`,
                 test=`test_lwr`&`test_upr`)]
  dat[, .(coverage=mean(test)),
      by=.(conf,n,k)]
}else{
  dat[, .(coverage_lwr=mean(test_lwr),
          coverage_upr=mean(test_upr)),
      by=.(conf,n,k)]
}


