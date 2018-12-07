setwd("~/MyPackages/SLutils/inst/simulations/ExactPredictionInterval")
library(SLutils)
library(data.table)

conf <- c(80,95)/100
k <- c(2, 3, 4, 5)
n <- c(5, 10)

# f1 <- function(){
#   DT <- data.table(conf)
#   DT <- DT[, list(n), by=names(DT)]
#   DT <- DT[, list(k), by=names(DT)]
# }
# f2 <- function(){
#   DT <- data.table(expand.grid(conf=conf, n=n, k=k))
# }
# f3 <- function(){
#   DT <- CJ(conf=conf, n=n, k=k)
# }
# library(microbenchmark)
# microbenchmark(
#   chaining = f1(),
#   expand = f2(),
#   CJ = f3(), # the best !
#   times=100
# )

dat <- CJ(conf=conf, n=n, k=k) # = expand.grid
key(dat)
# dat <- cbind(dat,
#       dat[, .(K=predictionFactor(n=n, df=n-1, k=k, method="exact", conf.level=conf)),
#           keyby=.(id=rownames(dat))][,id:=NULL])
dat <- dat[, .(K=predictionFactor(n=`n`, k=`k`, type="lower", method="exact", conf.level=`conf`)),
           by=.(conf,n,k)]
dim(dat)

#
nsims <- 3
sims1 <- round(matrix(rnorm(nsims*max(n)), nrow=nsims), 1)

# # nouveau DT
# DTsims <- dat[, .(sims=list(sims1[,1:`n`])), by=.(conf,n,k)]
# DTsims$sims

# ou merge :
dat[, `:=`(sims=list(sims1[,1:`n`])), by=.(n)] # by n
#dat$sims : colonne de matrices
dim(dat) # 16, 5

# mean and sd
dat <- dat[, .(K=K,
        mean = rowMeans(sims[[1]]),
        sd = apply(sims[[1]], 1, sd)),
    by=.(conf,n,k)] # by utile ??? oui
dim(dat) # 48, 6

# prediction bounds

dat <- dat[, .(lwr=`mean`-K*`sd`,
               upr=`mean`+K*`sd`,
               index=1:.N),
           by=.(conf,n,k)]
dim(dat) # 48, 6
setkeyv(dat, c(key(dat), "index"))

# new sims
sims2 <- round(matrix(rnorm(nsims*max(k)), nrow=nsims), 1)

# tests
dat[, `:=`(test=all(sims2[`index`, 1:`k`]>lwr)),
    by=.(index,conf,n,k)]
dim(dat) # 48, 7

# coverage
dat[, .(coverage=mean(test)),
    by=.(conf,n,k)]



