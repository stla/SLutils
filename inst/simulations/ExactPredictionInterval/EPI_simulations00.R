setwd("~/MyPackages/SLutils/inst/simulations/ExactPredictionInterval")
library(SLutils)

conf <- c(80,95)/100
k <- c(2, 3)
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

dat <- CJ(conf=conf, n=n, k=k)
setkey(dat)
# dat <- cbind(dat,
#       dat[, .(K=predictionFactor(n=n, df=n-1, k=k, method="exact", conf.level=conf)),
#           keyby=.(id=rownames(dat))][,id:=NULL])
dat <- dat[, .(K=predictionFactor_SL(n=`n`, k=`k`, method="exact", conf.level=`conf`)),
           keyby=.(conf,n,k)]
dim(dat)

#
nsims <- 150000
sims1 <- round(matrix(rnorm(nsims*max(n)), nrow=nsims), 1)

# # nouveau DT
# DTsims <- dat[, .(sims=list(sims1[,1:`n`])), by=.(conf,n,k)]
# DTsims$sims

# ou merge :
dat[, `:=`(sims=list(sims1[,1:`n`])), by=.(conf,n,k)]
#dat$sims : colonne de matrices
dim(dat)

# mean and sd
dat <- dat[, .(K=K,
        mean =apply(sims[[1]], 1, mean),
        sd = apply(sims[[1]], 1, sd)),
    by=.(conf,n,k)]
dim(dat)

# prediction bounds
dat <- dat[, .(lwr=`mean`-K*`sd`, upr=`mean`+K*`sd`, index=1:.N), by=.(conf,n,k)]
dim(dat)

# new sims
sims2 <- round(matrix(rnorm(nsims*max(k)), nrow=nsims), 1)

# tests
dat[, `:=`(test=all(sims2[index, 1:`k`]<upr & sims2[index, 1:`k`]>lwr)),
    by=.(index,conf,n,k)]
dim(dat)

# coverage
dat[, .(coverage=mean(test)),
    by=.(conf,n,k)]










# # t'as besoin que de mean et sd !!
# dat[, .(lwr=apply(sims[[1]], 1,
#                   function(sample){
#                     mean(sample)-K*sd(sample)
#                   }),
#         upr=apply(sims[[1]], 1,
#                   function(sample){
#                     mean(sample)+K*sd(sample)
#                   })),
#     by=.(conf,n,k)]
#
