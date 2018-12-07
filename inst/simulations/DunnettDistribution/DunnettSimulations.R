library(magrittr)
# variance-covariance matrix :
rho <- 0.7
Sigma <- matrix(rbind(c(1,rho),
                      c(rho,1)),
                nrow=2)
# mean
Mu <- c(0,0)
# degrees of freedom
df <- 4
# number of simulations
nsims <- 150000
# sampling function
sampling <- function(pkg){
  switch(pkg,
         "mnormt" = mnormt::rmt(nsims, mean=Mu, S=Sigma, df=df),
         "mvtnorm" = mvtnorm::rmvt(nsims, sigma=Sigma, df=df) +
           matrix(rep(Mu,nsims), ncol=2, byrow=TRUE),
         "LaplacesDemon" = LaplacesDemon::rmvt(nsims, mu=Mu, S=Sigma, df=df),
         "mixAK" = mixAK::rMVT(nsims, df=df, mu=Mu, Sigma=Sigma)$x
  )
}

# sampling
pkgs <- c("mnormt", "mvtnorm", "LaplacesDemon", "mixAK")
sims <- vapply(pkgs,
               FUN=sampling, FUN.VALUE=matrix(0, nrow=nsims, ncol=2))

# dataframe for ggplot2
dat <- setNames(cbind(rep(pkgs,each=nsims),
                      data.frame(do.call(rbind, plyr::alply(sims, 3)))),
                c("pkg", "x", "y"))
# plot
library(ggplot2)
ggplot(dat, aes(x=x, y=y)) +
  geom_point() +
  facet_grid(.~pkg)

# datatable
library(data.table)
DT <- as.data.table(dat)
setkey(DT, "pkg")
d <- 2

DT[, mean(`x`<d & `y`<d), by="pkg"]
SLutils::pDunnett(d, df=df, k=2, rho=rho, tail="two") %>% as.numeric

DT[, mean(abs(`x`)<d & abs(`y`)<d), by="pkg"]
SLutils::pDunnett(d, df=df, k=2, rho=rho, tail="one") %>% as.numeric

