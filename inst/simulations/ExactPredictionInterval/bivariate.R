x <- function(m,y1,y2,rho){
  (y1-rho*y2)^2/((y1-rho*y2)^2 + (1-rho^2)*(m+y2^2))
}

h <- function(y1,y2,nu,rho){
  y2/4/sqrt(nu*pi) *
    sum(purrr::map_dbl(1:(nu/2),
                       ~ gamma(.x-0.5)/gamma(.x) *
                         (1+y2^2/nu)^(0.5-.x) *
          (1+sign(y1-rho*y2)*gsl::beta_inc(1/2,.x-1/2,x(nu,y1,y2,rho)))))
}

P <- function(y1,y2,nu,rho){
  1/2/pi*atan(sqrt(1-rho^2)/-rho) +
    + h(y1,y2,nu,rho) + h(y2,y1,nu,rho)
}

y1 <- 1; y2 <- 0.5; nu <- 8; rho <- 0.05
P(y1,y2,nu,rho) + 0.5

mnormt::pmt(c(y1,y2), df=nu, S=matrix(c(1,rho,rho,1),ncol=2))


# variance-covariance matrix :
Sigma <- matrix(rbind(c(1,rho),
                      c(rho,1)),
                nrow=2)
# mean
Mu <- c(0,0)
# degrees of freedom
df <- nu
# number of simulations
nsims <- 400000
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
dat <- as.data.table(setNames(cbind(rep(pkgs,each=nsims),
                      data.frame(do.call(rbind, plyr::alply(sims, 3)))),
                c("pkg", "x", "y")))

setkey(dat, "pkg")

dat[, `:=`(test=`x`<y1 & `y`<y2), keyby=.(pkg)]

dat[, list(p=mean(test)), keyby=.(pkg)]

