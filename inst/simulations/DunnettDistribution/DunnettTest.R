library(magrittr)
library(SLutils)

nsims <- 100000
n <- 6
k <- 3

xsims <- matrix(rnorm(nsims*n), nrow=nsims)
z <- sqrt(1+1/n)
tsims <- t(apply(xsims, 1, function(s) (rnorm(k) - mean(s))/(z*sd(s))))

colMeans(tsims)
cor(tsims) # ce n'est pas censé correspondre à rho peut-être...

( rho <- 1/((n/1) + 1) )

dsims <- apply(tsims, 1, max)

curve(ecdf(dsims)(x), from=-1.5, to=2.5, ylim=c(0,1))
xx <- seq(-1.5, 2.5, length.out = 50)
yy <- purrr::map_dbl(xx, ~ pDunnett(.x, df=n-1, k=k, rho=rho, tailed="two"))
points(xx, yy, col="red", pch=19)








