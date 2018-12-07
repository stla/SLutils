library(nCDunnett)

p <- .95
df <- 4
k <- 2
rho <- 0.15

( q <- SLutils:::qDunnett(p, df=df, k=k, rho=rho, type="lower") )
#SLutils:::pDunnett(q, df=df, k=k, rho=rho)
nCDunnett::qNCDun(p=p, nu=df, rho=rho,
                  delta=rep(0,times=k), two.sided=F)



SLutils:::qDunnett(p, df=df, k=k, rho=rho, type="two-sided")
nCDunnett::qNCDun(p=p, nu=df, rho=rho,
                  delta=rep(0,times=k), two.sided=T)

# also https://github.com/cran/multxpert

library(DunnettTests)
# use mvtnorm::pmvt

qNCDun(0.95, 24, rep.int(0.5,3), rep.int(0,3), two.sided=TRUE) # should be 2.51 (from table)
SLutils:::qDunnett(0.95, df=24, k=3, rho=0.5)

qNCDun(0.95, 30, rep.int(0.5,3), rep.int(0,3), two.sided=TRUE) # should be 2.47
SLutils:::qDunnett(0.95, df=30, k=3, rho=0.5)



qDunnett <- function (p, df, k, rho,
                      type = c("two-sided", "one-sided"))
{
  type <- match.arg(type)
  alpha <- 1 - p
  if (type == "two-sided") {
    alpha <- alpha/2
  }
  S <- matrix(rho, nrow=k, ncol=k) + (1-rho)*diag(k)
  if (type == "two-sided") {
    f <- function(d, df, k, S, p) {
      mnormt::sadmvt(df=df, lower=rep(-d,k), upper=rep(d,k),
                      mean=rep(0,k), S=S, maxpts=2000*k) - p
    }
  }
  else {
    f <- function(d, df, k, S, p) {
      mnormt::pmt(d, S=S, df=df) - p
    }
  }
  d <- uniroot(f,
               df = df, k = k, S = S, p=p,
               lower=qt(1 - alpha, df),
               upper=qt(1 - alpha/k, df),
               tol=.Machine$double.eps, maxiter=5000)$root
  return(d)
}

p <- 0.95; df <- 24; rho <- 0.5; k <- 3
nCDunnett::qNCDun(p=p, nu=df, rho=rho,
                  delta=rep(0,times=k), two.sided=T)
qDunnett(p, df, k, rho)

