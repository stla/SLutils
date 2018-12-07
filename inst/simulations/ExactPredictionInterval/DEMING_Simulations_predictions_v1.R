
source('~/Work/R/Deming/myDeming_v0.R')

library(data.table)

### predict observed ynew-xnew given xnew0 (theoretical xnew)

level <- 95/100
M <- 10
n <- 100
x0 <- seq(0, M, length=n)
Xnew0 <- c(M/10, M/2, 9*M/10)
Intercept <- c(-1, 0, 1)
Slope <- c(0.5, 1, 2)
Lambda <- c(0.5, 1, 2)
Sigma2tot <- c(0.005, 0.01, 0.02)


DT <- data.table(Intercept)
DT <- DT[, list(Slope), by=names(DT)]
DT <- DT[, list(Lambda), by=names(DT)]
DT <- DT[, list(Sigma2tot), by=names(DT)]
DT <- DT[, list(Xnew0), by=names(DT)]
DT <- DT[, list(interval=c("lower","upper", "twosided")), by=names(DT)]
setkey(DT) 
DT[, coverage:=numeric(.N)]
nsims <- 50
zx <- matrix(rnorm(n*nsims), nrow=nsims)
zy <- matrix(rnorm(n*nsims), nrow=nsims)

for(intercept in Intercept){
  for(slope in Slope){
    for(lambda in Lambda){
      for(sigma2tot in Sigma2tot){
        sigmax <- sqrt(sigma2tot/(1+lambda))
        sigmay <- sqrt(lambda*sigma2tot/(1+lambda))
        for(xnew0 in Xnew0){
          tests <-  sapply(setNames(1:3, c("lower", "upper", "twosided")), function(i) logical(nsims))
          for(sim in 1:nsims){
            x <- x0 + sigmax*zx[sim, ]  
            y <- intercept+slope*x0 + sigmay*zy[sim,]  
            pred <- deming.predict(x, y, lambda, xnew0, level=level)["diff",c("lower","upper")]
            xnew <- rnorm(1, xnew0, sigmax)  
            ynew <- rnorm(1, intercept+slope*xnew0, sigmay)
            diff <- ynew-xnew
            tests[sim, "lower"] <- diff > pred[1] 
            tests[sim, "upper"] <- diff < pred[2]
          }
          tests[, "twosided"] <- tests[, "lower"] & tests[, "upper"] 
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "lower")]$coverage <- mean(tests[,"lower"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "upper")]$coverage <- mean(tests[,"upper"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "twosided")]$coverage <- mean(tests[,"twosided"])
        }
      }
    }
  }
}

library(ggplot2)

ggplot(DT[Sigma2tot==Sigma2tot[1] & Xnew0==Xnew0[1]], aes(x=Lambda, y=coverage, color=interval)) + geom_line() +
  facet_grid(Intercept~Slope,  labeller=label_both)

dd <- DT[Sigma2tot==Sigma2tot[1] & Xnew0==Xnew0[1]]
which(dd$coverage < 0.1)



#### predict observed ynew given xnew0 (theoretical xnew) ####

level <- 95/100
M <- 10
n <- 100
x0 <- seq(0, M, length=n)
Xnew0 <- c(M/10, M/2, 9*M/10)
Intercept <- c(-1, 0, 1)
Slope <- c(0.5, 1, 2)
Lambda <- c(0.5, 1, 2)
Sigma2tot <- c(0.005, 0.01, 0.02)

DT <- data.table(Intercept)
DT <- DT[, list(Slope), by=names(DT)]
DT <- DT[, list(Lambda), by=names(DT)]
DT <- DT[, list(Sigma2tot), by=names(DT)]
DT <- DT[, list(Xnew0), by=names(DT)]
DT <- DT[, list(interval=c("lower","upper", "twosided")), by=names(DT)]
setkey(DT) 
DT[, coverage:=numeric(.N)]
nsims <- 500
zx <- matrix(rnorm(n*nsims), nrow=nsims)
zy <- matrix(rnorm(n*nsims), nrow=nsims)

for(intercept in Intercept){
  for(slope in Slope){
    for(lambda in Lambda){
      for(sigma2tot in Sigma2tot){
        sigmax <- sqrt(sigma2tot/(1+lambda))
        sigmay <- sqrt(lambda*sigma2tot/(1+lambda))
        for(xnew0 in Xnew0){
          tests <-  sapply(setNames(1:3, c("lower", "upper", "twosided")), function(i) logical(nsims))
          for(sim in 1:nsims){
            x <- x0 + sigmax*zx[sim, ]  
            y <- intercept+slope*x0 + sigmay*zy[sim,]
            ynew <- rnorm(1, intercept+slope*xnew0, sigmay)
            pred <- deming.predict(x, y, lambda, xnew0, level=level)["pred1",c("lower","upper")]
            tests[sim, "lower"] <- ynew > pred[1] 
            tests[sim, "upper"] <- ynew < pred[2]
          }
          tests[, "twosided"] <- tests[, "lower"] & tests[, "upper"] 
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "lower")]$coverage <- mean(tests[,"lower"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "upper")]$coverage <- mean(tests[,"upper"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "twosided")]$coverage <- mean(tests[,"twosided"])
        }
      }
    }
  }
}

library(ggplot2)

ggplot(DT[Sigma2tot==Sigma2tot[1] & Xnew0==Xnew0[1]], aes(x=Lambda, y=coverage, color=interval)) + geom_line() +
  facet_grid(Intercept~Slope,  labeller=label_both)


#### predict observed ynew given observed xnew ####

level <- 95/100
M <- 10
n <- 100
x0 <- seq(0, M, length=n)
Xnew0 <- c(M/10, M/2, 9*M/10)
Intercept <- c(-1, 0, 1)
Slope <- c(0.5, 1, 2)
Lambda <- c(0.5, 1, 2)
Sigma2tot <- c(0.005, 0.01, 0.02)

DT <- data.table(Intercept)
DT <- DT[, list(Slope), by=names(DT)]
DT <- DT[, list(Lambda), by=names(DT)]
DT <- DT[, list(Sigma2tot), by=names(DT)]
DT <- DT[, list(Xnew0), by=names(DT)]
DT <- DT[, list(interval=c("lower","upper", "twosided")), by=names(DT)]
setkey(DT) 
DT[, coverage:=numeric(.N)]
nsims <- 500
zx <- matrix(rnorm(n*nsims), nrow=nsims)
zy <- matrix(rnorm(n*nsims), nrow=nsims)

for(intercept in Intercept){
  for(slope in Slope){
    for(lambda in Lambda){
      for(sigma2tot in Sigma2tot){
        sigmax <- sqrt(sigma2tot/(1+lambda))
        sigmay <- sqrt(lambda*sigma2tot/(1+lambda))
        for(xnew0 in Xnew0){
          tests <-  sapply(setNames(1:3, c("lower", "upper", "twosided")), function(i) logical(nsims))
          for(sim in 1:nsims){
            x <- x0 + sigmax*zx[sim, ]  
            y <- intercept+slope*x0 + sigmay*zy[sim,]
            xnew <- rnorm(1, xnew0, sigmax)  
            ynew <- rnorm(1, intercept+slope*xnew0, sigmay)
            pred <- deming.predict(x, y, lambda, xnew, level=level)["pred2",c("lower","upper")]
            tests[sim, "lower"] <- ynew > pred[1] 
            tests[sim, "upper"] <- ynew < pred[2]
          }
          tests[, "twosided"] <- tests[, "lower"] & tests[, "upper"] 
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "lower")]$coverage <- mean(tests[,"lower"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "upper")]$coverage <- mean(tests[,"upper"])
          DT[list(intercept, slope, lambda, sigma2tot, xnew0, "twosided")]$coverage <- mean(tests[,"twosided"])
        }
      }
    }
  }
}

library(ggplot2)

## archung ce code est pas correct ! - mettre Sigma2tot==0.05, etc
ggplot(DT[Sigma2tot==Sigma2tot[1] & Xnew0==Xnew0[1]], aes(x=Lambda, y=coverage, color=interval)) + geom_line() +
  facet_grid(Intercept~Slope,  labeller=label_both)


