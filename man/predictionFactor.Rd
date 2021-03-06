% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExactPredictionInterval.R
\name{predictionFactor}
\alias{predictionFactor}
\title{Dunnett's K factor}
\usage{
predictionFactor(n, df = n - 1, n.mean = 1, k = 1,
  method = c("Bonferroni", "exact"), type = "two-sided",
  conf.level = 0.95)
}
\arguments{
\item{n}{sample size}

\item{df}{degrees of freedom}

\item{n.mean}{number of averages}

\item{k}{number of future observations}

\item{method}{method}

\item{type}{interval type}

\item{conf.level}{confidence level}
}
\value{
The factor K used by \code{\link{predictionIntervals}}.
}
\description{
Dunnett's K factor for exact prediction intervals.
}
\details{
See the vignette.
}
\examples{
predictionFactor_SL(10, k=3, method="exact")
}
