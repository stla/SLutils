% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breakpoints.R
\name{breakpoints}
\alias{breakpoints}
\title{Find breakpoints}
\usage{
breakpoints(formula, dat, onebreak = TRUE, nbreaks.max = NULL, ...)
}
\arguments{
\item{formula}{model formula}

\item{dat}{dataframe}

\item{onebreak}{logical, whether to find only one breakpoint}

\item{nbreaks.max}{\code{NULL} for the optimal number of breakpoints, otherwise an integer for the maximal number of breakpoints}

\item{...}{arguments passed to \code{\link[strucchange]{Fstats}}}
}
\value{
A dataframe listing the breakpoints.
}
\description{
Find breakpoints for a Gaussian linear model.
}
\examples{
data(dataBP)
( bp <- breakpoints(y~1, dat=dataBP, onebreak=FALSE, nbreaks.max=NULL) )
paste("Breakpoints:", paste(dataBP$date[bp$breakpoint], collapse=", "))
}
