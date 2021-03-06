% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting.R
\name{enumerate}
\alias{enumerate}
\title{Sentence enumerating the elements of a vector}
\usage{
enumerate(x)
}
\arguments{
\item{x}{character or numeric vector}
}
\value{
A character string.
}
\description{
Make a sentence enumerating the elements of a vector.
}
\examples{
enumerate(letters[1])
enumerate(letters[1:2])
enumerate(letters[1:3])
}
