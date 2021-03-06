% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{findFiles}
\alias{findFiles}
\title{Find files before/after a given data with size lower/bigger than a given size}
\usage{
findFiles(path = ".", date = Sys.Date(), before = TRUE, size = 0,
  bigger = TRUE, ...)
}
\arguments{
\item{path}{the path to search in}

\item{date}{the reference date}

\item{before}{logical, whether to search files before the reference date}

\item{size}{reference size}

\item{bigger}{logical, whether to search file bigger than the reference size}

\item{...}{arguments passed to \code{\link[base]{list.files}}}
}
\value{
The found files, in a vector.
}
\description{
Find files before/after a given data with size lower/bigger than a given size.
}
