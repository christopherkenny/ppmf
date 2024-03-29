% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_ppmf}
\alias{read_ppmf}
\title{Read in PPMF Data}
\usage{
read_ppmf(state, path, ...)
}
\arguments{
\item{state}{two letter state (+ DC + PR) abbreviation or two digit state fips code}

\item{path}{where the data is saved to}

\item{...}{additional arguments passed on to \code{\link[readr:read_delim]{readr::read_csv()}}}
}
\value{
tibble of ppmf data
}
\description{
This reads in PPMF data from a file. Use \code{download_ppmf()} if you do
not have a local copy of the ppmf data.
}
\examples{
\dontrun{
# Takes a few minutes and requires read access to files
temp <- tempdir()
path <- download_ppmf('ppmf_12.csv', dir = temp)
# If you already have it downloaded, point to it with path:
ppmf <- read_ppmf('AL', path)
}
}
\concept{getdata}
