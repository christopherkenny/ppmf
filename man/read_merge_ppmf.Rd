% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_merge.R
\name{read_merge_ppmf}
\alias{read_merge_ppmf}
\title{Read PPMF data and Merge with Census 2010 Data}
\usage{
read_merge_ppmf(
  state,
  level,
  versions = c("19"),
  prefixes = paste0("v", versions, "_"),
  paths = Sys.getenv(paste0("ppmf", versions))
)
}
\arguments{
\item{state}{state abbreviation}

\item{level}{geography level. One of 'block', 'block group', 'tract', 'county'}

\item{versions}{character vector of ppmf versions. Currently '19', '12', and/or '4'}

\item{prefixes}{prefixes to give pop and vap columns in output. Default is \code{paste0('v', versions, '_')}}

\item{paths}{paths to PPMF data. Default is \code{Sys.getenv(paste0('ppmf', versions))}}
}
\value{
sf tibble of PPMF merged with Census 2010 data
}
\description{
Read PPMF data and Merge with Census 2010 Data
}
\examples{
\dontrun{
# Requires Census Bureau API
de_bg <- read_merge_ppmf('DE', 'block group')
}
}
\concept{getdata}
