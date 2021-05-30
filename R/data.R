
#' Example PPMF Data
#'
#' @name ppmf_ex
#'
#' @description
#' Includes Perry County, Alabama PPMF data from the April 28, 2021 PPMF data release.
#' This is a subset taken from the 12-2 P data.
#'
#' As each observation is a person, this does not cover every block in the county and
#' due to DAS, not every block with population appears in this data.
#'
#' @return tibble with sample ppmf data
#'
#' @usage
#' data('ppmf_ex')
#'
#' @examples
#' data('ppmf_ex')
#' 
#' @concept data
NULL

#' State Rows
#'
#' @name states
#'
#' @description
#' This data includes the 52 geographies (50 states plus D.C. and P.R.). Within the
#' 2010 PPMF, skip and n_max indicate the relevant rows for a geography.
#'
#' @return tibble with sample ppmf data
#'
#' @usage
#' data('states')
#'
#' @examples
#' data('states')
#' 
#' @concept data
NULL

#' Race Classifications
#' @name races
#'
#' @description
#' This data includes the basic race classifications used for redistricting to
#' get to an easier to work with set of values. This does not include `hisp` grouping
#' which is controlled separately by race within the census
#'
#' @return tibble with three columns
#'  - code: the two digit code used to code races
#'  - desc: the description of the races
#'  - group: the summary group used
#'
#' @usage
#' data('races')
#'
#' @examples
#' data('races')
#'
#'
#' @concept data
#' @md
NULL
