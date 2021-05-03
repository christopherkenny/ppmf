
#' Add Standard GEOID to PPMF Data
#'
#' Adds the GEOID identifier common to spatial census data sets, such as those loaded
#' by tigris. This allows for easier merging or aggregation by a single variable.
#'
#' @param ppmf tibble of ppmf data
#' @param state Column in ppmf with state (fips) ID. Default is \code{TABBLKST}.
#' @param county Column in ppmf with county (fips) ID. Default is \code{TABBLKCOU}.
#' @param tract Column in ppmf with tract ID. Default is \code{TABBLKTRACT}.
#' @param block_group Column in ppmf with block group ID. Default is \code{TABBLKGRP}
#' @param block Column in ppmf with block ID. Default is \code{TABBLK}.
#' @param level Geographic level to write the GEOID for. Options are block (default),
#' block_group, tract, and county.
#'
#' @return input data ppmf with added column GEOID
#' @export
#'
#' @importFrom rlang enquo eval_tidy
#' @importFrom dplyr mutate
#'
#' @examples
#' data(ppmf_ex)
#' ppmf_ex <- ppmf_ex %>% add_geoid()
#'
add_geoid <- function(ppmf, state = TABBLKST, county = TABBLKCOU,
                      tract = TABTRACT, block_group = TABBLKGRP,
                      block = TABBLK, level = 'block'){

  if(missing(ppmf)){
    stop('ppmf argument missing in add_geoid.')
  }

  match.arg(level, choices = c('block', 'block_group', 'tract', 'county'))

  if( level == 'block' ){
    block_group <- NULL
  } else if ( level == 'block_group' ) {
    block <- NULL
  } else if ( level == 'tract') {
    block <- NULL
    block_group <- NULL
  } else {
    block <- NULL
    block_group <- NULL
    tract <- NULL
  }

  ppmf %>% mutate(GEOID = paste0({{state}}, {{county}}, {{tract}}, {{block_group}},
                                 {{block}}))

}

utils::globalVariables(c('TABBLKST', 'TABBLKCOU', 'TABTRACT',
                         'TABBLKGRP', 'TABBLK'))
