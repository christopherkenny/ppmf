
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
#' @concept geoid
#' @examples
#' data(ppmf_ex)
#' ppmf_ex <- ppmf_ex |> add_geoid()
#'
add_geoid <- function(ppmf, state = TABBLKST, county = TABBLKCOU,
                      tract = TABTRACT, block_group = TABBLKGRP,
                      block = TABBLK, level = 'block'){

  if(missing(ppmf)){
    stop('ppmf argument missing in `add_geoid()`.')
  }

  match.arg(level, choices = c('block', 'block group', 'tract', 'county', 'state'))

  if( level == 'block' ){
    block_group <- NULL
  } else if ( level == 'block group' ) {
    block <- NULL
  } else if ( level == 'tract') {
    block <- NULL
    block_group <- NULL
  } else if (level == 'county') {
    block <- NULL
    block_group <- NULL
    tract <- NULL
  } else {
    block <- NULL
    block_group <- NULL
    tract <- NULL
    county <- NULL
  }

  ppmf |>
    dplyr::mutate(GEOID = paste0({{state}}, {{county}}, {{tract}}, {{block_group}},
                                 {{block}}))

}

#' Breakdown GEOID into Components
#'
#' @param ppmf tibble of ppmf data
#' @param GEOID Column in ppmf with GEOID. Default is \code{GEOID}.
#'
#' @return tibble. ppmf with columns added for state, county, tract, block group, and/or block
#' @export
#'
#' @concept geoid
#' @examples
#' data(ppmf_ex)
#' ppmf_ex <- ppmf_ex |> add_geoid()
#' ppmf_ex <- ppmf_ex |> censable::breakdown_geoid()
breakdown_geoid <- function(ppmf, GEOID = GEOID){
  if(missing(ppmf)){
    stop('ppmf argument missing in add_geoid.')
  }

  geoid_col <- rlang::eval_tidy(rlang::enquo(GEOID), ppmf)

  if(is.null(geoid_col[1])){
    stop('`GEOID` is not a column in ppmf.')
  }

  if(stringr::str_length(geoid_col[1]) < 2){
    stop('GEOID does not have a recognizable pattern.')
  } else {
    len <- stringr::str_length(geoid_col[1])
  }

  ppmf <- ppmf |> dplyr::mutate(state = stringr::str_sub({{GEOID}}, 1, 2))

  if(len >= 5){
    ppmf <- ppmf |> dplyr::mutate(county = stringr::str_sub({{GEOID}}, 3, 5))
  }

  if(len >= 11){
    ppmf <- ppmf |> dplyr::mutate(tract = stringr::str_sub({{GEOID}}, 6, 11))
  }

  if(len >= 12){
    ppmf <- ppmf |> dplyr::mutate(block_group = stringr::str_sub({{GEOID}},12,12))
  }
  if(len >= 15){
    ppmf <- ppmf |> dplyr::mutate(block = stringr::str_sub({{GEOID}}, 12, 15))
  }

  ppmf
}

utils::globalVariables(c('TABBLKST', 'TABBLKCOU', 'TABTRACT',
                         'TABBLKGRP', 'TABBLK', 'GEOID'))
