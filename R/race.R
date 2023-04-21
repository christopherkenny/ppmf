#' Replace Race Categories
#'
#' Replaces the Census's numeric categories for race with less specific racial
#' classifications, typically useful for redistricting purposes.
#'
#' @param ppmf tibble of ppmf data
#' @param race Column in ppmf containing race codes
#'
#' @return tibble with race column replaced by simpler racial classifications
#' @export
#'
#' @concept basic
#' @examples
#' data(ppmf_ex)
#' ppmf_ex |> replace_race()
replace_race <- function(ppmf, race = CENRACE){
  races <- get('races') |> dplyr::select(-.data$desc)

  #races <- races |> dplyr::rename(race = .data$code)
  races[[rlang::as_name(rlang::enquo(race))]] <- races$code

  if(rlang::as_name(rlang::enquo(race)) != 'code'){
    races <- races |> dplyr::select(-.data$code)
  }

  races <- races |> dplyr::rename(group_race_name = .data$group)

  ppmf <- ppmf |> dplyr::left_join(races, by = rlang::as_name(rlang::enquo(race)))

  ppmf$group_race_name[is.na(ppmf$group_race_name)] <- 'hisp'

  ppmf[[rlang::as_name(rlang::enquo(race))]] <- ppmf$group_race_name
  ppmf[['group_race_name']] <- NULL

  ppmf
}


#' Overwrite Races with Hispanic
#'
#' @param ppmf tibble of ppmf data
#' @param race Column in ppmf containing race codes
#' @param hisp Column in ppmf containing 1 for Not Hispanic and 2 for Hispanic
#'
#' @return tibble with race column entries replaced if the individual is Hispanic
#' @export
#' @concept basic
#' @examples
#' data(ppmf_ex)
#' ppmf_ex |> replace_race() |> overwrite_hisp_race()
overwrite_hisp_race <- function(ppmf, race = CENRACE, hisp = CENHISP){

  ppmf[[rlang::as_name(rlang::enquo(race))]][ppmf[[rlang::as_name(rlang::enquo(hisp))]] == 2 ] <- 'hisp'

  ppmf
}

utils::globalVariables(c('CENRACE', 'CENHISP'))
