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
#' @importFrom dplyr .data rename select left_join
#' @importFrom rlang as_name
#'
#' @examples
#' data(ppmf_ex)
#' ppmf_ex %>% replace_race()
replace_race <- function(ppmf, race = CENRACE){
  races <- get('races') %>% select(-.data$desc)

  #races <- races %>% rename(race = .data$code)
  races[[as_name(enquo(race))]] <- races$code

  if(as_name(enquo(race)) != 'code'){
    races <- races %>% select(-.data$code)
  }

  races <- races %>% rename(group_race_name = .data$group)

  ppmf <- ppmf %>% left_join(races, by = as_name(enquo(race)))

  ppmf$group_race_name[is.na(ppmf$group_race_name)] <- 'hisp'

  ppmf[[as_name(enquo(race))]] <- ppmf$group_race_name
  ppmf[['group_race_name']] <- NULL


  return(ppmf)
}


#' Overwrite Races with Hispanic
#'
#' @param ppmf tibble of ppmf data
#' @param race Column in ppmf containing race codes
#' @param hisp Column in ppmf containing 1 for Not Hispanic and 2 for Hispanic
#'
#' @return tibble with race column entries replaced if the individual is Hispanic
#' @export
#'
#' @examples
#' data(ppmf_ex)
#' ppmf_ex %>% replace_race() %>% overwrite_hisp_race()
overwrite_hisp_race <- function(ppmf, race = CENRACE, hisp = CENHISP){

  ppmf[[as_name(enquo(race))]][ppmf[[as_name(enquo(hisp))]] == 2 ] <- 'hisp'

  return(ppmf)

}

utils::globalVariables(c('CENRACE', 'CENHISP'))
