#' Aggregate PPMF Data
#'
#' @param ppmf tibble of ppmf data
#' @param group Column in ppmf to group by, typically GEOID
#' @param age Column in ppmf containing 1 for not voting age and 2 for voting age
#' @param race Column in ppmf containing race codes
#' @param hisp Column in ppmf containing 1 for Not Hispanic and 2 for Hispanic
#'
#' @return tibble of ppmf data aggregated by group with race classified with columns:
#' * `group`: named by entry group
#' * `pop`: total population
#' * `pop_hisp`: total population - Hispanic or Latino (of any race)
#' * `pop_white`: total population - White alone, not Hispanic or Latino
#' * `pop_black`: total population - Black or African American alone, not Hispanic or Latino
#' * `pop_aian`: total population - American Indian and Alaska Native alone, not Hispanic or Latino
#' * `pop_asian`: total population - Asian alone, not Hispanic or Latino
#' * `pop_nhpi`: total population - Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino
#' * `pop_other`: total population - Some Other Race alone, not Hispanic or Latino
#' * `pop_two`: total population - Population of two or more races, not Hispanic or Latino
#' * `vap`: voting age population
#' * `vap_hisp`: voting age population - Hispanic or Latino (of any race)
#' * `vap_white`: voting age population - White alone, not Hispanic or Latino
#' * `vap_black`: voting age population - Black or African American alone, not Hispanic or Latino
#' * `vap_aian`: voting age population - American Indian and Alaska Native alone, not Hispanic or Latino
#' * `vap_asian`: voting age population - Asian alone, not Hispanic or Latino
#' * `vap_nhpi`: voting age population - Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino
#' * `vap_other`: voting age population - Some Other Race alone, not Hispanic or Latino
#' * `vap_two`: voting age population - Population of two or more races, not Hispanic or Latino
#'
#'
#' @export
#'
#' @importFrom dplyr .data select full_join count group_by filter rowwise starts_with ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'
#' @md
#'
#' @examples
#' data(ppmf_ex)
#' ppmf_ex <- ppmf_ex %>% add_geoid()
#' blocks <- agg(ppmf_ex)
#'
agg <- function(ppmf, group = GEOID, age = VOTING_AGE, race = CENRACE, hisp = CENHISP){
  ppmf <- ppmf %>% select({{group}}, {{age}}, {{race}}, {{hisp}})

  ppmf <-  ppmf %>%
    replace_race(race = !!enquo(race)) %>%
    overwrite_hisp_race(race = !!enquo(race), hisp = !!enquo(hisp))

  ppmf <- ppmf %>% select(-{{hisp}})



  ppmf_vap <- ppmf %>% filter({{age}} == 2) %>% select(-{{age}}) %>%
    group_by({{group}}) %>% count({{race}}) %>%
    pivot_wider(id_cols = {{group}}, names_from = {{race}},
                values_from = .data$n, values_fill = 0, names_prefix = 'vap_')


  ppmf_nvap <- ppmf %>% filter({{age}} == 1) %>% select(-{{age}}) %>%
    group_by({{group}}) %>% count({{race}}) %>%
    pivot_wider(id_cols = {{group}}, names_from = {{race}},
                values_from = .data$n, values_fill = 0, names_prefix = 'nvap_')

  # ensure all needed columns present
  exp_cols <- c('black', 'white', 'hisp', 'two', 'other', 'aian', 'nhpi', 'asian')
  exp_vap <- paste0('vap_', exp_cols)
  exp_nvap <- paste0('nvap_', exp_cols)

  add_vap <- rep(0, length(exp_vap))
  names(add_vap) <- exp_vap
  ppmf_vap <- ppmf_vap %>% add_column(!!!add_vap[setdiff(names(add_vap),
                                                         names(ppmf_vap))])
  add_nvap <- rep(0, length(exp_nvap))
  names(add_nvap) <- exp_nvap
  ppmf_nvap <- ppmf_nvap %>% add_column(!!!add_nvap[setdiff(names(add_nvap),
                                                           names(ppmf_nvap))])

  # join

  ret <- ppmf_vap %>% full_join(ppmf_nvap, by = 'GEOID')

  # get the correct objects
  ret[is.na(ret)] <- 0
  ret <- ret %>% ungroup()
  ret <- ret %>% mutate(nvap = rowSums(select(., starts_with('nvap')), na.rm = TRUE),
                                      vap = rowSums(select(., starts_with('vap')), na.rm = TRUE))

  ret <- ret %>% mutate(pop = .data$nvap + .data$vap,
                        pop_white = .data$vap_white + .data$nvap_white,
                        pop_hisp = .data$vap_hisp + .data$nvap_hisp,
                        pop_black = .data$vap_black + .data$nvap_black,
                        pop_aian = .data$vap_aian + .data$nvap_aian,
                        pop_asian = .data$vap_asian + .data$nvap_asian,
                        pop_nhpi = .data$vap_nhpi + .data$nvap_nhpi,
                        pop_other = .data$vap_other + .data$nvap_other,
                        pop_two = .data$vap_two + .data$nvap_two)
  # reorder and return
  ret %>% select(.data$GEOID,
                 .data$pop, .data$pop_white, .data$pop_hisp,
                 .data$pop_black, .data$pop_aian, .data$pop_asian,
                 .data$pop_nhpi, .data$pop_other, .data$pop_two,
                 .data$vap, .data$vap_white, .data$vap_hisp,
                 .data$vap_black, .data$vap_aian, .data$vap_asian,
                 .data$vap_nhpi, .data$vap_other, .data$vap_two)

}

utils::globalVariables(c('GEOID','VOTING_AGE', '.'))
