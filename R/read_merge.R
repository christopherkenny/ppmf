#' Read PPMF data and Merge with Census 2010 Data
#'
#' @param state state abbreviation
#' @param level geography level. One of 'block', 'block group', 'tract', 'county'
#' @param versions character vector of ppmf versions. Currently '19', '12', and/or '4'
#' @param prefixes prefixes to give pop and vap columns in output. Default is `paste0('v', versions, '_')`
#' @param paths paths to PPMF data. Default is `Sys.getenv(paste0('ppmf', versions))`
#'
#' @return sf tibble of PPMF merged with Census 2010 data
#' @export
#'
#' @concept getdata
#' @md
#' @examples
#' \dontrun{
#' # Requires Census Bureau API
#' de_bg <- read_merge_ppmf('DE', 'block group')
#' }
read_merge_ppmf <- function(state, level, versions = c('19'),
                            prefixes = paste0('v', versions, '_'),
                            paths = Sys.getenv(paste0('ppmf', versions))) {

  # check inputs ----
  state <- censable::match_abb(state)
  stopifnot(length(state) == 1)
  match.arg(level, choices = c('block', 'block group', 'tract', 'county'))
  match.arg(versions, choices = c('19', '12', '4'), several.ok = TRUE)

  if (length(versions) != length(prefixes)) {
    stop('`versions` and `prefixes` must have equal lengths.')
  }

  lapply(seq_along(paths), function(x){
    if (!file.exists(paths[x])) {
      stop(paste0('File ', paths[x], 'does not exist. Did you download it with `download_ppmf`?'))
    }
  })

  # read census data ----
  cen <- censable::build_dec(geography = level, state = state, year = 2010)

  # read ppmf data ----
  ppmfs <- lapply(seq_along(versions), function(x) {
    read_ppmf(state = state, path = paths[x]) |>
    add_geoid(level = level) |>
    agg() |>
    dplyr::rename_with(.fn = function(i){paste0(prefixes[x], i)},
                       dplyr::starts_with(c('pop', 'vap')))
    })

  for (i in seq_along(ppmfs)) {
    cen <- cen |>
      dplyr::left_join(ppmfs[[i]], by = 'GEOID')
  }

  cen[is.na(cen)] <- 0

  cen
}
