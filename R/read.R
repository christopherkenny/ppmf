#' Read in PPMF Data
#'
#' This reads in PPMF data from a file. Use `download_ppmf()` if you do
#' not have a local copy of the ppmf data.
#'
#' @param state two letter state (+ DC + PR) abbreviation or two digit state fips code
#' @param path where the data is saved to
#' @param ... additional arguments passed on to [readr::read_csv()]
#'
#' @return tibble of ppmf data
#' @export
#'
#' @concept getdata
#'
#' @examples
#' \dontrun{
#' # Takes a few minutes and requires read access to files
#' temp <- tempdir()
#' path <- download_ppmf('ppmf_12.csv', dir = temp)
#' # If you already have it downloaded, point to it with path:
#' ppmf <- read_ppmf('AL', path)
#' }
read_ppmf <- function(state, path, ...){
  states <- ppmf::states

  if (!is.null(state)) {
    if(is.numeric(state)){
      state <- stringr::str_pad(state, 2, 'left', '0')
    }
    which_state <- which(state == states$state)
    if(length(which_state) == 0){
      which_state <- which(state == states$state_code)
    }
    if(length(which_state) == 0){
      stop('state does not match a state abbreviation or state fips.\n
         Check `data(states)` columns state or state_code.')
    }
  }

  if(!file.exists(path)){
    stop('`path` does not point to a file.')
  }

  cols_nom <- readr::read_lines(file = path, n_max = 1) |>
    stringr::str_split(pattern = ',', simplify = TRUE)

  skip_s <- ifelse(is.null(state), 1, states$skip[which_state])
  n_max_s <- ifelse(is.null(state), 312471327, states$n_max[which_state])

  readr::read_csv(path,
                  skip = skip_s,
                  n_max = n_max_s,
                  col_types = readr::cols(.default = readr::col_character()),
                  col_names = cols_nom, ...)

}
