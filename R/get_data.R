#' Read in PPMF Data
#'
#' This reads in PPMF data from a file. Use `download_ppmf()` if you do
#' not have a local copy of the ppmf data.
#'
#' @param state two letter state (+ DC + PR) abbreviation or two digit state fips code
#' @param path where the data is saved to
#'
#' @return tibble of ppmf data
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom stringr str_pad
#'
#' @examples
#' \dontrun{
#' # Takes a few minutes and requires read access to files
#' temp <- tempdir()
#' path <- download_ppmf('ppmf_12.csv', dir = temp)
#' # If you already have it downloaded, point to it with path:
#' ppmf <- read_ppmf('AL', path)
#' }
read_ppmf <- function(state, path){
  states <- get('states')

  if(is.numeric(state)){
    state <- str_pad(state, 2, 'left', '0')
  }
  which_state <- which(state == states$state)
  if(length(which_state) == 0){
    which_state <- which(state == states$state_code)
  }
  if(length(which_state) == 0){
    stop('state does not match a state abbreviation or state fips.\n
         Check `data(states)` columns state or state_code.')
  }


  if(!file.exists(path)){
    stop('`path` does not point to a file.')
  }


  cols_nom <- c('VINTAGE', 'TABBLKST', 'TABBLKCOU',
                'TABTRACT', 'TABBLKGRP', 'TABBLK',
                'RTYPE', 'GQTYPE_PL', 'VOTING_AGE',
                'CENHISP', 'CENRACE')

  read_csv(path,
           skip = states$skip[which_state],
           n_max = states$n_max[which_state],
           col_types = 'ccccccccccc',
           col_names = cols_nom)



}

#' Download PPMF Files
#'
#' Current reads from Chris's OneDrive for zip files!
#'
#' @param dsn (data save name) string to unzip the data to
#' @param dir the folder or directory to save the file in
#' @param version string in '12' or '4' signifying the 12-2 or 4-5 versions respectively
#' @param overwrite If a file is found at path/dsn, should it be overwritten?
#' Defaults to FALSE.
#'
#' @return a string path to where the
#' @export
#'
#' @importFrom utils unzip download.file
#'
#' @examples
#' \dontrun{
#' # Takes a few minutes and requires read access to files
#' temp <- tempdir()
#' path <- download_ppmf('ppmf_12', dir = temp)
#' }
download_ppmf <- function(dsn, dir = '', version = '12', overwrite = FALSE){
  match.arg(version, choices = c('12', '4'))

  if(!missing(dsn)){
    if(stringr::str_sub(dsn, -4, -1) == '.csv'){
      if(dir == ''){
        path <- dsn
      } else {
        path <- paste0(dir, '/', dsn, '.csv')
      }
    } else {
      if(dir == ''){
        path <- paste0(dsn, '.csv')
      } else {
        path <- paste0(dir, '/', dsn, '.csv')
      }
    }
  } else { # dsn is missing
    dsn <- paste0('ppmf_', version, '.csv')
    if(dir == ''){
      path <- dsn
    } else {
      path <- paste0(dir, '/', dsn, '.csv')
    }
  }

  if(file.exists(path) & !overwrite){
    cat('File already exists at the given path. To overwrite, set `overwrite = TRUE`.')
    return(path)
  }

  # If there is no file or we need to overwrite it, download zip:
  temp <- tempfile(fileext = '.zip')

  zip_path <- ifelse(version == '12',
                     'https://hu-my.sharepoint.com/:u:/g/personal/christopherkenny_fas_harvard_edu/Eev1S2CeszpFoxLcXTyQS_ABQtCP512MbVkqgi31eZJmog?download=1',
                     'https://hu-my.sharepoint.com/:u:/g/personal/christopherkenny_fas_harvard_edu/EakoCYWsIDBOl22gjmxDtEcBbOdIJDecU4OyPgKzxez6iA?download=1')

  download.file(zip_path, temp)

  # then create the desired file:
  unzip(zipfile = temp, exdir = path)

  # Might be a problem?
  cat('If you received a warning about unzipping, you *may* need to manually unzip the file.\n')
  cat('file at: ', temp, '\n')

  # and return where we put it:
  return(path)

}
