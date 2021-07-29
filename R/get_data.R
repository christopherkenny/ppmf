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
read_ppmf <- function(state, path){
  states <- ppmf::states

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
#' @importFrom utils download.file
#' @importFrom zip unzip
#'
#' @concept getdata
#'
#' @examples
#' \dontrun{
#' # Takes a few minutes and requires read access to files
#' temp <- tempdir()
#' path <- download_ppmf(dsn = 'ppmf_12', dir = temp)
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
                     'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_12.zip',
                     'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_4.zip')

  download.file(zip_path, temp)

  # then create the desired file:
  cat('File downloaded. Unzipping, please wait.\n')
  zip::unzip(zipfile = temp, exdir = dir)

  # rename it
  down_name <- ifelse(version == '12',
                      'ppmf_20210428_eps12-2_P.csv',
                      'ppmf_20210428_eps4-5_P.csv')

  file.rename(from = stringr::str_glue('{dir}/{down_name}'), to = path)

  # and return where we put it:
  return(path)

}


#' Get PPMF File Links
#'
#' Returns the urls for the data. This will be expanded to link to prior or
#' any new releases.
#'
#'
#' @param version string in '12' or '4' signifying the 12.2 or 4.5 versions respectively
#' @param release string. Only option is '04.28.2021' currently.
#' @param compressed boolean. Return a compressed version (TRUE). FALSE gives the
#' Census Bureau link to the uncompressed data.
#' @return a string with url
#' @export
#'
#' @concept getdata
#'
#' @examples
#' # 04.28.2021 version 12.2
#' get_ppmf_links()
#' # 04.28.2021 version 4.5
#' get_ppmf_links(version = '4')
get_ppmf_links <- function(version = '12', release = '04.28.2021', compressed = TRUE){
  match.arg(version, choices = c('12', '4'))

  # if(release == '04.28.2021'){
  if(compressed){
    if (version == '12') {
      'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_12.zip'
    } else {
      'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_4.zip'
    }
  } else {
    if (version == '12') {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/ppmf20210428/ppmf_20210428_eps12-2_P.csv'
    } else {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/ppmf20210428/ppmf_20210428_eps4-5_P.csv'
    }
  }
  #}

}

