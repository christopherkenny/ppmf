#' Download PPMF Files
#'
#'  Downloads zipped ppmf files from GitHub.
#'
#' @param dsn (data save name) string to unzip the data to
#' @param dir the folder or directory to save the file in
#' @param version string in '19r', '19', '12' or '4' signifying the revised 19.61,
#' original 19.61, 12.2 or 4.5 versions respectively
#' @param overwrite If a file is found at path/dsn, should it be overwritten?
#' Defaults to FALSE.
#'
#' @return a string path to where the file was downloaded to
#' @export
#'
#' @concept getdata
#'
#' @examples
#' \dontrun{
#' # Takes a few minutes and requires read access to files
#' temp <- tempdir()
#' path <- download_ppmf(dsn = 'ppmf_12', dir = temp)
#' }
download_ppmf <- function(dsn, dir = '', version = '19r', overwrite = FALSE){
  match.arg(version, choices = c('19r', '19','12', '4'))

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

  zip_path <- 'https://github.com/christopherkenny/ppmf_data/releases/download/08122021/ppmf_19.zip'
  if(version == '12') {
    zip_path <- 'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_12.zip'
  } else if (version == '4') {
    zip_path <- 'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_4.zip'
  } else if (version == '19r') {
    zip_path <- 'https://github.com/christopherkenny/ppmf_data/releases/download/04032023/ppmf_19_ordered.zip'
  }


  utils::download.file(zip_path, temp)

  # then create the desired file:
  cat('File downloaded. Unzipping, please wait.\n')
  zip::unzip(zipfile = temp, exdir = dir)

  # rename it
  down_name <- 'ppmf_20210608_P.csv'
  if (version == '12') {
    down_name <- 'ppmf_20210428_eps12-2_P.csv'
  } else if (version == '4') {
    down_name <- 'ppmf_20210428_eps4-5_P.csv'
  } else if (version == '19r') {
    down_name <- 'ppmf_20230403_P_ordered.csv'
  }


  file.rename(from = stringr::str_glue('{dir}/{down_name}'), to = path)

  # and return where we put it:
  path
}


#' Get PPMF File Links
#'
#' Returns the urls for the data. This will be expanded to link to prior or
#' any new releases.
#'
#'
#' @param version string in '19r',, '19', '12' or '4' signifying the 19.61, 12.2, or 4.5 versions respectively
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
get_ppmf_links <- function(version = '19r', compressed = TRUE){
  match.arg(version, choices = c('19r', '19', '12', '4'))

  if(compressed){
    if (version == '12') {
      'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_12.zip'
    } else if (version == '4') {
      'https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_4.zip'
    } else if (version == '19') {
      'https://github.com/christopherkenny/ppmf_data/releases/download/08122021/ppmf_19.zip'
    } else if (version == '19r') {
      'https://github.com/christopherkenny/ppmf_data/releases/download/04032023/ppmf_19_ordered.zip'
    }
  } else {
    if (version == '12') {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/ppmf20210428/ppmf_20210428_eps12-2_P.csv'
    } else if (version == '4') {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/ppmf20210428/ppmf_20210428_eps4-5_P.csv'
    } else if (version == '19') {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/ppmf20210608/ppmf_20210608_P.csv'
    } else if (version == '19r') {
      'https://www2.census.gov/programs-surveys/decennial/2020/program-management/data-product-planning/2010-demonstration-data-products/04-Demonstration_Data_Products_Suite/2023-04-03/2023-04-03_Privacy-Protected_Microdata_File/2023-04-03-ppmf_p.csv'
    }
  }
}
