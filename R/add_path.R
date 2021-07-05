#' Add ppmf12 path to Renviron
#'
#' @param path path where ppmf12 data is stored
#' @param overwrite Defaults to FALSE. Should existing ppmf12 in Renviron be overwritten?
#' @param install Defaults to FALSE. Should ppmf12 be added to '~/.Renviron' file?
#'
#' @return path, invisibly
#' @export
#' @concept save
#' @examples
#' \dontrun{
#' tp <- tempfile(fileext = '.csv')
#' add_ppmf12_path(tp)
#' path12 <- Sys.getenv('path12')
#' }
#'
add_ppmf12_path <- function(path, overwrite = FALSE, install = FALSE) {
  if (missing(path)) {
    stop('Input `path` cannot be missing.')
  }

  if (install) {
    r_env <- file.path(Sys.getenv('HOME'), '.Renviron')

    if (!file.exists(r_env)) {
      file.create(r_env)
    }

    lines <- readLines(r_env)
    newline <- paste0("ppmf12='", path.expand(path), "'")

    exists <- stringr::str_detect(lines, 'ppmf12=')

    if (any(exists)) {
      if (sum(exists) > 1) {
        stop('Multiple entries in .Renviron have name matching input `name`.\nEdit manually with `usethis::edit_r_environ()`.')
      }

      if (overwrite) {
        lines[exists] <- newline
        writeLines(lines, r_env)
        message('Run `readRenviron("~/.Renviron")` to update your active environment.')
      } else {
        message('ppmf12 already exists in .Renviron. \nEdit manually with `usethis::edit_r_environ() or set `overwrite = TRUE`.')
      }
    } else {
      lines[length(lines) + 1] <- newline
      writeLines(lines, r_env)
      message('Run `readRenviron("~/.Renviron")` to update your active environment.')
    }
  } else {
    Sys.setenv(ppmf12 = path)
  }

  invisible(path)
}


#' Add ppmf4 path to Renviron
#'
#' @param path path where ppmf4 data is stored
#' @param overwrite Defaults to FALSE. Should existing ppmf4 in Renviron be overwritten?
#' @param install Defaults to FALSE. Should ppmf4 be added to '~/.Renviron' file?
#'
#' @return path, invisibly
#' @export
#'
#' @concept save
#' @examples
#' \dontrun{
#' tp <- tempfile(fileext = '.csv')
#' add_ppmf4_path(tp)
#' path4 <- Sys.getenv('path4')
#' }
#'
add_ppmf4_path <- function(path, overwrite = FALSE, install = FALSE) {
  if (missing(path)) {
    stop('Input `path` cannot be missing.')
  }

  if (install) {
    r_env <- file.path(Sys.getenv('HOME'), '.Renviron')

    if (!file.exists(r_env)) {
      file.create(r_env)
    }

    lines <- readLines(r_env)
    newline <- paste0("ppmf4='", path.expand(path), "'")

    exists <- stringr::str_detect(lines, 'ppmf4=')

    if (any(exists)) {
      if (sum(exists) > 1) {
        stop('Multiple entries in .Renviron have name matching input `name`.\nEdit manually with `usethis::edit_r_environ()`.')
      }

      if (overwrite) {
        lines[exists] <- newline
        writeLines(lines, r_env)
        message('Run `readRenviron("~/.Renviron")` to update your active environment.')
      } else {
        message('ppmf4 already exists in .Renviron. \nEdit manually with `usethis::edit_r_environ() or set `overwrite = TRUE`.')
      }
    } else {
      lines[length(lines) + 1] <- newline
      writeLines(lines, r_env)
      message('Run `readRenviron("~/.Renviron")` to update your active environment.')
    }
  } else {
    Sys.setenv(ppmf4 = path)
  }

  invisible(path)
}
