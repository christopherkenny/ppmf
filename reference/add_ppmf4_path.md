# Add ppmf4 path to Renviron

Add ppmf4 path to Renviron

## Usage

``` r
add_ppmf4_path(path, overwrite = FALSE, install = FALSE)
```

## Arguments

- path:

  path where ppmf4 data is stored

- overwrite:

  Defaults to FALSE. Should existing ppmf4 in Renviron be overwritten?

- install:

  Defaults to FALSE. Should ppmf4 be added to '~/.Renviron' file?

## Value

path, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- tempfile(fileext = '.csv')
add_ppmf4_path(tp)
path4 <- Sys.getenv('path4')
} # }
```
