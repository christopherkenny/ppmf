# Add ppmf12 path to Renviron

Add ppmf12 path to Renviron

## Usage

``` r
add_ppmf12_path(path, overwrite = FALSE, install = FALSE)
```

## Arguments

- path:

  path where ppmf12 data is stored

- overwrite:

  Defaults to FALSE. Should existing ppmf12 in Renviron be overwritten?

- install:

  Defaults to FALSE. Should ppmf12 be added to '~/.Renviron' file?

## Value

path, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- tempfile(fileext = '.csv')
add_ppmf12_path(tp)
path12 <- Sys.getenv('path12')
} # }
```
