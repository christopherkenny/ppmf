# Add ppmf19 path to Renviron

Add ppmf19 path to Renviron

## Usage

``` r
add_ppmf19_path(path, overwrite = FALSE, install = FALSE)
```

## Arguments

- path:

  path where ppmf19 data is stored

- overwrite:

  Defaults to FALSE. Should existing ppmf19 in Renviron be overwritten?

- install:

  Defaults to FALSE. Should ppmf19 be added to '~/.Renviron' file?

## Value

path, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- tempfile(fileext = '.csv')
add_ppmf19_path(tp)
path19 <- Sys.getenv('path19')
} # }
```
