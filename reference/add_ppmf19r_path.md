# Add ppmf19r path to Renviron

Path for the 19.61 *r*eplication in 2023.

## Usage

``` r
add_ppmf19r_path(path, overwrite = FALSE, install = FALSE)
```

## Arguments

- path:

  path where ppmf19r data is stored

- overwrite:

  Defaults to FALSE. Should existing ppmf19 in Renviron be overwritten?

- install:

  Defaults to FALSE. Should ppmf19r be added to '~/.Renviron' file?

## Value

path, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- tempfile(fileext = '.csv')
add_ppmf19r_path(tp)
path19 <- Sys.getenv('path19')
} # }
```
