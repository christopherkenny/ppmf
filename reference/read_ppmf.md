# Read in PPMF Data

This reads in PPMF data from a file. Use
[`download_ppmf()`](https://christopherkenny.github.io/ppmf/reference/download_ppmf.md)
if you do not have a local copy of the ppmf data.

## Usage

``` r
read_ppmf(state, path, ...)
```

## Arguments

- state:

  two letter state (+ DC + PR) abbreviation or two digit state fips code

- path:

  where the data is saved to

- ...:

  additional arguments passed on to
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)

## Value

tibble of ppmf data

## Examples

``` r
if (FALSE) { # \dontrun{
# Takes a few minutes and requires read access to files
temp <- tempdir()
path <- download_ppmf('ppmf_12.csv', dir = temp)
# If you already have it downloaded, point to it with path:
ppmf <- read_ppmf('AL', path)
} # }
```
