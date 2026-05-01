# Get PPMF File Links

Returns the urls for the data. This will be expanded to link to prior or
any new releases.

## Usage

``` r
get_ppmf_links(version = "19r", compressed = TRUE)
```

## Arguments

- version:

  string in '19r',, '19', '12' or '4' signifying the 19.61, 12.2, or 4.5
  versions respectively

- compressed:

  boolean. Return a compressed version (TRUE). FALSE gives the Census
  Bureau link to the uncompressed data.

## Value

a string with url

## Examples

``` r
# 04.28.2021 version 12.2
get_ppmf_links()
#> [1] "https://github.com/christopherkenny/ppmf_data/releases/download/04032023/ppmf_19_ordered.zip"
# 04.28.2021 version 4.5
get_ppmf_links(version = '4')
#> [1] "https://github.com/christopherkenny/ppmf_data/releases/download/04282021/ppmf_4.zip"
```
