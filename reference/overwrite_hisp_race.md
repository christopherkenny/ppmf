# Overwrite Races with Hispanic

Overwrite Races with Hispanic

## Usage

``` r
overwrite_hisp_race(ppmf, race = CENRACE, hisp = CENHISP)
```

## Arguments

- ppmf:

  tibble of ppmf data

- race:

  Column in ppmf containing race codes

- hisp:

  Column in ppmf containing 1 for Not Hispanic and 2 for Hispanic

## Value

tibble with race column entries replaced if the individual is Hispanic

## Examples

``` r
data(ppmf_ex)
ppmf_ex |> replace_race() |> overwrite_hisp_race()
#> # A tibble: 10,588 × 11
#>    VINTAGE          TABBLKST TABBLKCOU TABTRACT TABBLKGRP TABBLK RTYPE GQTYPE_PL
#>    <chr>            <chr>    <chr>     <chr>        <dbl>  <dbl> <dbl>     <dbl>
#>  1 20210428_eps12-… 01       105       686800           1   1000     3         0
#>  2 20210428_eps12-… 01       105       686800           1   1000     3         0
#>  3 20210428_eps12-… 01       105       686800           1   1000     3         0
#>  4 20210428_eps12-… 01       105       686800           1   1000     3         0
#>  5 20210428_eps12-… 01       105       686800           1   1000     3         0
#>  6 20210428_eps12-… 01       105       686800           1   1003     3         0
#>  7 20210428_eps12-… 01       105       686800           1   1003     3         0
#>  8 20210428_eps12-… 01       105       686800           1   1003     3         0
#>  9 20210428_eps12-… 01       105       686800           1   1003     3         0
#> 10 20210428_eps12-… 01       105       686800           1   1003     3         0
#> # ℹ 10,578 more rows
#> # ℹ 3 more variables: VOTING_AGE <dbl>, CENHISP <dbl>, CENRACE <chr>
```
