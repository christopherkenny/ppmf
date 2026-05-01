# Replace Race Categories

Replaces the Census's numeric categories for race with less specific
racial classifications, typically useful for redistricting purposes.

## Usage

``` r
replace_race(ppmf, race = CENRACE)
```

## Arguments

- ppmf:

  tibble of ppmf data

- race:

  Column in ppmf containing race codes

## Value

tibble with race column replaced by simpler racial classifications

## Examples

``` r
data(ppmf_ex)
ppmf_ex |> replace_race()
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
