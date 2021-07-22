
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppmf <a href='https://christopherkenny.github.io/ppmf'><img src='man/figures/logo.png' align="right" height="129" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/ppmf/workflows/R-CMD-check/badge.svg)](https://github.com/christopherkenny/ppmf/actions)
<!-- badges: end -->

The goal of ppmf is to convert Census Privacy Protected Microdata Files
into somewhat wider data aggregated to a geographic level.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("christopherkenny/ppmf")
```

## Basics

Load the package:

``` r
library(ppmf)
```

Download and read data with:

``` r
path <- download_ppmf(dsn = 'filename.csv', dir = 'some/directory')
al <- read_ppmf(state = 'AL', path = path)
```

For future use, I recommend storing the path to the data for future
sessions using:

``` r
add_pmmf12_path(path)
```

Then the path can be recovered with:

``` r
path12 <- Sys.getenv('ppmf12')
```

Once you’ve read in what you want, you can aggregate it to the right
level:

``` r
al <- al %>% add_geoid()
blocks <- agg(al)
```

And aggregated data can use the GEOID to merge with shapefiles:

``` r
library(dplyr) # to clean up the data

shp <- tigris::blocks('AL', year = 2010) %>% 
  select(GEOID10, geometry) %>% rename(GEOID = GEOID10)
shp <- shp %>% left_join(blocks, by = 'GEOID')

# always clean shp!
shp[is.na(shp)] <- 0
```
