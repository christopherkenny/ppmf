# Read PPMF data and Merge with Census 2010 Data

Read PPMF data and Merge with Census 2010 Data

## Usage

``` r
read_merge_ppmf(
  state,
  level,
  versions = c("19"),
  prefixes = paste0("v", versions, "_"),
  paths = Sys.getenv(paste0("ppmf", versions))
)
```

## Arguments

- state:

  state abbreviation

- level:

  geography level. One of 'block', 'block group', 'tract', 'county'

- versions:

  character vector of ppmf versions. Currently '19', '12', and/or '4'

- prefixes:

  prefixes to give pop and vap columns in output. Default is
  `paste0('v', versions, '_')`

- paths:

  paths to PPMF data. Default is `Sys.getenv(paste0('ppmf', versions))`

## Value

sf tibble of PPMF merged with Census 2010 data

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires Census Bureau API
de_bg <- read_merge_ppmf('DE', 'block group')
} # }
```
