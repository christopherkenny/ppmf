# Add Standard GEOID to PPMF Data

Adds the GEOID identifier common to spatial census data sets, such as
those loaded by tigris. This allows for easier merging or aggregation by
a single variable.

## Usage

``` r
add_geoid(
  ppmf,
  state = TABBLKST,
  county = TABBLKCOU,
  tract = TABTRACT,
  block_group = TABBLKGRP,
  block = TABBLK,
  level = "block"
)
```

## Arguments

- ppmf:

  tibble of ppmf data

- state:

  Column in ppmf with state (fips) ID. Default is `TABBLKST`.

- county:

  Column in ppmf with county (fips) ID. Default is `TABBLKCOU`.

- tract:

  Column in ppmf with tract ID. Default is `TABBLKTRACT`.

- block_group:

  Column in ppmf with block group ID. Default is `TABBLKGRP`

- block:

  Column in ppmf with block ID. Default is `TABBLK`.

- level:

  Geographic level to write the GEOID for. Options are block (default),
  block_group, tract, and county.

## Value

input data ppmf with added column GEOID

## Examples

``` r
data(ppmf_ex)
ppmf_ex <- ppmf_ex |> add_geoid()
```
