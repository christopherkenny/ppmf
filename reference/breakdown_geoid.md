# Breakdown GEOID into Components

Breakdown GEOID into Components

## Usage

``` r
breakdown_geoid(ppmf, GEOID = GEOID)
```

## Arguments

- ppmf:

  tibble of ppmf data

- GEOID:

  Column in ppmf with GEOID. Default is `GEOID`.

## Value

tibble. ppmf with columns added for state, county, tract, block group,
and/or block

## Examples

``` r
data(ppmf_ex)
ppmf_ex <- ppmf_ex |> add_geoid()
ppmf_ex <- ppmf_ex |> censable::breakdown_geoid()
```
