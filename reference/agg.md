# Aggregate PPMF Data

Aggregate PPMF Data

## Usage

``` r
agg(ppmf, group = GEOID, age = VOTING_AGE, race = CENRACE, hisp = CENHISP)
```

## Arguments

- ppmf:

  tibble of ppmf data

- group:

  Column in ppmf to group by, typically GEOID

- age:

  Column in ppmf containing 1 for not voting age and 2 for voting age

- race:

  Column in ppmf containing race codes

- hisp:

  Column in ppmf containing 1 for Not Hispanic and 2 for Hispanic

## Value

tibble of ppmf data aggregated by group with race classified with
columns:

- `group`: named by entry group

- `pop`: total population

- `pop_hisp`: total population - Hispanic or Latino (of any race)

- `pop_white`: total population - White alone, not Hispanic or Latino

- `pop_black`: total population - Black or African American alone, not
  Hispanic or Latino

- `pop_aian`: total population - American Indian and Alaska Native
  alone, not Hispanic or Latino

- `pop_asian`: total population - Asian alone, not Hispanic or Latino

- `pop_nhpi`: total population - Native Hawaiian and Other Pacific
  Islander alone, not Hispanic or Latino

- `pop_other`: total population - Some Other Race alone, not Hispanic or
  Latino

- `pop_two`: total population - Population of two or more races, not
  Hispanic or Latino

- `vap`: voting age population

- `vap_hisp`: voting age population - Hispanic or Latino (of any race)

- `vap_white`: voting age population - White alone, not Hispanic or
  Latino

- `vap_black`: voting age population - Black or African American alone,
  not Hispanic or Latino

- `vap_aian`: voting age population - American Indian and Alaska Native
  alone, not Hispanic or Latino

- `vap_asian`: voting age population - Asian alone, not Hispanic or
  Latino

- `vap_nhpi`: voting age population - Native Hawaiian and Other Pacific
  Islander alone, not Hispanic or Latino

- `vap_other`: voting age population - Some Other Race alone, not
  Hispanic or Latino

- `vap_two`: voting age population - Population of two or more races,
  not Hispanic or Latino

## Examples

``` r
data(ppmf_ex)
ppmf_ex <- ppmf_ex |> add_geoid()
blocks <- agg(ppmf_ex)
```
