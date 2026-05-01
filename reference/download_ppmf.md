# Download PPMF Files

Downloads zipped ppmf files from GitHub.

## Usage

``` r
download_ppmf(dsn, dir = "", version = "19r", overwrite = FALSE)
```

## Arguments

- dsn:

  (data save name) string to unzip the data to

- dir:

  the folder or directory to save the file in

- version:

  string in '19r', '19', '12' or '4' signifying the revised 19.61,
  original 19.61, 12.2 or 4.5 versions respectively

- overwrite:

  If a file is found at path/dsn, should it be overwritten? Defaults to
  FALSE.

## Value

a string path to where the file was downloaded to

## Examples

``` r
if (FALSE) { # \dontrun{
# Takes a few minutes and requires read access to files
temp <- tempdir()
path <- download_ppmf(dsn = 'ppmf_12', dir = temp)
} # }
```
