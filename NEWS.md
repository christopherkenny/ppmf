# ppmf 0.1.0
* Add support for PLB 19.61

# ppmf 0.0.6
* Adds `censable` dependency and function `read_merge_ppmf` to create cleaner merging workflows.
* Removes references to the string `block_group`, moving towards the more consistent `block group`.

# ppmf 0.0.5
* Fix issue where `read_ppmf()` can't read in files if there's an object named `states` in the global environment.

# ppmf 0.0.4
* Update information for public use

# ppmf 0.0.3
* Uses GitHub release links for compressed data
* Add get_ppmf_links() to get the links for data
* Basic pkgdown site
* Function to add ppmf paths to environment

# ppmf 0.0.2
* Added a `NEWS.md` file to track changes to the package.
* Added function breakdown_geoid() to convert GEOIDs into component parts
* fix typo in ppmf_ex names
* Runs tools::resaveRdaFiles('data/') to compress Rda for faster install
