ScienceBase R Tools
===

Tools for interfacing R with ScienceBase data services.

## Package Description

This package provides a rich interface to USGS's [ScienceBase](https://www.sciencebase.gov/), a data cataloging and collaborative data management platform. For further information, see the [sbtools manuscript in The R Journal](https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf) (USGS IP-075498). See `citation('sbtools')` for how to cite the package.

## Package Status

[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

|Linux|Test Coverage|
|----------|------------|
| ![R-CMD-check](https://github.com/usgs-r/sbtools/workflows/R-CMD-check/badge.svg) |[![codecov.io](https://codecov.io/github/USGS-R/sbtools/coverage.svg?branch=master)](https://codecov.io/github/USGS-R/sbtools?branch=master)|

### Current CRAN information

|Version|Monthly Downloads|Total Downloads|
|----------|------------|------------|
|[![CRAN version](https://www.r-pkg.org/badges/version/sbtools)](https://cran.r-project.org/package=sbtools)|[![](https://cranlogs.r-pkg.org/badges/sbtools)](https://cran.r-project.org/package=sbtools)|[![](https://cranlogs.r-pkg.org/badges/grand-total/sbtools)](https://cran.r-project.org/package=sbtools)|

## Package Installation
To install the `sbtools` package, you must be using R 3.0 or greater and run the following command:

```r
install.packages("sbtools")
```

To get cutting-edge changes, install from GitHub using the `devtools` packages:

```r
remotes::install_github("USGS-R/sbtools")
```

## Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:

[https://github.com/USGS-R/sbtools/issues](https://github.com/USGS-R/sbtools/issues)


Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

## Examples

```r
library(sbtools)

# Query ScienceBase for data about Antarctica
query_sb_text('Antarctica', limit=1)
## [[1]]
## <ScienceBase Item>
##   Title: Antarctica. Unnamed peak in the Nunataks near camp 18. January 21, 1978.
##   Creator/LastUpdatedBy:      /
##   Provenance (Created / Updated):   /
##   Children:
##   Item ID: 51dc2e89e4b0f81004b79cf6
##   Parent ID: 519ba0a3e4b0e4e151ef5dd9

# Query for a specific DOI
query_sb_doi('10.5066/F7M043G7')
## [[1]]
## <ScienceBase Item>
##   Title: 2013 Raw Ground Penetrating Radar Data on Alaska's Glaciers
##   Creator/LastUpdatedBy: /
##   Provenance (Created / Updated): 2015-06-15T16:55:03Z / 2015-12-15T20:39:06Z
##   Children: TRUE
##   Item ID: 557f0367e4b023124e8ef621
##   Parent ID: 5474ec49e4b04d7459a7eab2

# Inspect the contents of the above item
children <- item_list_children('557f0367e4b023124e8ef621')
sapply(children, function(child) child$title)
## [1] "Raw Ground Penetrating Radar Data, Valdez Glacier, Alaska; 2013"   
## [2] "Raw Ground Penetrating Radar Data, Gulkana Glacier, Alaska; 2013"  
## [3] "Raw Ground Penetrating Radar Data, Eklutna Glacier, Alaska; 2013"  
## [4] "Raw Ground Penetrating Radar Data, Eureka Glacier, Alaska; 2013"   
## [5] "Raw Ground Penetrating Radar Data, Taku Glacier, Alaska; 2013"     
## [6] "Raw Ground Penetrating Radar Data, Scott Glacier, Alaska; 2013"    
## [7] "Raw Ground Penetrating Radar Data, Wolverine Glacier, Alaska; 2013"

# Log in (requires a ScienceBase account) and create an item
authenticate_sb() # type in username and password
my_home_item <- user_id()
new_item <- item_create(title = 'new test item', parent_id = my_home_item)
test.txt <- file.path(tempdir(), 'test.txt')
writeLines(c('this is','my data file'), test.txt)
item_append_files(new_item, test.txt)
item_list_files(new_item)$fname
## [1] "test.txt"
```

## Release Procedure

For release of the sbtools package, a number of steps are required.

1. Ensure all checks pass and code coverage is adequate.
1. Ensure `NEWS.md` reflects updates in version.
1. Update `DESCRIPTION` to reflect release version.
1. Build source package and upload to CRAN.
1. Once accepted to CRAN, convert `DISCLAIMER.md` to approved language.
1. Change references to main branch in `code.json` to release version.
1. Commit changes and tag repository at release version. Push tag.
1. Change `code.json` and `DISCLAIMER.md` back to development mode.

## Disclaimer

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

From: https://www2.usgs.gov/fsp/fsp_disclaimers.asp#5

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits "official USGS copyright policy")

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.
This software is provided "AS IS."

