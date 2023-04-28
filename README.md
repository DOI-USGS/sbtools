
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ScienceBase R Tools

Tools for interfacing R with ScienceBase data services.

## Package Description

This package provides a rich interface to USGS’s
[ScienceBase](https://www.sciencebase.gov/), a data cataloging and
collaborative data management platform. For further information, see the
[sbtools manuscript in The R
Journal](https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf)
(USGS IP-075498). See `citation('sbtools')` for how to cite the package.

### Recommended Citation:

      Winslow, LA, S Chamberlain, AP Appling, and JS Read. 2016. sbtools: 
      A package connecting R to cloud-based data for collaborative online 
      research. The R Journal 8:387-398.

Package source code DOI: <https://doi.org/10.5066/P912NGFV>

| Linux                                                                             | Test Coverage                                                                                                                                |
|-----------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------|
| ![R-CMD-check](https://github.com/doi-usgs/sbtools/workflows/R-CMD-check/badge.svg) | [![codecov.io](https://codecov.io/github/DOI-USGS/sbtools/coverage.svg?branch=master)](https://app.codecov.io/github/DOI-USGS/sbtools?branch=master) |

### Current CRAN information

| Version                                                                                                     | Monthly Downloads                                                                            | Total Downloads                                                                                          |
|-------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| [![CRAN version](https://www.r-pkg.org/badges/version/sbtools)](https://cran.r-project.org/package=sbtools) | [![](https://cranlogs.r-pkg.org/badges/sbtools)](https://cran.r-project.org/package=sbtools) | [![](https://cranlogs.r-pkg.org/badges/grand-total/sbtools)](https://cran.r-project.org/package=sbtools) |

## Package Installation

To install the `sbtools` package, you must be using R 3.0 or greater and
run the following command:

``` r
install.packages("sbtools")
```

To get cutting-edge changes, install from GitHub using the `devtools`
packages:

``` r
remotes::install_github("DOI-USGS/sbtools")
```

## Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:

<https://github.com/DOI-USGS/sbtools/issues>

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter
Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)

## Examples

``` r
library(sbtools)

# Query ScienceBase for data about Antarctica
query_sb_text('Antarctica', limit=1)
#> [[1]]
#> <ScienceBase Item> 
#>   Title: Tectonics of Antarctica
#>   Creator/LastUpdatedBy:      / 
#>   Provenance (Created / Updated):   / 
#>   Children: 
#>   Item ID: 505ba488e4b08c986b3203fa
#>   Parent ID: 4f4e4771e4b07f02db47e1e4

# Query for a specific DOI
query_sb_doi('10.5066/F7M043G7')
#> list()

# Inspect the contents of the above item
children <- item_list_children('557f0367e4b023124e8ef621')
#> Request failed [404]. Retrying in 1 seconds...
#> Request failed [404]. Retrying in 2.2 seconds...
#> Item not found for ID=557f0367e4b023124e8ef621. Either the item does not exist or the item is secured and requires authentication to access.FALSE
#> Request failed [404]. Retrying in 1.7 seconds...
#> Request failed [404]. Retrying in 1 seconds...
sapply(children, function(child) child$title)
#> list()

# Log in (requires a ScienceBase account) and create an item
authenticate_sb(Sys.getenv("sb_user")) # type in password
my_home_item <- user_id()
new_item <- item_create(title = 'new test item', parent_id = my_home_item)
test.txt <- file.path(tempdir(), 'test.txt')
writeLines(c('this is','my data file'), "test.txt")
item_append_files(new_item, "test.txt")
#> <ScienceBase Item> 
#>   Title: new test item
#>   Creator/LastUpdatedBy:     dblodgett@usgs.gov / dblodgett@usgs.gov
#>   Provenance (Created / Updated):  2022-11-03T19:47:46Z / 2022-11-03T19:47:48Z
#>   Children: FALSE
#>   Item ID: 63641ae2d34ebe4425074d93
#>   Parent ID: 4f7afec1e4b0b2f259355f30
item_list_files(new_item)$fname
#> [1] "test.txt"

item_rm(new_item)
#> Response [https://www.sciencebase.gov/catalog/item/63641ae2d34ebe4425074d93?format=json]
#>   Date: 2022-11-03 19:47
#>   Status: 200
#>   Content-Type: <unknown>
#> <EMPTY BODY>
unlink("test.txt")
```

## Release Procedure

For release of the sbtools package, a number of steps are required.

1.  Ensure all checks pass and code coverage is adequate.
2.  Ensure `NEWS.md` reflects updates in version.
3.  Update `DESCRIPTION` to reflect release version.
4.  Build source package and upload to CRAN.
5.  Once accepted to CRAN, convert `DISCLAIMER.md` to approved language.
6.  Change references to main branch in `code.json` to release version.
7.  Commit changes and tag repository at release version. Push tag.
8.  Change `code.json` and `DISCLAIMER.md` back to development mode.

# Disclaimer

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

This software is in the public domain because it contains materials that
originally came from the U.S. Geological Survey, an agency of the United
States Department of Interior. For more information, see the [official
USGS copyright
policy](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits "official USGS copyright policy")

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
