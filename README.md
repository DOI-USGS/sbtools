
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

| Linux                                                                               | Test Coverage                                                                                                                                        |
|-------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
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

## Release Procedure

For release of the sbtools package, a number of steps are required.

1.  Ensure all checks pass and code coverage is adequate.
2.  Ensure `NEWS.md` reflects updates in version.
3.  Update `DESCRIPTION` to reflect release version.
4.  Convert `DISCLAIMER.md` to approved language and rebuild
    `README.Rmd`.
5.  Create release candidate branch and commit release candidate.
6.  Build source package and upload to CRAN.
7.  Once accepted to CRAN, tag release candidate branch an push to
    repositories.
8.  Change `DISCLAIMER.md` back to development mode and increment
    description version.
9.  Merge release candidate and commit.
10. Open PR/MR in development state.

# Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
