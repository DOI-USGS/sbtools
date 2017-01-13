ScienceBase R Tools
===

[![Build Status](https://travis-ci.org/USGS-R/sbtools.svg?branch=master)](https://travis-ci.org/USGS-R/sbtools) [![codecov.io](https://codecov.io/github/USGS-R/sbtools/coverage.svg?branch=master)](https://codecov.io/github/USGS-R/sbtools?branch=master)
Tools for interfacing R with ScienceBase data services.

##Package Description

This package provides a rich interface to USGS's [ScienceBase](https://www.sciencebase.gov/), a data cataloging and collaborative data management platform. For further information, see the [sbtools manuscript in The R Journal](https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf). See `citation('sbtools')` for how to cite the package.


##Package Installation
To install the `sbtools` package, you must be using R 3.0 or greater and run the following command:

```r
install.packages("sbtools")
```

To get inter-CRAN release updates, use the command:
```r
install.packages("sbtools",repos="https://owi.usgs.gov/R")
```

To get cutting-edge changes, install from GitHub using the `devtools` packages:

```r
library(devtools)
install_github("USGS-R/sbtools")
```

##Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:

[https://github.com/USGS-R/sbtools/issues](https://github.com/USGS-R/sbtools/issues)

### Current CRAN information

|Version|Monthly Downloads|Total Downloads|
|----------|------------|------------|
|[![CRAN version](https://www.r-pkg.org/badges/version/sbtools)](https://cran.r-project.org/package=sbtools)|[![](https://cranlogs.r-pkg.org/badges/sbtools)](https://cran.r-project.org/package=sbtools)|[![](https://cranlogs.r-pkg.org/badges/grand-total/sbtools)](https://cran.r-project.org/package=sbtools)|


##Examples

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


##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

