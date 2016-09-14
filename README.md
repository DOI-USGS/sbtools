ScienceBase R Tools
===

[![Build Status](https://travis-ci.org/USGS-R/sbtools.svg?branch=master)](https://travis-ci.org/USGS-R/sbtools) [![codecov.io](https://codecov.io/github/USGS-R/sbtools/coverage.svg?branch=master)](https://codecov.io/github/USGS-R/sbtools?branch=master)
Tools for interfacing R with ScienceBase data services.


##Package Installation
To install the dataRetrieval package, you must be using R 3.0 or greater and run the following command:

```R
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

### Current CRAN information:

|Version|Monthly Downloads|Total Downloads|
|----------|------------|------------|
|[![CRAN version](http://www.r-pkg.org/badges/version/sbtools)](https://cran.r-project.org/package=sbtools)|[![](http://cranlogs.r-pkg.org/badges/sbtools)](https://cran.r-project.org/package=sbtools)|[![](http://cranlogs.r-pkg.org/badges/grand-total/sbtools)](https://cran.r-project.org/package=sbtools)|


##Sample Workflow

```r
library(sbtools)
authenticate_sb()
temp.directory <- tempdir()
item_file_download('548b2b31e4b03f64633662a4', names='gdp_new.txt',
		destinations=file.path(temp.directory, 'gdp_new.txt'))
read.table(file.path(temp.directory, 'gdp_new.txt'), sep = ":", header = FALSE)
   V1     V2
1 RT   blkah		
		
```


##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

