
# exwProject3

<!-- badges: start -->
[![R-CMD-check](https://github.com/ElliotWinters/exwProject3/workflows/R-CMD-check/badge.svg)](https://github.com/ElliotWinters/exwProject3/actions)
[![codecov](https://codecov.io/gh/ElliotWinters/exwProject3/branch/master/graph/badge.svg?token=F5MRWV5ZNI)](https://codecov.io/gh/ElliotWinters/exwProject3)
<!-- badges: end -->

exwProject3 is a demonstration of several tools for statistical inference and
prediction learned in STAT 302 at the University of Washington. Functions for
linear modeling, t-testing, k-nearest neighbors and random forest
cross validation are included.

## Installation

To download the exwProject3 package, use the code below.

``` r
install.packages("exwProject3")
library(exwProject3)
```

Alternatively, you can install the development version directly from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("ElliotWinters/exwProject3")
library(exwProject3)
```

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code:

``` r
library(exwProject3)
# Use this to view the vignette in the exwProject3 HTML help
help(package = "exwProject3", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "exwProject3")
```
