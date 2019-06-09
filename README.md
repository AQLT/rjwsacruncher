
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjwsacruncher

[![Travis Build
Status](https://travis-ci.org/AQLT/rjwsacruncher.svg?branch=master)](https://travis-ci.org/AQLT/rjwsacruncher)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg?logo=github)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjdemetra)](https://cran.r-project.org/package=rjdemetra)

The goal of rjwsacruncher is to launch quickly and easily the
[JWSACruncher](https://github.com/jdemetra/jwsacruncher) of
[JDemetra+](https://github.com/jdemetra/jdemetra-app). The JWSACruncher
is a console tool that allows to update a JDemetra+ workspace’s and to
export the results without having to use JDemetra+. More details on the
JWSACruncher can be found on
<https://github.com/jdemetra/jwsacruncher/wiki>.

## Installation

The development version can be installed from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("AQLT/rjwsacruncher", build_vignettes = TRUE)
```

The JWSACruncher is not included in the package. It can be downloaded
from GitHub (<https://github.com/jdemetra/jwsacruncher/releases>) or
with the function `download_cruncher()`:

``` r
library(rjwsacruncher)
# Directory where to save the JWSACruncher:
directory <- tempdir()
download_cruncher(directory)
```

Since the version 2.2.0, the JWSACruncher needs Java 8 or higher to run.
If you cannot install it (for example for security reasons) you can
install a portable version of ‘Java’ (that does not require
administrator rights) and configure the JWSACruncher to use this
portable version with the function `configure_jwsacruncher()`.

## Usage

The JWSACruncher can be easily runned with the function
`cruncher_and_param()`. To use it, the path to the “Bin” folder of the
JWSACruncher has to be defined. For example, if the JWSACruncher is
under `D:/jdemetra-cli-2.2.2/`:

``` r
options(cruncher_bin_directory = "D:/jdemetra-cli-2.2.2/bin/")
```

The export items can be changed with the function
“default\_matrix\_item” and “default\_tsmatrix\_series”:

``` r
# To get the default values:
head(getOption("default_matrix_item"))
#> [1] "period"       "span.start"   "span.end"     "span.n"      
#> [5] "span.missing" "espan.start"
getOption("default_tsmatrix_series")
#> [1] "y"    "t"    "sa"   "s"    "i"    "ycal"
# To only export the seasonally adjusted series and its forecasts:
options(default_tsmatrix_series = c("sa", "sa_f"))
```

The run the JWSACruncher on the workspace `D:/workspace.xml` with the
refresh policy “lastoutliers”:

``` r
cruncher_and_param(workspace = "D:/workspace.xml",
                   policy = "lastoutliers")
```
