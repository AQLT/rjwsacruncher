
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjwsacruncher

[![R-CMD-check](https://github.com/AQLT/rjwsacruncher/workflows/R-CMD-check/badge.svg)](https://github.com/AQLT/rjwsacruncher/actions)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjwsacruncher)](https://cran.r-project.org/package=rjwsacruncher)
[![CRAN last
release](http://www.r-pkg.org/badges/last-release/rjwsacruncher)](https://cran.r-project.org/package=rjwsacruncher)
[![CRAN monthly
downloads](http://cranlogs.r-pkg.org/badges/rjwsacruncher?color=lightgrey)](https://cran.r-project.org/package=rjwsacruncher)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/rjwsacruncher?color=lightgrey)](https://cran.r-project.org/package=rjwsacruncher)

The goal of rjwsacruncher is to launch quickly and easily the
[JWSACruncher](https://github.com/jdemetra/jwsacruncher) of
[JDemetra+](https://github.com/jdemetra/jdemetra-app). The JWSACruncher
is a console tool that allows to update a JDemetra+ workspace and to
export the results without having to open JDemetra+. More details on the
JWSACruncher can be found on
<https://github.com/jdemetra/jwsacruncher/wiki>.

## Installation

``` r
# Install release version from CRAN
install.packages("rjwsacruncher")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("AQLT/rjwsacruncher")
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
under `D:/jdemetra-cli-2.2.3/`:

``` r
options(cruncher_bin_directory = "D:/jdemetra-cli-2.2.3/bin/")
```

The export items can be changed with the function
“default\_matrix\_item” and “default\_tsmatrix\_series”:

``` r
# To get the default values:
head(getOption("default_matrix_item"))
#> [1] "period"       "span.start"   "span.end"     "span.n"       "span.missing"
#> [6] "espan.start"
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
