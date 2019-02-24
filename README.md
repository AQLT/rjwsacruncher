
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
with the function `download_cruncher()`;

``` r
library(rjwsacruncher)
# Directory where to save the JWSACruncher:
directory <- tempdir()
download_cruncher(directory)
```
