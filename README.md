
<!-- 
README.md is generated from README.Rmd. Please edit that file
rmarkdown::render("README.Rmd") 
-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/equidIO)](http://cran.r-project.org/package=equidIO) [![Travis-CI Build Status](https://travis-ci.org/bgctw/equidIO.svg?branch=master)](https://travis-ci.org/bgctw/equidIO)

Overview
--------

The `equidIO` package provides functionaly to deal with tabular data composed of records of equidistant time steps.

Main functions are

-   efficient reading of parts of such files: [readEquidistantCsv](https://github.com/bgctw/equidIO/tree/master/vignettes/readEquidistantCsv.md)
-   updating of grouped data.frames with ensuring equidistant time steps: [updateOutputRange](https://github.com/bgctw/equidIO/tree/master/vignettes/updateOutputRange.md)

Installation
------------

``` r
# From CRAN (in future)
#install.packages("equidIO")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("bgctw/equidIO")
```

Usage
-----

See the [package vignettes](https://github.com/bgctw/equidIO/tree/master/vignettes) (\*.md) for examples.
