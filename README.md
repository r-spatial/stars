
<!-- README.md is generated from README.Rmd. Please edit that file - rmarkdown::render('README.Rmd', output_format = 'github_document', output_file = 'README.md') -->
Spatiotemporal Arrays: Raster and Vector Datacubes
==================================================

[![Build Status](https://travis-ci.org/r-spatial/stars.png?branch=master)](https://travis-ci.org/r-spatial/stars) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-spatial/stars?branch=master&svg=true)](https://ci.appveyor.com/project/edzerpebesma/stars) [![codecov](https://codecov.io/gh/r-spatial/stars/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatial/stars) [![CRAN](http://www.r-pkg.org/badges/version/stars)](https://cran.r-project.org/package=stars) [![cran checks](https://cranchecks.info/badges/worst/stars)](https://cran.r-project.org/web/checks/check_results_stars.html) [![Downloads](http://cranlogs.r-pkg.org/badges/stars?color=brightgreen)](http://www.r-pkg.org/pkg/stars)

Spatiotemporal data often comes in the form of dense arrays, with space and time being array dimensions. Examples include

-   socio-economic or demographic data,
-   environmental variables monitored at fixed stations,
-   raster maps
-   time series of satellite images with multiple spectral bands,
-   spatial simulations, and
-   climate or weather model output.

This R package provides classes and methods for reading, manipulating, plotting and writing such data cubes, to the extent that there are proper formats for doing so.

Raster and vector data cubes
----------------------------

The canonical data cube most of us have in mind is that where two dimensions represent spatial raster dimensions, and the third time (or band), as e.g. shown here:

``` r
knitr::include_graphics("https://raw.githubusercontent.com/r-spatial/stars/master/images/cube1.png")
```

<img src="https://raw.githubusercontent.com/r-spatial/stars/master/images/cube1.png" width="50%" />

By data cubes however we also consider higher-dimensional cubes (hypercubes) such as a five-dimensional cube where in addition to time, spectral band and sensor form dimensions:

``` r
knitr::include_graphics("https://raw.githubusercontent.com/r-spatial/stars/master/images/cube2.png")
```

<img src="https://raw.githubusercontent.com/r-spatial/stars/master/images/cube2.png" width="50%" />

or lower-dimensional cubes such as a raster image:

``` r
suppressPackageStartupMessages(library(tidyverse))
library(stars)
# Loading required package: abind
# Loading required package: sf
# Linking to GEOS 3.7.0, GDAL 2.4.0, PROJ 5.2.0
tif = system.file("tif/L7_ETMs.tif", package = "stars")
read_stars(tif) %>%
  slice(prec, index = 1, along = "band") %>%
  plot()
```

![](images/unnamed-chunk-3-1.png)

The `raster` package
--------------------

Package `raster` is probably still the most powerful package for handling this kind of data in memory and on disk, but does not address non-raster time series, rasters time series with multiple attributes, rasters with mixed type attributes, or spatially distributed sets of satellite images.

Other `stars` resources:
------------------------

-   blog posts: [first](http://r-spatial.org/r/2017/11/23/stars1.html), [second](https://www.r-spatial.org/r/2018/03/22/stars2.html), [third](https://www.r-spatial.org/r/2018/03/23/stars3.html)
-   vignettes: [first](https://r-spatial.github.io/stars/articles/stars1.html), [second](https://r-spatial.github.io/stars/articles/stars2.html), [third](https://r-spatial.github.io/stars/articles/stars3.html), [fourth](https://r-spatial.github.io/stars/articles/stars4.html), [fifth](https://r-spatial.github.io/stars/articles/stars5.html)
-   How `raster` functions map to `stars` functions: [wiki](https://github.com/r-spatial/stars/wiki/How-%60raster%60-functions-map-to-%60stars%60-functions)
-   the original [R Consortium proposal](https://github.com/edzer/stars/blob/master/PROPOSAL.md).

### Acknowledgment

This project has been realized with financial [support](https://www.r-consortium.org/blog/2017/04/03/q1-2017-isc-grants) from the

<a href="https://www.r-consortium.org/projects/awarded-projects"> <img src="http://pebesma.staff.ifgi.de/RConsortium_Horizontal_Pantone.png" width="400"> </a>
