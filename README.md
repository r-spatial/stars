# Spatiotemporal Raster and Vector Datacubes

[![Build Status](https://travis-ci.org/r-spatial/stars.png?branch=master)](https://travis-ci.org/r-spatial/stars) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-spatial/stars?branch=master&svg=true)](https://ci.appveyor.com/project/edzerpebesma/stars)
[![codecov](https://codecov.io/gh/r-spatial/stars/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatial/stars)
[![CRAN](http://www.r-pkg.org/badges/version/stars)](https://cran.r-project.org/package=stars) 
[![cran checks](https://cranchecks.info/badges/worst/stars)](https://cran.r-project.org/web/checks/check_results_stars.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/stars?color=brightgreen)](http://www.r-pkg.org/pkg/stars)

Spatiotemporal data often comes in the form of dense arrays,
with space and time being array dimensions. Examples include

* socio-economic or demographic data, 
* environmental variables monitored at fixed stations, 
* time series of satellite images with multiple spectral bands, 
* spatial simulations, and
* climate model results. 

Currently, R does not have infrastructure to handle and analyse such
arrays easily. Package raster is probably still the most powerful
package for handling this kind of data in memory and on disk, but
does not address non-raster time series, rasters time series with
multiple attributes, rasters with mixed type attributes, or spatially
distributed sets of satellite images. 

This project will not only deal with these cases, but also extend the
"in memory or on disk" model to that where the data are held remotely
in cloud storage, which is a more feasible option e.g. for satellite
data collected Today. We will implement pipe-based workflows that
are developed and tested on samples before they are evaluated for
complete datasets, and discuss the challenges of visualisation and
storage in such workflows. 

## Progress

* blog posts: [first](http://r-spatial.org/r/2017/11/23/stars1.html),
[second](https://www.r-spatial.org/r/2018/03/22/stars2.html),
[third](https://www.r-spatial.org/r/2018/03/23/stars3.html)
* vignettes: [first](https://r-spatial.github.io/stars/articles/stars1.html), [second](https://r-spatial.github.io/stars/articles/stars2.html), [third](https://r-spatial.github.io/stars/articles/stars3.html), [fourth](https://r-spatial.github.io/stars/articles/stars4.html), [fifth](https://r-spatial.github.io/stars/articles/stars5.html)

## Proposal

Read the full proposal [here](https://github.com/edzer/stars/blob/master/PROPOSAL.md).

### Acknowledgment

This project is being realized with financial
[support](https://www.r-consortium.org/blog/2017/04/03/q1-2017-isc-grants)
from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="http://pebesma.staff.ifgi.de/RConsortium_Horizontal_Pantone.png" width="400">
</a>
