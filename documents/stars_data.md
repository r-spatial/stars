# An Earth data processing backend for testing and evaluating stars

[Edzer Pebesma](https://github.com/edzer/), 
[Michael Sumner](https://github.com/mdsumner/) 

## Summary

The amount of freely available data generated by either direct observation from satellites or from weather or climate model calculations exceeds our capacity to download and process locally. The only feasible approach to analyse such data is to send compute jobs to cloud servers with direct access to this data, and have the small summaries (maps, time series, numbers) returned. R lacks software to analyse such datasets on compute servers, and software to serve such processes.

The [stars](https://github.com/r-spatial/stars/) project will enable the
processing such data, locally as well as on a remote server, without
downloading the data. This project, `stars:backend`, will (i) create software
to run a back-end, (ii) develop scripts and tutorials that explain how such
a data server and processing backend can be set up, and (iii) create an instance
of such a backend in the AWS cloud that can be used for testing and evaluation
purposes, that serves a reasonable amount of geoscientific data (up to 4 Tb),
and that is publicly accessible.

## The problem 

There is a strong tradition of sharing data in Earth sciences: we only have one Earth, a small amount of shared satellites, and a limited number of climate and weather models. The geoscientific world has managed to set up protocols (file formats, web services) to share data. The currently working mode is to download data, and process it locally. However, now that these datasets have become too large to download, new architectures are needed that allow processing of the data close to where the data are, and downloading the much smaller results from these computations. The geoscientific community has not managed to find a good way of doing this, making it hard to use R for such purposes. Closed source solutions like [Google Earth Engine](https://earthengine.google.com/) fill a gap here, but lack the openness and transparency required for scientific application. 

As an example, the Sentinel-2 satellite pair covers the Earth
every six days with 15 spectral bands and pixels up to 10 x 10 m,
which leads to a data stream of more than 60 Tb/week. Data are
available as 100 km x 100 km tiles (see figure), each roughly a Gb,
with 57000 tiles coverng the Earth:

![](s2.png)

All Sentinel data is freely [downloadable](https://scihub.copernicus.eu/).

The [openEO](https://r-spatial.org/2016/11/29/openeo.html) blog further sketches the current situation. The [stars](https://github.com/r-spatial/stars) R consortium funded project creates client and middleware software for this problem. To test, evaluate and demonstrate the stars software (and demonstrate R provides a viable solution for these problems), a demonstrator backend is needed. This backend will serve moderately large data (up to 4 Tb) of different types (including spatiotemporal NetCDF model computations, and Landsat and Sentinel satellite imagery), and will be publicly available to evaluate and test the stars software for remote computing on large imagery.

### Existing work

R packages, as distributed over CRAN and/or github, are not suitable for distributing large amounts of data, and are also not suitable to demonstrate how data repositories can be organised. Yet, we want R packages to be able to work with large data repositories.

Two examples from data repositories for (amonst others) R users are the [global administrative data](http://gadm.org/) and the [global climate data](http://www.worldclim.org/), both set up by Robert Hijmans (author of [raster](https://cran.r-project.org/package=raster)) and co-workers. The latter has led to a publication which received over 11,000 citations, mainly from users of the dataset.

The [rOpenSci](https://ropensci.org/) project has also developed several packages for downloading geoscientific data, for instance [rnoaa](https://cran.r-project.org/package=rnoaa), but these all allow download - a useful ability that no longer scales.

As mentioned, [Google Earth Engine](https://earthengine.google.com/) provides remote processing on Tb-Pb sized dateset, but is not open source, and hence to some extent a black box. In addition, users have limited control over which data it serves, and which software it runs (e.g., it has a python interface but does not allow using numpy)

The Australian Antarctic Division has [R tools](https://github.com/AustralianAntarcticDivision/raadtools) to organise large amounts of Earth science data and serve it to researchers.

## The Plan: 

We will develop an R package that

* works as a back-end to the stars package for data downloading and offering data catalogs
* understands different data structure layouts and server configurations (e.g. nfs, S3)

and set up a web server in the AWS cloud that

* serves moderately large datasets of different kind, including NetCDF (climate models, sea surface temperature), GRIB (weather models), MODIS, Landsat and Sentinel data.
* can be used to evaluate the stars architecture, including image processing on the backend and performant interactive visualisation with the mapview/leaflet packages
* is accessible to everyone
* is fully transparent in revealing the way it has been set up, to make it easy for third parties to set up their own backends

We will document the software and provide tutorials and reproducible
data analysis examples.  We will publish the resulting products in an open access form, in the R journal, but also in a journal (or on a conference) more directed to the Earth observation community.

Timeline: start in Feb 2018.

* Month 1-2: work out design, choose AWS configuration, program R backend package
* Month 3-6: download datasets, test software
* Month 7-8: invite users for testing
* Month 9-12: write tutorials, develop teaching material and reproducible examples

Failure modes:

* AWS service costs are larger than expected; solution path: find further funds, change to more limited services; limit duration of experiments

## How Can The ISC Help: 

We will use most funding for cloud deployment, and to develop the R package. Total costs will be 5,000 USD, and breaks down in:

* AWS resources (8 core/32 Gb machine, 3 Tb disc; USD 3500)
* Programming, project communication: (USD 1500).

## Dissemination: 

We will regularly post blogs about the project on [r-spatial.org](https://r-spatial.org/), use twitter, post to [r-sig-geo](https://stat.ethz.ch/mailman/listinfo/r-sig-geo), stackoverflow, and communicate through github issues or gitter discussion. The project will live on GitHub, in the [r-spatial](https://github.com/r-spatial/) organisation. We will work under a permissive open source license, probably dual GPL + MIT. Pull requests will be encouraged. R consortium blogs will be provided at start and end. Publications in _the R Journal_ or other scientific outlets are foreseen.

