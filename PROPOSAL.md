# Scalable, spatiotemporal Tidy Arrays for R

[Edzer Pebesma](https://github.com/edzer/), [Michael
Sumner](https://github.com/mdsumer/), [Etienne
Racine](https://github.com/etiennebr)

## Summary

A lot of spatiotemporal data takes the form of dense,
multidimensional arrays. Examples are

* population counts by region, year and age group 
* meteorological data by variable, time step and sensor location 
* satellite imagery, e.g. energy by color, location and time step
* or climate data (e.g. surface temperature by location, time and climate scenario). 

Although such data _can_ be represented in long tables, it is
rarely done in for larger datasets because it has to replicate
dimension indexes, and because the array form provides faster
access.  R's native arrays 
* can't handle heterogeneous data records (e.g. consisting of a `numeric`, a `logical` and a `Date`) like we typically find in data.frame's, 
* can only deal with in-memory data, and 
* do not handle spatial or temporal array dimensions. 

This project will (i) implement a flexible and generic
multidimensional array model for heterogeneous records that
(ii) handles strong spatial and temporal referencing of
array indexes, and (iii) scales from moderately sized _in-
memory_ data, to large _on-disk_ data, and to massive
data held in one or more remote servers, while using
a unified user interface that follows the [tidy tools
manifesto](https://cran.r-project.org/web/packages/tidyverse/vignettes/manifesto.html).

Now that [simple features for R](https://github.com/edzer/sfr) has
provided a large modernizing of the handling and analysis vector
data (points, lines, polygons) in R, it is time for raster data to
catch up. This proposal aims at 2D/3D temporal rasters, as well as
time series of feature data.

_Italics: the [R Consortium call](https://www.r-consortium.org/projects/call-for-proposals); deadline [Feb 10, 2017](https://www.r-consortium.org/blog/2016/12/06/call-for-proposals)_

## The Problem

_What problem do you want to solve? Why is it a problem? Who does it affect? What will solving the problem enable? This section should include a brief summary of existing work, such as R packages that may be relevant._

How do we handle and analyze large amounts of spatially referenced
time series data in R? How do we handle satellite imagery that don't
fit on a local disc, with R, or for which we need a small cluster
to finish computation within acceptable time? How can we quickly
and easily develop an analysis by testing it on a small portion of
the spatiotemporal datasets, before deploying it on the massive
data set?  How can we use pipe-based workflows or dplyr-verbs on such
data sets? How can we visualy explore high-dimensional raster data?

Today, many people use R for large spatiotemporal data, but hit
limits related to usability, user interface, and scalability. The
[r-sig-geo](https://stat.ethz.ch/pipermail/r-sig-geo/) mailing list
documents many of these cases.

### Existing work

Base R supports n-dimensional homogeneous arrays of basic
types (double, integer, logical, logical), in-memory. Package
[ff](https://CRAN.R-project.org/package=ff) supports out-of-memory
data structures, held on local disc, but no spatial or temporal
references on dimensions.

Spatial packages include
[rgdal](https://CRAN.R-project.org/package=rgdal),
which lets you read and write raster data, possibly
piece-by-piece, in one of 142 different file formats. Package
[raster](https://CRAN.R-project.org/package=raster) allows users
to work with raster maps or stacks of them, where a stack can
either refer to different bands (color) or different time steps,
but not both. Package raster can iterate functions over large
files on disc, and takes care of the caching, using either
rgdal or [ncdf4](https://CRAN.R-project.org/package=ncdf4).
Another package for reading and writing NetCDF is
[RNetCDF](https://cran.r-project.org/package=RNetCDF). Packages that
are more dedicated to a single data source or type include include
[RStoolbox](https://CRAN.R-project.org/package=RStoolbox),
[MODIS](https://CRAN.R-project.org/package=MODIS),
[landsat](https://CRAN.R-project.org/package=landsat), and
[hsdar](https://CRAN.R-project.org/package=hsdar); each of these
relies on raster or rgdal for file-based I/O.

CRAN package
[spacetime](https://CRAN.R-project.org/package=spacetime) provides
heterogeneous records, using a `data.frame` for attributes.
It keeps indexes for each record to a spatial geometry (grid
cell/point/polygon) and time instance or period; it keeps all data in
memory and builds on [xts](https://CRAN.R-project.org/package=xts)
for temporal, and [sp](https://CRAN.R-project.org/package=sp)
for spatial reference.

Relevant work outside R includes
* [GDAL](http://www.gdal.org/), in particular gdal [virtual tiles](http://www.gdal.org/gdalbuildvrt.html) for building arbitrary large grid data sets from many individual files,
* [SciDB](http://www.paradigm4.com/), an open source array database which has no spatial or temporal capabilities
* [SciDB4geo](https://github.com/appelmar/scidb4geo), a SciDB Plugin
for Managing Spatial and Temporal Reference Information of Arrays, and
[SciDB4gdal](https://github.com/appelmar/scidb4geo), a GDAL driver for SciDB arrays, two activities to make SciDB databases aware of space and time
* [PostGIS Raster](http://postgis.net/docs/RT_reference.html), a raster data extension of [PostGIS](http://www.postgis.net/)
* [Rasdaman](http://www.rasdaman.com/), an array database dedicate to images, which is partially open source.

Since there is a definite trend that
[downloading Earth observation data is no longer
feasible](http://r-spatial.org/2016/11/29/openeo.html),
we will have to work towards a solution where the data
are accessed over a web service interface. 

Possible interface for this is [opencpu](http://www.opencpu.org/)
or [Rserve](https://cran.r-project.org/package=Rserve), but the
final choice depends on how the service back-end organizes its data
and processes.

## The Plan: 

_How are you going to solve the problem? Include the concrete actions you will take and an estimated timeline. What are likely failure modes and how will you recover from them?_

We will develop an R package and container infrastructure that 
* supports dense arrays with heterogeneous records, 
* supports flexible reference from array dimensions to space, where space can be gridded (2D/3D raster) or a set of simple features (irregular)
* supports flexible reference from array dimensions to time (`POSIXct`, `Date`)
* allow for regular arrays (fixed cell size / time step) and irregular arrays
* allows working in memory, on local disc, and on a remote computer (using a web service interface),
* allows R functions to be passed on to the web service back-end, and executed there (potentially in parallel),
* uses in-memory proxies to large arrays, allowing to work with the first _n_ records before computations are carried out on the full arrays (similar to how dplyr does this)
* allows pipe-based workflows, uses `data.frame`s, and dplyr-style verbs.

We will document the software and provide tutorials and reproducible
data analysis examples using locally downloaded imagery, as well as
scalable examples accessing larger (> 1 Tb) datasets using docker
container images.

We will document the RESTful API that connects the R client with the
web service holding (and processing) the big Earth observation data.

We will also develop a migration path for the raster package (over
43K lines of R, C and C++ code), and its functionality, into the
new infrastructure.

We will publish the resulting products in an open access form,
in the R journal, but also in a journal (or on a conference) more
directed to the Earth observation community.

Timeline:
* Month 1-2: work out design, decide web service technology, basic web service API design
* Month 3-6: programming the R package, testing with smaller data sets
* Month 7-8: testing on larger datasets, develop test cases, deploy on docker containers
* Month 9-12: write tutorials, develop teaching material and reproducible examples
* Month 9-12: experiment with different back-ends: file-based, or database such as SciDB

Failure modes:
* we can't get the RESTful API to work properly; solution path: ask the rOpenSci people for help (Scott Chamberlain,
Jeroen Ooms)
* downloading large image sets is too cumbersome (slow) for the larger tutorial examples; solution path: deploy a test server for teaching/experimenting purposes in the Amazon cloud, where Landsat and Sentinel imagery is readily available

## How Can The ISC Help: 

_Please describe how you think the ISC can help. If you are looking for a cash grant include a detailed itemised budget and spending plan. We expect that most of the budget will be allocated for people, but we will consider funding travel, equipment and services, such as cloud computing resources with good justification. If you are seeking to start an ISC working group, then please describe the goals of the group and provide the name of the individual who will be committed to leading and managing the group’s activities. Also describe how you think the ISC can we help promote your project._

We will use most funding to actually develop the R package and web service API. Total costs are estimated at 10,000 USD, and break down in:
* workshop: travel costs for Etienne Racine and Michael Sumner to visit Muenster, or another venue (USD 2500).
* Programming, project communication: (USD 7000).
* Cloud deployment in the Amazon cloud (USD 500).

## Dissemination: 

_How will you ensure that your work is available to the widest number of people? Please specify the open source license will you use, how you will host your code so that others can contribute, and how you will publicise your work. We encourage you to plan at least two blog posts to the R consortium blog: one to announce the project, and one to write up what you achieved._

We will regularly post blogs about the project on [r-spatial.org](http://r-spatial.org/), use twitter, post to [r-sig-geo](https://stat.ethz.ch/mailman/listinfo/r-sig-geo), stackoverflow, and communicate through github issues. The project will involve on github, most likely in the [r-spatial](https://github.com/r-spatial/) organisation.We will work under a permissive open source license, probably LGPL-2.1.  Pull requests will be encouraged. R consortium blogs will be provided at start and end. Publications in _the R Journal_ and scientific outlets are foreseen.

