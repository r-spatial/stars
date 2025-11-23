# read raster/array dataset from file or connection

read raster/array dataset from file or connection

## Usage

``` r
read_stars(
  .x,
  sub = TRUE,
  ...,
  options = character(0),
  driver = character(0),
  quiet = FALSE,
  NA_value = NA_real_,
  along = NA_integer_,
  RasterIO = list(),
  proxy = getOption("stars.n_proxy") %||% 1e+08,
  curvilinear = character(0),
  normalize_path = TRUE,
  RAT = character(0),
  tolerance = 1e-10,
  exclude = "",
  shorten = TRUE
)
```

## Arguments

- .x:

  character vector with name(s) of file(s) or data source(s) to be read,
  or a function that returns such a vector

- sub:

  character, integer or logical; name, index or indicator of
  sub-dataset(s) to be read

- ...:

  passed on to [st_as_stars](st_as_stars.md) if `curvilinear` was set

- options:

  character; opening options

- driver:

  character; driver to use for opening file. To override fixing for
  subdatasets and autodetect them as well, use `NULL`.

- quiet:

  logical; print progress output?

- NA_value:

  numeric value to be used for conversion into NA values; by default
  this is read from the input file

- along:

  length-one character or integer, or list; determines how several
  arrays are combined, see Details.

- RasterIO:

  list with named parameters for GDAL's RasterIO, to further control the
  extent, resolution and bands to be read from the data source; see
  details.

- proxy:

  logical; if `TRUE`, an object of class `stars_proxy` is read which
  contains array metadata only; if `FALSE` the full array data is read
  in memory. Always `FALSE` for curvilinear girds. If set to a number,
  defaults to `TRUE` when the number of cells to be read is larger than
  that number.

- curvilinear:

  length two character vector with names of subdatasets holding
  longitude and latitude values for all raster cells, or named length 2
  list holding longitude and latitude matrices; the names of this list
  should correspond to raster dimensions referred to

- normalize_path:

  logical; if `FALSE`, suppress a call to
  [normalizePath](https://rdrr.io/r/base/normalizePath.html) on `.x`

- RAT:

  character; raster attribute table column name to use as factor levels

- tolerance:

  numeric; passed on to
  [all.equal](https://rdrr.io/r/base/all.equal.html) for comparing
  dimension parameters.

- exclude:

  character; vector with category value(s) to exclude

- shorten:

  logical or character; if `TRUE` and `length(.x) > 1`, remove common
  start and end parts of array names; if character a new prefix

## Value

object of class `stars`

## Details

In case `.x` contains multiple files, they will all be read and combined
with [c.stars](c.stars.md). Along which dimension, or how should objects
be merged? If `along` is set to `NA` it will merge arrays as new
attributes if all objects have identical dimensions, or else try to
merge along time if a dimension called `time` indicates different time
stamps. A single name (or positive value) for `along` will merge along
that dimension, or create a new one if it does not already exist. If the
arrays should be arranged along one of more dimensions with values (e.g.
time stamps), a named list can passed to `along` to specify them; see
example.

`RasterIO` is a list with zero or more of the following named arguments:
`nXOff`, `nYOff` (both 1-based: the first row/col has offset value 1),
`nXSize`, `nYSize`, `nBufXSize`, `nBufYSize`, `bands`, `resample`. See
<https://gdal.org/en/latest/doxygen/classGDALDataset.html> for their
meaning; `bands` is an integer vector containing the band numbers to be
read (1-based: first band is 1). Note that if `nBufXSize` or `nBufYSize`
are specified for downsampling an image, resulting in an adjusted
geotransform. `resample` reflects the resampling method and has to be
one of: "nearest_neighbour" (the default), "bilinear", "cubic",
"cubic_spline", "lanczos", "average", "mode", or "Gauss".

Data that are read into memory (`proxy=FALSE`) are read into a numeric
(double) array, except for categorical variables which are read into an
numeric (integer) array of class `factor`.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x1 = read_stars(tif))
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif     1      54     69 68.91242      86  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
(x2 = read_stars(c(tif, tif)))
#> stars object with 3 dimensions and 2 attributes
#> attribute(s):
#>                Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif       1      54     69 68.91242      86  255
#> L7_ETMs.tif.1     1      54     69 68.91242      86  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
(x3 = read_stars(c(tif, tif), along = "band"))
#> stars object with 3 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>              Min. 1st Qu. Median    Mean 3rd Qu. Max.
#> L7_ETMs.tif    47      65     76 77.3419      87  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1  12      NA    NA                         NA    NA    
(x4 = read_stars(c(tif, tif), along = "new_dimensions")) # create 4-dimensional array
#> stars object with 4 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>              Min. 1st Qu. Median    Mean 3rd Qu. Max.
#> L7_ETMs.tif    47      65     76 77.3419      87  255
#> dimension(s):
#>                from  to  offset delta                     refsys point x/y
#> x                 1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y                 1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band              1   6      NA    NA                         NA    NA    
#> new_dimensions    1   2      NA    NA                         NA    NA    
x1o = read_stars(tif, options = "OVERVIEW_LEVEL=1")
t1 = as.Date("2018-07-31")
# along is a named list indicating two dimensions:
read_stars(c(tif, tif, tif, tif), along = list(foo = c("bar1", "bar2"), time = c(t1, t1+2)))
#> stars object with 5 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>              Min. 1st Qu. Median    Mean 3rd Qu. Max.
#> L7_ETMs.tif    47      65     76 77.3419      87  255
#> dimension(s):
#>      from  to     offset  delta                     refsys point     values x/y
#> x       1 349     288776   28.5 SIRGAS 2000 / UTM zone 25S FALSE       NULL [x]
#> y       1 352    9120761  -28.5 SIRGAS 2000 / UTM zone 25S FALSE       NULL [y]
#> band    1   6         NA     NA                         NA    NA       NULL    
#> foo     1   2         NA     NA                         NA    NA bar1, bar2    
#> time    1   2 2018-07-31 2 days                       Date    NA       NULL    

m = matrix(1:120, nrow = 12, ncol = 10)
dim(m) = c(x = 10, y = 12) # named dim
st = st_as_stars(m)
attr(st, "dimensions")$y$delta = -1
attr(st, "dimensions")$y$offset = 12
st
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1     1   30.75   60.5 60.5   90.25  120
#> dimension(s):
#>   from to offset delta point x/y
#> x    1 10      0     1 FALSE [x]
#> y    1 12     12    -1 FALSE [y]
tmp = tempfile(fileext = ".tif")
write_stars(st, tmp)
(red <- read_stars(tmp))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                       Min. 1st Qu. Median Mean 3rd Qu. Max.
#> file203c10008f09.tif     1   30.75   60.5 60.5   90.25  120
#> dimension(s):
#>   from to offset delta x/y
#> x    1 10      0     1 [x]
#> y    1 12     12    -1 [y]
read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 12,
   nBufXSize = 2, nBufYSize = 2))[[1]]
#>      [,1] [,2]
#> [1,]   33   93
#> [2,]   38   98
(red <- read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 12,
   nBufXSize = 2, nBufYSize = 2)))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                       Min. 1st Qu. Median Mean 3rd Qu. Max.
#> file203c10008f09.tif    33   36.75   65.5 65.5   94.25   98
#> dimension(s):
#>   from to offset delta x/y
#> x    1  2      0     5 [x]
#> y    1  2     12    -6 [y]
red[[1]] # cell values of subsample grid:
#>      [,1] [,2]
#> [1,]   33   93
#> [2,]   38   98
if (FALSE) { # \dontrun{
  plot(st, reset = FALSE, axes = TRUE, ylim = c(-.1,12.1), xlim = c(-.1,10.1),
    main = "nBufXSize & nBufYSize demo", text_values = TRUE)
  plot(st_as_sfc(red, as_points = TRUE), add = TRUE, col = 'red', pch = 16)
  plot(st_as_sfc(st_as_stars(st), as_points = FALSE), add = TRUE, border = 'grey')
  plot(st_as_sfc(red, as_points = FALSE), add = TRUE, border = 'green', lwd = 2)
} # }
file.remove(tmp)
#> [1] TRUE
```
