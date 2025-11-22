# convert objects into a stars object

convert objects into a stars object

## Usage

``` r
# S3 method for class 'ncdfgeom'
st_as_stars(.x, ..., sf_geometry = NA)

# S3 method for class 'CFDataset'
st_as_stars(
  .x,
  ...,
  .var = .x$var_names[1],
  all_compatible = length(.var) == 1
)

# S3 method for class 'CFVariable'
st_as_stars(.x, ...)

# S3 method for class 'cubble_df'
st_as_stars(.x, ..., check_times = FALSE)

# S3 method for class 'OpenStreetMap'
st_as_stars(.x, ..., as_col = FALSE)

# S3 method for class 'stars_proxy'
st_as_stars(
  .x,
  ...,
  downsample = 0,
  url = attr(.x, "url"),
  envir = parent.frame()
)

# S3 method for class 'data.frame'
st_as_stars(.x, ..., dims = coords, xy, y_decreasing = TRUE, coords = 1:2)

# S3 method for class 'Raster'
st_as_stars(.x, ..., att = 1, ignore_file = FALSE)

# S3 method for class 'SpatRaster'
st_as_stars(
  .x,
  ...,
  ignore_file = FALSE,
  as_attributes = all(terra::is.factor(.x))
)

# S3 method for class 'sf'
st_as_stars(.x, ..., dims = attr(.x, "sf_column"))

st_as_stars(.x, ...)

# S3 method for class 'list'
st_as_stars(.x, ..., dimensions = NULL)

# Default S3 method
st_as_stars(.x = NULL, ..., raster = NULL)

# S3 method for class 'stars'
st_as_stars(.x, ..., curvilinear = NULL, crs = st_crs("OGC:CRS84"))

# S3 method for class 'bbox'
st_as_stars(
  .x,
  ...,
  nx,
  ny,
  dx = dy,
  dy = dx,
  xlim = .x[c("xmin", "xmax")],
  ylim = .x[c("ymin", "ymax")],
  values = 0,
  n = 64800,
  pretty = FALSE,
  inside = FALSE,
  nz,
  proxy = FALSE
)

# S3 method for class 'xts'
st_as_stars(.x, ..., dimensions, name = "attr")
```

## Arguments

- .x:

  object to convert

- ...:

  in case `.x` is of class `bbox`, arguments passed on to
  [pretty](https://rdrr.io/r/base/pretty.html). In case `.x` is of class
  `nc_proxy`, arguments passed on to [`read_ncdf`](read_ncdf.md).

- sf_geometry:

  sf data.frame with geometry and attributes to be added to stars
  object. Must have same number of rows as timeseries instances.

- .var:

  Character vector with names of netCDF data variables to convert into
  stars attributes. All data variables have to use the same set of axes.
  If omitted, the first data variable will be used.

- all_compatible:

  Logical flag that indicates if all data variables with the same axes
  as argument \`var\` uses should be converted into attributes. Default
  is \`TRUE\` when only one data variable is specified, \`FALSE\`
  otherwise.

- check_times:

  logical; should we check that the time stamps of all time series are
  identical?

- as_col:

  logical; return rgb numbers (FALSE) or (character) color values
  (TRUE)?

- downsample:

  integer: if larger than 0, downsample with this rate (number of pixels
  to skip in every row/column); if length 2, specifies downsampling rate
  in x and y.

- url:

  character; URL of the stars endpoint where the data reside

- envir:

  environment to resolve objects in

- dims:

  the column names or indices that form the cube dimensions

- xy:

  the x and y raster dimension names or indices; only takes effect after
  `dims` has been specified, see details

- y_decreasing:

  logical; if TRUE, (numeric) y values get a negative delta (decrease
  with increasing index)

- coords:

  same as dims, for symmetry with
  [st_as_sf](https://r-spatial.github.io/sf/reference/st_as_sf.html)

- att:

  see factorValues; column in the RasterLayer's attribute table

- ignore_file:

  logical; if `TRUE`, ignore the SpatRaster object file name

- as_attributes:

  logical; if `TRUE` and `.x` has more than one layer, load these as
  separate attributes rather than as a band or time dimension (only
  implemented for the case where `ignore_file` is `TRUE`)

- dimensions:

  object of class dimensions

- raster:

  character; the names of the dimensions that denote raster dimensions

- curvilinear:

  only for creating curvilinear grids: named length 2 list holding
  longitude and latitude matrices or stars arrays, or the names of the
  corresponding attributes in `.x`; the names of this vector should
  correspond to raster dimensions the matrices are associated with; see
  Details.

- crs:

  object of class `crs` with the coordinate reference system of the
  values in `curvilinear`; see details

- nx:

  integer; number of cells in x direction; see details

- ny:

  integer; number of cells in y direction; see details

- dx:

  numeric or object of class units; cell size in x direction; see
  details

- dy:

  numeric or object of class units; cell size in y direction; see
  details

- xlim:

  length 2 numeric vector with extent (min, max) in x direction

- ylim:

  length 2 numeric vector with extent (min, max) in y direction

- values:

  value(s) to populate the raster values with

- n:

  the (approximate) target number of grid cells

- pretty:

  logical; should cell coordinates have
  [pretty](https://rdrr.io/r/base/pretty.html) values?

- inside:

  logical; should all cells entirely fall inside the bbox, potentially
  not covering it completely (`TRUE`), or always cover the bbox
  (`FALSE`), or find a good approximation (`NA`, default)?

- nz:

  integer; number of cells in z direction; if missing no z-dimension is
  created.

- proxy:

  logical; should a `stars_proxy` object be created? (requires
  gdal_create binary when sf \< 1.0-6)

- name:

  character; attribute name for array from an `xts` object

## Details

For the `ncdfgeom` method: objects are point-timeseries with optional
line or polygon geometry for each timeseries specified with the
`sf_geometry` parameter. See ncdfgeom for more about this NetCDF-based
format for geometry and timeseries.

If `xy` is not specified and the first two dimensions in `dims` are both
numeric, then it is set to these two dimensions.

The `st_as_stars` method for `sf` objects without any additional
arguments returns a one-dimensional data cube with a dimension for the
simple features geometries, and all remaining attributes as data cube
attributes. When used with further arguments, the method for
`data.frame`s is called.

if `curvilinear` is a list with `stars` objects with longitude and
latitude values, its coordinate reference system is typically not that
of the latitude and longitude values. If `curvilinear` contains the
names of two arrays in `.x`, then these are removed from the returned
object.

For the `bbox` method: if `pretty` is `TRUE`, raster cells may extend
the coordinate range of `.x` on all sides. If in addition to `nx` and
`ny`, `dx` and `dy` are also missing, these are set to a single value
computed as `sqrt(diff(xlim)*diff(ylim)/n)`.

If `nx` and `ny` are missing and `values` is a matrix, the number of
columns and rows of the matrix are taken.

Otherwise, if `nx` and `ny` are missing, they are computed as the
(ceiling, floor, or rounded to integer value) of the ratio of the (x or
y) range divided by (dx or dy), depending on the value of `inside`.
Positive `dy` will be made negative. Further named arguments (`...`) are
passed on to `pretty`. If `dx` or `dy` are `units` objects, their value
is converted to the units of `st_crs(.x)` (only when sf \>= 1.0-7).

for the `xts` methods, if `dimensions` are provided, time has to be the
first dimension.

## Examples

``` r
if (require(plm, quietly = TRUE)) {
 data(Produc, package = "plm")
 st_as_stars(Produc)
}
#> 
#> Attaching package: ‘plm’
#> The following objects are masked from ‘package:dplyr’:
#> 
#>     between, lag, lead
#> stars object with 2 dimensions and 9 attributes
#> attribute(s):
#>     region         pcap              hwy            water        
#>  5      :136   Min.   :  2627   Min.   : 1827   Min.   :  228.5  
#>  8      :136   1st Qu.:  7097   1st Qu.: 3858   1st Qu.:  764.5  
#>  4      :119   Median : 17572   Median : 7556   Median : 2266.5  
#>  1      :102   Mean   : 25037   Mean   :10218   Mean   : 3618.8  
#>  3      : 85   3rd Qu.: 27692   3rd Qu.:11267   3rd Qu.: 4318.7  
#>  6      : 68   Max.   :140217   Max.   :47699   Max.   :24592.3  
#>  (Other):170                                                     
#>      util               pc               gsp              emp         
#>  Min.   :  538.5   Min.   :  4053   Min.   :  4354   Min.   :  108.3  
#>  1st Qu.: 2488.3   1st Qu.: 21651   1st Qu.: 16502   1st Qu.:  475.0  
#>  Median : 7008.8   Median : 40671   Median : 39987   Median : 1164.8  
#>  Mean   :11199.4   Mean   : 58188   Mean   : 61014   Mean   : 1747.1  
#>  3rd Qu.:11598.5   3rd Qu.: 64796   3rd Qu.: 68126   3rd Qu.: 2114.1  
#>  Max.   :80728.1   Max.   :375342   Max.   :464550   Max.   :11258.0  
#>                                                                       
#>      unemp       
#>  Min.   : 2.800  
#>  1st Qu.: 5.000  
#>  Median : 6.200  
#>  Mean   : 6.602  
#>  3rd Qu.: 7.900  
#>  Max.   :18.000  
#>                  
#> dimension(s):
#>       from to offset delta              values
#> state    1 48     NA    NA ALABAMA,...,WYOMING
#> year     1 17   1970     1                NULL
if (require(dplyr, quietly = TRUE)) {
  # https://stackoverflow.com/questions/77368957/
spatial_dim <- st_sf(
  ID = 1:3,
  geometry = list(
    st_polygon(list(
      cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
    )),
    st_polygon(list(
      cbind(c(1, 2, 2, 1, 1), c(0, 0, 1, 1, 0))
    )),
    st_polygon(list(
      cbind(c(2, 3, 3, 2, 2), c(0, 0, 1, 1, 0))
    ))
  )
)
weekdays_dim <- data.frame(weekdays = c("Monday", "Tuesday", "Wednesday", 
    "Thursday", "Friday", "Saturday", "Sunday"))
hours_dim <- data.frame(hours = c("8am", "11am", "4pm", "11pm"))
sf_dta <- spatial_dim |>
  cross_join(weekdays_dim)|>
  cross_join(hours_dim) |>
  mutate(population = rnorm(n(), mean = 1000, sd = 200)) |>
  select(everything(), geometry)

st_as_stars(sf_dta, dims = c("weekdays", "hours", "geometry"))
}
#> stars object with 3 dimensions and 2 attributes
#> attribute(s):
#>                 Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> ID            1.0000   1.0000    2.000    2.000    3.000    3.000
#> population  594.2642 903.1372 1013.056 1003.978 1106.926 1324.842
#> dimension(s):
#>          from to point
#> weekdays    1  7    NA
#> hours       1  4    NA
#> geometry    1  3 FALSE
#>                                                                 values
#> weekdays                                             Monday,...,Sunday
#> hours                                                     8am,...,11pm
#> geometry POLYGON ((0 0, 1 0, 1 1, ...,...,POLYGON ((2 0, 3 0, 3 1, ...
demo(nc, echo=FALSE,ask=FALSE)
st_as_stars(nc)
#> stars object with 1 dimensions and 14 attributes
#> attribute(s):
#>      AREA           PERIMETER         CNTY_         CNTY_ID    
#>  Min.   :0.0420   Min.   :0.999   Min.   :1825   Min.   :1825  
#>  1st Qu.:0.0910   1st Qu.:1.324   1st Qu.:1902   1st Qu.:1902  
#>  Median :0.1205   Median :1.609   Median :1982   Median :1982  
#>  Mean   :0.1263   Mean   :1.673   Mean   :1986   Mean   :1986  
#>  3rd Qu.:0.1542   3rd Qu.:1.859   3rd Qu.:2067   3rd Qu.:2067  
#>  Max.   :0.2410   Max.   :3.640   Max.   :2241   Max.   :2241  
#>     NAME               FIPS               FIPSNO         CRESS_ID      
#>  Length:100         Length:100         Min.   :37001   Min.   :  1.00  
#>  Class :character   Class :character   1st Qu.:37050   1st Qu.: 25.75  
#>  Mode  :character   Mode  :character   Median :37100   Median : 50.50  
#>                                        Mean   :37100   Mean   : 50.50  
#>                                        3rd Qu.:37150   3rd Qu.: 75.25  
#>                                        Max.   :37199   Max.   :100.00  
#>      BIR74           SID74          NWBIR74           BIR79      
#>  Min.   :  248   Min.   : 0.00   Min.   :   1.0   Min.   :  319  
#>  1st Qu.: 1077   1st Qu.: 2.00   1st Qu.: 190.0   1st Qu.: 1336  
#>  Median : 2180   Median : 4.00   Median : 697.5   Median : 2636  
#>  Mean   : 3300   Mean   : 6.67   Mean   :1050.8   Mean   : 4224  
#>  3rd Qu.: 3936   3rd Qu.: 8.25   3rd Qu.:1168.5   3rd Qu.: 4889  
#>  Max.   :21588   Max.   :44.00   Max.   :8027.0   Max.   :30757  
#>      SID79          NWBIR79       
#>  Min.   : 0.00   Min.   :    3.0  
#>  1st Qu.: 2.00   1st Qu.:  250.5  
#>  Median : 5.00   Median :  874.5  
#>  Mean   : 8.36   Mean   : 1352.8  
#>  3rd Qu.:10.25   3rd Qu.: 1406.8  
#>  Max.   :57.00   Max.   :11631.0  
#> dimension(s):
#>      from  to refsys point
#> geom    1 100  NAD27 FALSE
#>                                                             values
#> geom MULTIPOLYGON (((-81.47276...,...,MULTIPOLYGON (((-78.65572...
st_as_stars(st_drop_geometry(nc), dims = "NAME")
#> stars object with 1 dimensions and 13 attributes
#> attribute(s):
#>      AREA           PERIMETER         CNTY_         CNTY_ID    
#>  Min.   :0.0420   Min.   :0.999   Min.   :1825   Min.   :1825  
#>  1st Qu.:0.0910   1st Qu.:1.324   1st Qu.:1902   1st Qu.:1902  
#>  Median :0.1205   Median :1.609   Median :1982   Median :1982  
#>  Mean   :0.1263   Mean   :1.673   Mean   :1986   Mean   :1986  
#>  3rd Qu.:0.1542   3rd Qu.:1.859   3rd Qu.:2067   3rd Qu.:2067  
#>  Max.   :0.2410   Max.   :3.640   Max.   :2241   Max.   :2241  
#>     FIPS               FIPSNO         CRESS_ID           BIR74      
#>  Length:100         Min.   :37001   Min.   :  1.00   Min.   :  248  
#>  Class :character   1st Qu.:37050   1st Qu.: 25.75   1st Qu.: 1077  
#>  Mode  :character   Median :37100   Median : 50.50   Median : 2180  
#>                     Mean   :37100   Mean   : 50.50   Mean   : 3300  
#>                     3rd Qu.:37150   3rd Qu.: 75.25   3rd Qu.: 3936  
#>                     Max.   :37199   Max.   :100.00   Max.   :21588  
#>      SID74          NWBIR74           BIR79           SID79      
#>  Min.   : 0.00   Min.   :   1.0   Min.   :  319   Min.   : 0.00  
#>  1st Qu.: 2.00   1st Qu.: 190.0   1st Qu.: 1336   1st Qu.: 2.00  
#>  Median : 4.00   Median : 697.5   Median : 2636   Median : 5.00  
#>  Mean   : 6.67   Mean   :1050.8   Mean   : 4224   Mean   : 8.36  
#>  3rd Qu.: 8.25   3rd Qu.:1168.5   3rd Qu.: 4889   3rd Qu.:10.25  
#>  Max.   :44.00   Max.   :8027.0   Max.   :30757   Max.   :57.00  
#>     NWBIR79       
#>  Min.   :    3.0  
#>  1st Qu.:  250.5  
#>  Median :  874.5  
#>  Mean   : 1352.8  
#>  3rd Qu.: 1406.8  
#>  Max.   :11631.0  
#> dimension(s):
#>      from  to             values
#> NAME    1 100 Ashe,...,Brunswick
data.frame(expand.grid(x=1:5, y = 1:5), z = rnorm(25)) |> st_as_stars()
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>         Min.   1st Qu.     Median       Mean   3rd Qu.     Max.
#> z  -2.194688 -1.452389 -0.5143981 -0.4837413 0.2901747 1.627052
#> dimension(s):
#>   from to offset delta x/y
#> x    1  5    0.5     1 [x]
#> y    1  5    5.5    -1 [y]
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
#> Reading layer `nc.gpkg' from data source 
#>   `/home/runner/work/_temp/Library/sf/gpkg/nc.gpkg' using driver `GPKG'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
st_as_stars(nc)
#> stars object with 1 dimensions and 14 attributes
#> attribute(s):
#>      AREA           PERIMETER         CNTY_         CNTY_ID    
#>  Min.   :0.0420   Min.   :0.999   Min.   :1825   Min.   :1825  
#>  1st Qu.:0.0910   1st Qu.:1.324   1st Qu.:1902   1st Qu.:1902  
#>  Median :0.1205   Median :1.609   Median :1982   Median :1982  
#>  Mean   :0.1263   Mean   :1.673   Mean   :1986   Mean   :1986  
#>  3rd Qu.:0.1542   3rd Qu.:1.859   3rd Qu.:2067   3rd Qu.:2067  
#>  Max.   :0.2410   Max.   :3.640   Max.   :2241   Max.   :2241  
#>     NAME               FIPS               FIPSNO         CRESS_ID      
#>  Length:100         Length:100         Min.   :37001   Min.   :  1.00  
#>  Class :character   Class :character   1st Qu.:37050   1st Qu.: 25.75  
#>  Mode  :character   Mode  :character   Median :37100   Median : 50.50  
#>                                        Mean   :37100   Mean   : 50.50  
#>                                        3rd Qu.:37150   3rd Qu.: 75.25  
#>                                        Max.   :37199   Max.   :100.00  
#>      BIR74           SID74          NWBIR74           BIR79      
#>  Min.   :  248   Min.   : 0.00   Min.   :   1.0   Min.   :  319  
#>  1st Qu.: 1077   1st Qu.: 2.00   1st Qu.: 190.0   1st Qu.: 1336  
#>  Median : 2180   Median : 4.00   Median : 697.5   Median : 2636  
#>  Mean   : 3300   Mean   : 6.67   Mean   :1050.8   Mean   : 4224  
#>  3rd Qu.: 3936   3rd Qu.: 8.25   3rd Qu.:1168.5   3rd Qu.: 4889  
#>  Max.   :21588   Max.   :44.00   Max.   :8027.0   Max.   :30757  
#>      SID79          NWBIR79       
#>  Min.   : 0.00   Min.   :    3.0  
#>  1st Qu.: 2.00   1st Qu.:  250.5  
#>  Median : 5.00   Median :  874.5  
#>  Mean   : 8.36   Mean   : 1352.8  
#>  3rd Qu.:10.25   3rd Qu.: 1406.8  
#>  Max.   :57.00   Max.   :11631.0  
#> dimension(s):
#>      from  to refsys point
#> geom    1 100  NAD27 FALSE
#>                                                             values
#> geom MULTIPOLYGON (((-81.47276...,...,MULTIPOLYGON (((-78.65572...
```
