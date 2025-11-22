# get dimensions from stars object

get dimensions from stars object

## Usage

``` r
st_dimensions(.x, ...)

# S3 method for class 'stars'
st_dimensions(.x, ...)

st_dimensions(x) <- value

# S3 method for class 'stars'
st_dimensions(x) <- value

# S3 method for class 'stars_proxy'
st_dimensions(x) <- value

# S3 method for class 'list'
st_dimensions(x) <- value

# S3 method for class 'array'
st_dimensions(.x, ...)

# Default S3 method
st_dimensions(
  .x,
  ...,
  .raster,
  affine = c(0, 0),
  cell_midpoints = FALSE,
  point = FALSE
)

st_set_dimensions(
  .x,
  which,
  values = NULL,
  point = NULL,
  names = NULL,
  xy,
  ...
)

st_get_dimension_values(.x, which, ..., where = NA, max = FALSE, center = NA)
```

## Arguments

- .x:

  object to retrieve dimensions information from

- ...:

  further arguments

- x:

  object of class `dimensions`

- value:

  new object of class `dimensions`, with matching dimensions

- .raster:

  length 2 character array with names (if any) of the raster dimensions

- affine:

  numeric; specify parameters of the affine transformation

- cell_midpoints:

  logical; if `TRUE` AND the dimension values are strictly regular, the
  values are interpreted as the cell midpoint values rather than the
  cell offset values when calculating offset (i.e., the half-cell-size
  correction is applied); can have a value for each dimension, or else
  is recycled

- point:

  logical; does the pixel value (measure) refer to a point (location)
  value or to an pixel (area) summary value?

- which:

  integer or character; index or name of the dimension to be changed

- values:

  values for this dimension (e.g. `sfc` list-column), or length-1
  `dimensions` object; setting special value `NULL` removes dimension
  values, for instance to remove curvilinear raster coordinates

- names:

  character; vector with new names for all dimensions, or with the
  single new name for the dimension indicated by `which`

- xy:

  length-2 character vector; (new) names for the `x` and `y` raster
  dimensions

- where:

  character, one of 'start', 'center' or 'end'. Set to NA (default) to
  ignore and use `max` and `center` explictly. This argument provides a
  convenient alternative to setting `max` and `center`.

- max:

  logical; if `TRUE` return the end, rather than the beginning of an
  interval

- center:

  logical; if `TRUE` return the center of an interval; if `NA` return
  the center for raster dimensions, and the start of intervals in other
  cases

## Value

the `dimensions` attribute of `x`, of class `dimensions`

## Details

dimensions can be specified in two ways. The simplest is to pass a
vector with numeric values for a numeric dimension, or character values
for a categorical dimension. Parameter `cell_midpoints` is used to
specify whether numeric values refer to the offset (start) of a
dimension interval (default), or to the center; the center case is only
available for regular dimensions. For rectilinear numeric dimensions,
one can specify either a vector with cell borders (start values), or a
data.frame with two columns named "start" and "end", with the respective
interval start and end values. In the first case, the end values are
computed from the start values by assuming the last two intervals have
equal width.

## Examples

``` r
x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
# Landsat 7 ETM+ band semantics: https://landsat.gsfc.nasa.gov/the-enhanced-thematic-mapper-plus/
# set bands to values 1,2,3,4,5,7:
(x1 = st_set_dimensions(x, "band", values = c(1,2,3,4,5,7), names = "band_number", point = TRUE))
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif     1      54     69 68.91242      86  255
#> dimension(s):
#>             from  to  offset delta                     refsys point  values x/y
#> x              1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE    NULL [x]
#> y              1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE    NULL [y]
#> band_number    1   6      NA    NA                         NA  TRUE 1,...,7    
# set band values as bandwidth
rbind(c(0.45,0.515), c(0.525,0.605), c(0.63,0.69), c(0.775,0.90), c(1.55,1.75), c(2.08,2.35)) %>%
  units::set_units("um") -> bw # or: units::set_units(µm) -> bw
# set bandwidth midpoint:
(x2 = st_set_dimensions(x, "band", values = 0.5 * (bw[,1]+bw[,2]), 
   names = "bandwidth_midpoint", point = TRUE))
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif     1      54     69 68.91242      86  255
#> dimension(s):
#>                    from  to  offset delta                     refsys point
#> x                     1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE
#> y                     1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE
#> bandwidth_midpoint    1   6      NA    NA                    udunits  TRUE
#>                                        values x/y
#> x                                        NULL [x]
#> y                                        NULL [y]
#> bandwidth_midpoint 0.4825 [um],...,2.215 [um]    
# set bandwidth intervals:
(x3 = st_set_dimensions(x, "band", values = make_intervals(bw), names = "bandwidth"))
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif     1      54     69 68.91242      86  255
#> dimension(s):
#>           from  to  offset delta                     refsys point
#> x            1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE
#> y            1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE
#> bandwidth    1   6      NA    NA                    udunits    NA
#>                                           values x/y
#> x                                           NULL [x]
#> y                                           NULL [y]
#> bandwidth [0.45,0.515) [um],...,[2.08,2.35) [um]    
m = matrix(1:20, nrow = 5, ncol = 4)
dim(m) = c(x = 5, y = 4) # named dim
(s = st_as_stars(m))
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>     Min. 1st Qu. Median Mean 3rd Qu. Max.
#> A1     1    5.75   10.5 10.5   15.25   20
#> dimension(s):
#>   from to offset delta point x/y
#> x    1  5      0     1 FALSE [x]
#> y    1  4      0     1 FALSE [y]
st_get_dimension_values(s, 'x', where = "start")
#> [1] 0 1 2 3 4
st_get_dimension_values(s, 'x', center = FALSE)
#> [1] 0 1 2 3 4
st_get_dimension_values(s, 'x', where = "center")
#> [1] 0.5 1.5 2.5 3.5 4.5
st_get_dimension_values(s, 'x', center = TRUE)
#> [1] 0.5 1.5 2.5 3.5 4.5
st_get_dimension_values(s, 'x', where = "end")
#> [1] 1 2 3 4 5
st_get_dimension_values(s, 'x', max = TRUE)
#> [1] 1 2 3 4 5
```
