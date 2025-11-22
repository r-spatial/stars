# Read or write data using GDAL's multidimensional array API

Read or write data using GDAL's multidimensional array API

## Usage

``` r
read_mdim(
  filename,
  variable = character(0),
  ...,
  options = character(0),
  raster = NULL,
  offset = integer(0),
  count = integer(0),
  step = integer(0),
  proxy = FALSE,
  debug = FALSE,
  bounds = TRUE,
  curvilinear = NA,
  normalize_path = TRUE
)

write_mdim(
  x,
  filename,
  driver = detect.driver(filename),
  ...,
  root_group_options = character(0),
  options = character(0),
  as_float = TRUE,
  normalize_path = TRUE
)
```

## Arguments

- filename:

  name of the source or destination file or data source

- variable:

  name of the array to be read; if \`"?"\`, a list of array names is
  returned, with group name as list element names.

- ...:

  ignored

- options:

  character; driver specific options regarding the opening (read_mdim)
  or creation (write_mdim) of the dataset

- raster:

  names of the raster variables (default: first two dimensions)

- offset:

  integer; zero-based offset for each dimension (pixels) of sub-array to
  read, defaults to 0 for each dimension(requires sf \>= 1.0-9)

- count:

  integer; size for each dimension (pixels) of sub-array to read
  (default: read all); a value of NA will read the corresponding
  dimension entirely; counts are relative to the step size (requires sf
  \>= 1.0-9)

- step:

  integer; step size for each dimension (pixels) of sub-array to read;
  defaults to 1 for each dimension (requires sf \>= 1.0-9)

- proxy:

  logical; return proxy object?

- debug:

  logical; print debug info?

- bounds:

  logical or character: if `TRUE` tries to infer from "bounds"
  attribute; if character, named vector of the form
  `c(longitude="lon_bnds", latitude="lat_bnds")` with names dimension
  names

- curvilinear:

  control reading curvilinear (geolocation) coordinate arrays; if `NA`
  try reading the x/y dimension names; if character, defines the arrays
  to read; if `FALSE` do not try; see also [read_stars](read_stars.md)

- normalize_path:

  logical; if `FALSE`, suppress a call to
  [normalizePath](https://rdrr.io/r/base/normalizePath.html) on
  `filename`

- x:

  stars object

- driver:

  character; driver name

- root_group_options:

  character; driver specific options regarding the creation of the root
  group

- as_float:

  logical; if `TRUE` write 4-byte floating point numbers, if `FALSE`
  write 8-byte doubles

## Details

it is assumed that the first two dimensions are easting and northing

## See also

[gdal_utils](https://r-spatial.github.io/sf/reference/gdal_utils.html),
in particular util `mdiminfo` to query properties of a file or data
source containing arrays

## Examples

``` r
set.seed(135)
m = matrix(runif(10), 2, 5)
names(dim(m)) = c("stations", "time")
times = as.Date("2022-05-01") + 1:5
pts = st_as_sfc(c("POINT(0 1)", "POINT(3 5)"))
s = st_as_stars(list(Precipitation = m)) |>
 st_set_dimensions(1, values = pts) |>
 st_set_dimensions(2, values = times)
nc = tempfile(fileext=".nc")
if (compareVersion(sf_extSoftVersion()["GDAL"], "3.4.0") > -1) {
  write_mdim(s, nc)
  # try ncdump on the generated file
  print(read_mdim(nc))
}
#> Warning: GDAL Error 6: SetIndexingVariable() not implemented
#> Warning: GDAL Error 6: SetIndexingVariable() not implemented
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                      Min.   1st Qu.    Median      Mean  3rd Qu.      Max.
#> Precipitation  0.03524588 0.3224987 0.3772574 0.4289465 0.511113 0.9204841
#> dimension(s):
#>          from to     offset  delta refsys point                   values
#> stations    1  2         NA     NA     NA  TRUE POINT (0 1), POINT (3 5)
#> time        1  5 2022-05-02 1 days   Date    NA                     NULL
```
