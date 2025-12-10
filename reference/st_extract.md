# Extract cell values at point locations

Extract cell values at point locations

## Usage

``` r
st_extract(x, ...)

# S3 method for class 'stars'
st_extract(
  x,
  at,
  ...,
  bilinear = FALSE,
  time_column = attr(at, "time_column") %||% attr(at, "time_col"),
  interpolate_time = bilinear,
  FUN = mean,
  resampling = c("nearest", "bilinear", "cubic", "cubicspline"),
  sfc_attribute = NULL
)
```

## Arguments

- x:

  object of class `stars` or `stars_proxy`

- ...:

  passed on to [aggregate.stars](aggregate.stars.md) when geometries are
  not exclusively POINT geometries

- at:

  object of class `sf` or `sfc` with geometries, or two-column matrix
  with coordinate points in rows, indicating where to extract values of
  `x`, or a `stars` object with geometry and temporal dimensions (vector
  data cube)

- bilinear:

  logical; use bilinear interpolation rather than nearest neighbour?

- time_column:

  character or integer; name or index of a column with time or date
  values that will be matched to values of the first temporal dimension
  (matching classes `POSIXct`, `POSIXt`, `Date`, or `CFTime`), in `x`,
  after which this dimension is reduced. This is useful to extract data
  cube values along a trajectory; see
  https://github.com/r-spatial/stars/issues/352 .

- interpolate_time:

  logical; should time be interpolated? if FALSE, time instances are
  matched using the coinciding or the last preceding time in the data
  cube.

- FUN:

  function used to aggregate pixel values when geometries of `at`
  intersect with more than one pixel

- resampling:

  character; resampling method; for method cubic or cubicspline,
  \`stars_proxy\` objects should be used and GDAL should have version
  \>= 3.10.0

- sfc_attribute:

  character; if `at` is of class `stars` should the aggregation be
  performed for the attribute geometry rather than the dimension
  geometry? If `NULL` (default), the aggregation is performed at the
  dimension geometries, else the name of the attribute geometry to
  perform the aggregation on. If the given attribute geometry does not
  exist, the aggregation defaults to the dimension geometry.

## Value

if `at` is of class `matrix`, a matrix with extracted values is
returned; if `at` is of class `stars` and a temporal dimension was
passed to `time_column`, a `stars` object with the original `at`
dimensions and the extracted values as attributes. otherwise: if `x` has
more dimensions than only x and y (raster), an object of class `stars`
with POINT geometries replacing x and y raster dimensions, if this is
not the case, an object of `sf` with extracted values.

## Details

points outside the raster are returned as `NA` values. For large sets of
points for which extraction is needed, passing a matrix as to `at` may
be much faster than passing an `sf` or `sfc` object.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
r = read_stars(tif)
pnt = st_sample(st_as_sfc(st_bbox(r)), 10)
st_extract(r, pnt)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median  Mean 3rd Qu. Max.
#> L7_ETMs.tif    13   51.75     68 65.25   79.25  112
#> dimension(s):
#>          from to                     refsys point
#> geometry    1 10 SIRGAS 2000 / UTM zone 25S  TRUE
#> band        1  6                         NA    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> band                                                      NULL
st_extract(r, pnt) %>% st_as_sf()
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 289232.3 ymin: 9111273 xmax: 297312.4 ymax: 9120157
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>    L7_ETMs.tif.V1 L7_ETMs.tif.V2 L7_ETMs.tif.V3 L7_ETMs.tif.V4 L7_ETMs.tif.V5
#> 1              84             67             73             51            102
#> 2              63             50             41             80             81
#> 3              73             66             60            103             90
#> 4              61             52             31            108             80
#> 5              79             69             73             71             97
#> 6              56             43             29             73             56
#> 7              74             68             62             85            104
#> 8              95             85             60             13             13
#> 9              62             48             34             79             73
#> 10             78             68             71             74            112
#>    L7_ETMs.tif.V6                 geometry
#> 1              84 POINT (292954.5 9111273)
#> 2              46 POINT (292431.3 9118384)
#> 3              52 POINT (296727.8 9119773)
#> 4              38 POINT (292851.5 9115697)
#> 5              68 POINT (296791.7 9114387)
#> 6              26 POINT (289232.3 9119293)
#> 7              58 POINT (294882.6 9114838)
#> 8              13 POINT (297312.4 9111919)
#> 9              35 POINT (289854.7 9120157)
#> 10             75 POINT (289511.7 9117396)
st_extract(r[,,,1], pnt)
#> Simple feature collection with 10 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 289232.3 ymin: 9111273 xmax: 297312.4 ymax: 9120157
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>    L7_ETMs.tif                 geometry
#> 1           84 POINT (292954.5 9111273)
#> 2           63 POINT (292431.3 9118384)
#> 3           73 POINT (296727.8 9119773)
#> 4           61 POINT (292851.5 9115697)
#> 5           79 POINT (296791.7 9114387)
#> 6           56 POINT (289232.3 9119293)
#> 7           74 POINT (294882.6 9114838)
#> 8           95 POINT (297312.4 9111919)
#> 9           62 POINT (289854.7 9120157)
#> 10          78 POINT (289511.7 9117396)
st_extract(r, st_coordinates(pnt)) # "at" is a matrix: return a matrix
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]   84   67   73   51  102   84
#>  [2,]   63   50   41   80   81   46
#>  [3,]   73   66   60  103   90   52
#>  [4,]   61   52   31  108   80   38
#>  [5,]   79   69   73   71   97   68
#>  [6,]   56   43   29   73   56   26
#>  [7,]   74   68   62   85  104   58
#>  [8,]   95   85   60   13   13   13
#>  [9,]   62   48   34   79   73   35
#> [10,]   78   68   71   74  112   75
# Extraction on non-POINT geometries
poly = st_buffer(pnt, 1000)
st_extract(r, poly)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> L7_ETMs.tif  12.61357 54.78787 69.76618 66.72112 78.81806 107.9066
#> dimension(s):
#>          from to                     refsys point
#> geometry    1 10 SIRGAS 2000 / UTM zone 25S FALSE
#> band        1  6                         NA    NA
#>                                                                 values
#> geometry POLYGON ((293954.5 911127...,...,POLYGON ((290511.7 911739...
#> band                                                              NULL

# Extraction with time matching
rdate = c(r, r*2, along = "date")
dates = c(Sys.Date()-1, Sys.Date())
rdate = st_set_dimensions(rdate, "date", values = c(dates))

pntsf = st_sf(date = dates, geometry = pnt)
st_extract(split(rdate, "band"), pntsf) # POINT geometries
#> stars object with 2 dimensions and 6 attributes
#> attribute(s):
#>     Min. 1st Qu. Median   Mean 3rd Qu. Max.
#> X1    56   73.75  103.5 108.75   146.5  190
#> X2    43   66.75   85.5  92.40   132.5  170
#> X3    29   59.50   69.5  80.10   120.0  146
#> X4    13   73.75  102.5 110.55   150.5  216
#> X5    13   80.75  108.0 121.20   166.5  224
#> X6    13   44.00   69.0  74.25    95.0  168
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2025-12-09 1 days                       Date    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> date                                                      NULL

polysf = st_buffer(pntsf, 1000)
st_extract(split(rdate, "band"), polysf, time_column = "date") # POLYGON geometries
#> Simple feature collection with 10 features and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 288232.3 ymin: 9110273 xmax: 298312.4 ymax: 9121157
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>           X1        X2        X3        X4        X5        X6       date
#> 1   86.74235  73.34041  78.95659  59.57558 107.90662  86.68551 2025-12-09
#> 2  128.05442 104.12335  87.59834 154.64110 156.04872  86.64680 2025-12-10
#> 3   80.84317  69.51889  73.68789  68.72257 102.97050  79.01656 2025-12-09
#> 4  143.92547 121.26812 117.35352 141.02174 184.27640 120.74896 2025-12-10
#> 5   84.99715  73.55567  70.69834  51.76877  78.77188  60.54195 2025-12-09
#> 6  126.98972 100.18972  83.01095 146.08226 144.47297  79.95821 2025-12-10
#> 7   80.71654  68.98010  71.40930  69.38656 105.59302  79.18656 2025-12-09
#> 8  192.11344 175.69852 129.11318  27.31106  27.26237  25.22714 2025-12-10
#> 9   64.81101  51.57237  44.47277  71.82245  77.79326  46.09780 2025-12-09
#> 10 134.47686 110.56999 102.17447 140.02693 169.27237 106.59299 2025-12-10
#>                          geometry
#> 1  POLYGON ((293954.5 9111273,...
#> 2  POLYGON ((293431.3 9118384,...
#> 3  POLYGON ((297727.8 9119773,...
#> 4  POLYGON ((293851.5 9115697,...
#> 5  POLYGON ((297791.7 9114387,...
#> 6  POLYGON ((290232.3 9119293,...
#> 7  POLYGON ((295882.6 9114838,...
#> 8  POLYGON ((298312.4 9111919,...
#> 9  POLYGON ((290854.7 9120157,...
#> 10 POLYGON ((290511.7 9117396,...

vdc = st_sf(rdm = rnorm(20), polygons = st_buffer(st_sample(st_bbox(pnt), 20), 500),
      geometry = rep(pnt, 2), date = rep(dates, each = 10)) |> 
  st_as_stars(dims = c("geometry", "date"))

(vdc_new = st_extract(split(rdate, "band"), vdc)) # stars vector data cube
#> stars object with 2 dimensions and 6 attributes
#> attribute(s):
#>     Min. 1st Qu. Median   Mean 3rd Qu. Max.
#> X1    56   73.75  103.5 108.75   146.5  190
#> X2    43   66.75   85.5  92.40   132.5  170
#> X3    29   59.50   69.5  80.10   120.0  146
#> X4    13   73.75  102.5 110.55   150.5  216
#> X5    13   80.75  108.0 121.20   166.5  224
#> X6    13   44.00   69.0  74.25    95.0  168
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2025-12-09 1 days                       Date    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> date                                                      NULL
merge(vdc_new, name = "band")
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>                    Min. 1st Qu. Median   Mean 3rd Qu. Max.
#> X1.X2.X3.X4.X5.X6    13      62   84.5 97.875     136  224
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2025-12-09 1 days                       Date    NA
#> band        1  6         NA     NA                         NA    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> date                                                      NULL
#> band                                                 X1,...,X6

### Extraction applied to the geometries inside the vector data cube (cell values)
(vdc_new2 = st_extract(split(rdate, "band"), vdc,
             sfc_attribute = "polygons")) # stars vector data cube
#> stars object with 2 dimensions and 6 attributes
#> attribute(s):
#>         Min.  1st Qu.    Median      Mean  3rd Qu.     Max.
#> X1  69.40166 79.43934 109.04942 115.84515 149.1752 198.9679
#> X2  57.68841 66.33621  90.94643  98.17146 122.4885 186.0393
#> X3  54.82225 69.63102  83.03128  93.11446 123.9066 147.0114
#> X4  26.95868 62.63331  72.92654  88.81582 119.3498 157.1760
#> X5  26.96694 89.27355 109.30090 127.02705 180.5688 214.6674
#> X6  24.62397 60.09940  83.82858  91.14182 118.5457 176.8760
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2025-12-09 1 days                       Date    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> date                                                      NULL
merge(vdc_new2, name = "band")
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>                        Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> X1.X2.X3.X4.X5.X6  24.62397 69.45698 88.30459 102.3526 133.1578 214.6674
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2025-12-09 1 days                       Date    NA
#> band        1  6         NA     NA                         NA    NA
#>                                                         values
#> geometry POINT (292954.5 9111273),...,POINT (289511.7 9117396)
#> date                                                      NULL
#> band                                                 X1,...,X6
```
