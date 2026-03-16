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
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif    12   60.75     74 72.51667      87  150
#> dimension(s):
#>          from to                     refsys point
#> geometry    1 10 SIRGAS 2000 / UTM zone 25S  TRUE
#> band        1  6                         NA    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> band                                                      NULL
st_extract(r, pnt) |> st_as_sf()
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 288950.3 ymin: 9111189 xmax: 298340.2 ymax: 9119338
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>    L7_ETMs.tif.V1 L7_ETMs.tif.V2 L7_ETMs.tif.V3 L7_ETMs.tif.V4 L7_ETMs.tif.V5
#> 1              97             88             67             14             13
#> 2              82             66             74             49            107
#> 3              66             54             46             73             79
#> 4              80             68             69             77            117
#> 5              87             85            104             87            120
#> 6              90             83             65             13             13
#> 7              63             46             38             65             83
#> 8             110            101            114             74            150
#> 9              80             68             74             54            110
#> 10             80             65             65             44             84
#>    L7_ETMs.tif.V6                 geometry
#> 1              12 POINT (298340.2 9114943)
#> 2              82 POINT (293918.4 9114415)
#> 3              42 POINT (293485.2 9118749)
#> 4              86 POINT (294440.9 9114839)
#> 5              79   POINT (295209 9118813)
#> 6              12 POINT (295048.9 9111189)
#> 7              50 POINT (289649.4 9116888)
#> 8             128 POINT (295730.3 9119338)
#> 9              88 POINT (288950.3 9111816)
#> 10             71 POINT (289531.4 9111471)
st_extract(r[,,,1], pnt)
#> Simple feature collection with 10 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 288950.3 ymin: 9111189 xmax: 298340.2 ymax: 9119338
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>    L7_ETMs.tif                 geometry
#> 1           97 POINT (298340.2 9114943)
#> 2           82 POINT (293918.4 9114415)
#> 3           66 POINT (293485.2 9118749)
#> 4           80 POINT (294440.9 9114839)
#> 5           87   POINT (295209 9118813)
#> 6           90 POINT (295048.9 9111189)
#> 7           63 POINT (289649.4 9116888)
#> 8          110 POINT (295730.3 9119338)
#> 9           80 POINT (288950.3 9111816)
#> 10          80 POINT (289531.4 9111471)
st_extract(r, st_coordinates(pnt)) # "at" is a matrix: return a matrix
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]   97   88   67   14   13   12
#>  [2,]   82   66   74   49  107   82
#>  [3,]   66   54   46   73   79   42
#>  [4,]   80   68   69   77  117   86
#>  [5,]   87   85  104   87  120   79
#>  [6,]   90   83   65   13   13   12
#>  [7,]   63   46   38   65   83   50
#>  [8,]  110  101  114   74  150  128
#>  [9,]   80   68   74   54  110   88
#> [10,]   80   65   65   44   84   71
# Extraction on non-POINT geometries
poly = st_buffer(pnt, 1000)
st_extract(r, poly)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> L7_ETMs.tif  12.22199 61.95172 70.34698 69.55859 81.08313 112.3657
#> dimension(s):
#>          from to                     refsys point
#> geometry    1 10 SIRGAS 2000 / UTM zone 25S FALSE
#> band        1  6                         NA    NA
#>                                                                 values
#> geometry POLYGON ((299340.2 911494...,...,POLYGON ((290531.4 911147...
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
#> X1    63   81.50  118.0 125.25   161.0  220
#> X2    46   68.00   96.5 108.60   136.0  202
#> X3    38   68.50   98.0 107.40   135.0  228
#> X4    13   47.75   75.5  82.50   113.5  174
#> X5    13   82.00  118.5 131.40   179.5  300
#> X6    12   48.00   85.0  97.50   146.0  256
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2026-03-15 1 days                       Date    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> date                                                      NULL

polysf = st_buffer(pntsf, 1000)
st_extract(split(rdate, "band"), polysf, time_column = "date") # POLYGON geometries
#> Simple feature collection with 10 features and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 287950.3 ymin: 9110189 xmax: 299340.2 ymax: 9120338
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>           X1        X2        X3        X4        X5        X6       date
#> 1   95.61855  87.97014  66.87460  13.91219  13.35652  12.22199 2026-03-15
#> 2  168.67356 145.05173 153.31092 133.18779 221.45680 171.79255 2026-03-16
#> 3   70.16835  59.61072  56.02305  77.30407  93.00078  58.88915 2026-03-15
#> 4  161.71081 138.51940 142.88153 141.05122 211.41852 157.77237 2026-03-16
#> 5   76.32885  66.93708  67.44537  77.82367 102.07587  68.78250 2026-03-15
#> 6  174.55556 155.56283 129.95569  68.87434  74.75926  53.56283 2026-03-16
#> 7   69.69241  57.50929  54.95356  66.84740  89.00159  59.73938 2026-03-15
#> 8  163.53261 144.08644 152.66253 148.69203 224.73137 167.08644 2026-03-16
#> 9   77.67062  62.33319  62.58754  53.40356  86.72064  66.59983 2026-03-15
#> 10 154.69930 124.51919 123.22393 107.94016 164.82381 124.12995 2026-03-16
#>                          geometry
#> 1  POLYGON ((299340.2 9114943,...
#> 2  POLYGON ((294918.4 9114415,...
#> 3  POLYGON ((294485.2 9118749,...
#> 4  POLYGON ((295440.9 9114839,...
#> 5  POLYGON ((296209 9118813, 2...
#> 6  POLYGON ((296048.9 9111189,...
#> 7  POLYGON ((290649.4 9116888,...
#> 8  POLYGON ((296730.3 9119338,...
#> 9  POLYGON ((289950.3 9111816,...
#> 10 POLYGON ((290531.4 9111471,...

vdc = st_sf(rdm = rnorm(20), polygons = st_buffer(st_sample(st_bbox(pnt), 20), 500),
      geometry = rep(pnt, 2), date = rep(dates, each = 10)) |> 
  st_as_stars(dims = c("geometry", "date"))

(vdc_new = st_extract(split(rdate, "band"), vdc)) # stars vector data cube
#> stars object with 2 dimensions and 6 attributes
#> attribute(s):
#>     Min. 1st Qu. Median   Mean 3rd Qu. Max.
#> X1    63   81.50  118.0 125.25   161.0  220
#> X2    46   68.00   96.5 108.60   136.0  202
#> X3    38   68.50   98.0 107.40   135.0  228
#> X4    13   47.75   75.5  82.50   113.5  174
#> X5    13   82.00  118.5 131.40   179.5  300
#> X6    12   48.00   85.0  97.50   146.0  256
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2026-03-15 1 days                       Date    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> date                                                      NULL
merge(vdc_new, name = "band")
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>                    Min. 1st Qu. Median    Mean 3rd Qu. Max.
#> X1.X2.X3.X4.X5.X6    12      68   94.5 108.775   148.5  300
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2026-03-15 1 days                       Date    NA
#> band        1  6         NA     NA                         NA    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> date                                                      NULL
#> band                                                 X1,...,X6

### Extraction applied to the geometries inside the vector data cube (cell values)
(vdc_new2 = st_extract(split(rdate, "band"), vdc,
             sfc_attribute = "polygons")) # stars vector data cube
#> stars object with 2 dimensions and 6 attributes
#> attribute(s):
#>         Min.  1st Qu.    Median      Mean  3rd Qu.     Max.
#> X1  64.05389 81.78454 114.71922 121.18932 160.0073 193.3864
#> X2  52.09223 71.41825  96.37767 103.37781 134.0551 177.9421
#> X3  44.29948 68.55390  95.45427 100.92211 136.0219 157.0072
#> X4  13.16426 63.60088  76.40195  89.18981 130.5063 160.7764
#> X5  13.76653 94.78568 118.90647 133.22952 193.5465 217.5130
#> X6  12.76550 75.00908  95.18119  98.57301 133.8600 168.5140
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2026-03-15 1 days                       Date    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> date                                                      NULL
merge(vdc_new2, name = "band")
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>                       Min.  1st Qu.   Median     Mean  3rd Qu.    Max.
#> X1.X2.X3.X4.X5.X6  12.7655 73.93376 102.7202 107.7469 143.4256 217.513
#> dimension(s):
#>          from to     offset  delta                     refsys point
#> geometry    1 10         NA     NA SIRGAS 2000 / UTM zone 25S  TRUE
#> date        1  2 2026-03-15 1 days                       Date    NA
#> band        1  6         NA     NA                         NA    NA
#>                                                         values
#> geometry POINT (298340.2 9114943),...,POINT (289531.4 9111471)
#> date                                                      NULL
#> band                                                 X1,...,X6
```
