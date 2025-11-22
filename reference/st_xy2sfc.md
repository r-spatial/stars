# replace x y raster dimensions with simple feature geometry list (points, or polygons = rasterize) and vice versa

replace x y raster dimensions with simple feature geometry list (points,
or polygons = rasterize) and vice versa

## Usage

``` r
st_xy2sfc(x, as_points, ..., na.rm = TRUE)

st_sfc2xy(x, ...)
```

## Arguments

- x:

  object of class `stars`, or of class `sf`

- as_points:

  logical; if `TRUE`, generate points at cell centers, else generate
  polygons

- ...:

  for \`st_xy2sfc\`: arguments passed on to `st_as_sfc`, for
  \`st_sfc2xy\` arguments passed on to
  [st_as_stars.data.frame](st_as_stars.md)

- na.rm:

  logical; omit (remove) cells which are entirely missing valued (across
  other dimensions)?

## Value

\`st_xy2sfc\` returns an object of class `stars` with x and y raster
dimensions replaced by a single sfc geometry list column containing
either points, or polygons. Adjacent cells with identical values are not
merged; see `st_rasterize` for this.

\`st_sfc2xy\` returns an object of class `stars` with a POINT list
replaced by X and Y raster dimensions. This only works when the points
are distributed over a regular or rectilinear grid.

## Details

\`st_xy2sfc\` replaces x y raster dimensions with simple feature
geometry list (points, or polygons = rasterize)

\`st_sfc2xy\` replaces POINT simple feature geometry list with an x y
raster

## Examples

``` r
(reduced_nc = read_ncdf(system.file("nc/reduced.nc", package = "stars")))
#> no 'var' specified, using sst, anom, err, ice
#> other available variables:
#>  lon, lat, zlev, time
#> 0-360 longitude crossing the international date line encountered.
#> Longitude coordinates will be 0-360 in output.
#> Will return stars object with 16200 cells.
#> No projection information found in nc file. 
#>  Coordinate variable units found to be degrees, 
#>  assuming WGS84 Lat/Lon.
#> stars object with 4 dimensions and 4 attributes
#> attribute(s):
#>                Min. 1st Qu. Median       Mean 3rd Qu.  Max.  NA's
#> sst [°C]      -1.80   -0.03 13.655 12.9940841 24.8125 32.97  4448
#> anom [°C]     -7.95   -0.58 -0.080 -0.1847324  0.2100  2.99  4449
#> err [°C]       0.11    0.16  0.270  0.2626872  0.3200  0.84  4448
#> ice [percent]  0.01    0.47  0.920  0.7178118  0.9600  1.00 13266
#> dimension(s):
#>      from  to         offset delta         refsys point values x/y
#> lon     1 180             -1     2 WGS 84 (CRS84)    NA   NULL [x]
#> lat     1  90            -90     2 WGS 84 (CRS84)    NA   NULL [y]
#> zlev    1   1             NA    NA             NA    NA      0    
#> time    1   1 1981-12-31 UTC    NA        POSIXct  TRUE   NULL    
(x = stars::st_xy2sfc(reduced_nc, as_points = TRUE, na.rm = FALSE))
#> stars object with 3 dimensions and 4 attributes
#> attribute(s):
#>                Min. 1st Qu. Median       Mean 3rd Qu.  Max.  NA's
#> sst [°C]      -1.80   -0.03 13.655 12.9940841 24.8125 32.97  4448
#> anom [°C]     -7.95   -0.58 -0.080 -0.1847324  0.2100  2.99  4449
#> err [°C]       0.11    0.16  0.270  0.2626872  0.3200  0.84  4448
#> ice [percent]  0.01    0.47  0.920  0.7178118  0.9600  1.00 13266
#> dimension(s):
#>          from    to         offset         refsys point
#> geometry    1 16200             NA WGS 84 (CRS84)  TRUE
#> zlev        1     1             NA             NA    NA
#> time        1     1 1981-12-31 UTC        POSIXct  TRUE
#>                                    values
#> geometry POINT (0 -89),...,POINT (358 89)
#> zlev                                    0
#> time                                 NULL
# roundtrip:
st_sfc2xy(x, dims=c("X", "Y","zlev","time"))
#> stars object with 4 dimensions and 4 attributes
#> attribute(s):
#>        Min. 1st Qu. Median       Mean 3rd Qu.  Max.  NA's
#> sst   -1.80   -0.03 13.655 12.9940841 24.8125 32.97  4448
#> anom  -7.95   -0.58 -0.080 -0.1847324  0.2100  2.99  4449
#> err    0.11    0.16  0.270  0.2626872  0.3200  0.84  4448
#> ice    0.01    0.47  0.920  0.7178118  0.9600  1.00 13266
#> dimension(s):
#>      from  to         offset delta         refsys x/y
#> X       1 180             -1     2 WGS 84 (CRS84) [x]
#> Y       1  90             90    -2 WGS 84 (CRS84) [y]
#> zlev    1   1              0    NA             NA    
#> time    1   1 1981-12-31 UTC    NA        POSIXct    
```
