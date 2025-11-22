# get the raster type (if any) of a stars object

get the raster type (if any) of a stars object

## Usage

``` r
st_raster_type(x, dimension = character(0))
```

## Arguments

- x:

  object of class `stars`

- dimension:

  optional: numbers or names of dimension(s) to get per-dimension type

## Value

if `dimension` is not specified, return the spatial raster type: one of
`NA` (if the object does not have raster dimensions), `"curvilinear"`,
`"rectilinear"`, `"affine"`, or `"regular"`. In case dimension(s) are
specified, return one of `"regular"`, `"rectilinear"` (irregular but
numeric), or `"discrete"` (anything else).

## Details

categories `"curvilinear"` and `"affine"` only refer to the relationship
between a pair of spatial (raster) dimensions.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
st_raster_type(x)
#> [1] "regular"
st_raster_type(x, 1:3)
#>          x          y       band 
#>  "regular"  "regular" "discrete" 
```
