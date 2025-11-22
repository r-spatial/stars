# obtain (spatial) resolution of a stars object

obtain resolution(s) of a stars object: by default only the (absolute)
x/y raster dimensions, optionally all `delta` dimension parameters

## Usage

``` r
st_res(x, all = FALSE, absolute = !all)
```

## Arguments

- x:

  an object of class `stars`

- all:

  logical; if FALSE return a vector with the x/y raster resolution

- absolute:

  logical; only works when `all = FALSE`; if TRUE return absolute
  resolution values, if FALSE return `delta` values

## Value

if `all = FALSE` a vector with x/y raster resolutions, otherwise a list
with delta values

## Examples

``` r
st_res(L7_ETMs)
#>    x    y 
#> 28.5 28.5 
st_res(L7_ETMs, absolute = FALSE)
#>     x     y 
#>  28.5 -28.5 
st_res(L7_ETMs, all = TRUE)
#> $x
#> [1] 28.5
#> 
#> $y
#> [1] -28.5
#> 
#> $band
#> [1] NA
#> 
if (require(starsdata)) {
  paste0("netcdf/", c("avhrr-only-v2.19810901.nc", 
    "avhrr-only-v2.19810902.nc",
    "avhrr-only-v2.19810903.nc",
    "avhrr-only-v2.19810904.nc")) |>
  system.file(package = "starsdata") |>
  read_stars(quiet = TRUE) -> x
  st_res(x) |> print()
  st_res(x, all = TRUE) |> print()
}
#> Loading required package: starsdata
#>    x    y 
#> 0.25 0.25 
#> $x
#> [1] 0.25
#> 
#> $y
#> [1] -0.25
#> 
#> $zlev
#> [1] NA
#> 
#> $time
#> [1] NA
#> 
```
