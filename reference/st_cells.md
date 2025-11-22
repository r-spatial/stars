# return the cell index corresponding to the location of a set of points

If the object has been cropped without normalization, then the indices
return are relative to the original uncropped extent. See
[`st_crop`](st_crop.md)

## Usage

``` r
st_cells(x, sf)
```

## Arguments

- x:

  object of class `stars`

- sf:

  object of class `sf` or `sfc`

## Examples

``` r
set.seed(1345)
st_bbox(L7_ETMs) |> 
  st_as_sfc() |> 
  st_sample(10) -> pts 
(x <- st_cells(L7_ETMs, pts))
#>  [1] 102496 112324  74862  56976 112862 105242  75593  11784  91245  63885
# get the pixel values (first band only):
st_as_stars(L7_ETMs)[[1]][x]
#>  [1]  92  92  89  76  67  94  77  98 113  56
# get pixel values for all bands:
st_as_stars(L7_ETMs) |> split() |> sapply(`[`, x)
#>        X1 X2  X3 X4  X5  X6
#>  [1,]  92 81  60 14  14  11
#>  [2,]  92 83  59 13  13  12
#>  [3,]  89 83  88 66 113  92
#>  [4,]  76 63  66 61 108  78
#>  [5,]  67 55  50 65  69  38
#>  [6,]  94 81  92 60 125 105
#>  [7,]  77 63  65 58 114  93
#>  [8,]  98 85  88 57  93  80
#>  [9,] 113 98 104 66 110  94
#> [10,]  56 40  27 68  50  24
# compare with st_extract():
st_as_stars(L7_ETMs) |> split() |> st_extract(pts)
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 289267.4 ymin: 9111549 xmax: 297166.8 ymax: 9119794
#> Projected CRS: SIRGAS 2000 / UTM zone 25S
#>     X1 X2  X3 X4  X5  X6                 geometry
#> 1   92 81  60 14  14  11 POINT (295582.2 9112406)
#> 2   92 83  59 13  13  12 POINT (297166.8 9111599)
#> 3   89 83  88 66 113  92 POINT (293775.8 9114659)
#> 4   76 63  66 61 108  78 POINT (291294.7 9116093)
#> 5   67 55  50 65  69  38 POINT (292609.8 9111549)
#> 6   94 81  92 60 125 105 POINT (294276.2 9112159)
#> 7   77 63  65 58 114  93   POINT (294731 9114598)
#> 8   98 85  88 57  93  80 POINT (296360.6 9119794)
#> 9  113 98 104 66 110  94 POINT (293206.5 9113313)
#> 10  56 40  27 68  50  24 POINT (289267.4 9115521)
```
