# Compute or plot contour lines or sets

Compute contour lines or sets

## Usage

``` r
st_contour(
  x,
  na.rm = TRUE,
  contour_lines = FALSE,
  breaks = classInt::classIntervals(na.omit(as.vector(x[[1]])))$brks
)
```

## Arguments

- x:

  object of class `stars`

- na.rm:

  logical; should missing valued cells be removed, or also be converted
  to features?

- contour_lines:

  logical; if `FALSE`, polygons are returned (contour sets), otherwise
  contour lines

- breaks:

  numerical; values at which to "draw" contour levels

## Details

this function requires GDAL \>= 2.4.0

## See also

for polygonizing rasters following grid boundaries, see
[st_as_sf](st_as_sf.md) with arguments `as_points=FALSE` and
`merge=TRUE`; [contour](https://rdrr.io/r/graphics/contour.html) plots
contour lines using R's native algorithm (which also plots contour
levels)
