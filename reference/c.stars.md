# combine multiple stars objects, or combine multiple attributes in a single stars object into a single array

combine multiple stars objects, or combine multiple attributes in a
single stars object into a single array

## Usage

``` r
# S3 method for class 'stars_proxy'
c(
  ...,
  along = NA_integer_,
  along_crs = FALSE,
  try_hard = FALSE,
  nms = names(list(...)),
  tolerance = sqrt(.Machine$double.eps)
)

# S3 method for class 'stars'
c(
  ...,
  along = NA_integer_,
  try_hard = FALSE,
  nms = names(list(...)),
  tolerance = sqrt(.Machine$double.eps)
)
```

## Arguments

- ...:

  object(s) of class `star`: in case of multiple arguments, these are
  combined into a single stars object, in case of a single argument, its
  attributes are combined into a single attribute. In case of multiple
  objects, all objects should have the same dimensionality.

- along:

  integer; see [read_stars](read_stars.md)

- along_crs:

  logical; if `TRUE`, combine arrays along a CRS dimension

- try_hard:

  logical; if `TRUE` and some arrays have different dimensions, combine
  those that dimensions matching to the first array

- nms:

  character; vector with array names

- tolerance:

  numeric; values used in
  [all.equal](https://rdrr.io/r/base/all.equal.html) to compare
  dimension values combine those that dimensions matching to the first
  array

## Value

a single `stars` object with merged (binded) arrays.

## Details

An error is raised when attempting to combine arrays with different
measurement units into a single array. If this was intentded,
`drop_units` can be used to remove units of a `stars` object before
merging.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
(new = c(x, x))
#> stars object with 3 dimensions and 2 attributes
#> attribute(s):
#>                Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif       1      54     69 68.91242      86  255
#> L7_ETMs.tif.1     1      54     69 68.91242      86  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
c(new) # collapses two arrays into one with an additional dimension
#> stars object with 3 dimensions and 2 attributes
#> attribute(s):
#>                Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif       1      54     69 68.91242      86  255
#> L7_ETMs.tif.1     1      54     69 68.91242      86  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
c(x, x, along = 3)
#> stars object with 3 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>              Min. 1st Qu. Median    Mean 3rd Qu. Max.
#> L7_ETMs.tif    47      65     76 77.3419      87  255
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1  12      NA    NA                         NA    NA    
```
