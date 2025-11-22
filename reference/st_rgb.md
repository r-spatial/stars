# reduce dimension to rgb (alpha) hex values

reduce dimension to rgb (alpha) hex values

## Usage

``` r
st_rgb(
  x,
  dimension = 3,
  use_alpha = dim(x)[dimension] == 4,
  maxColorValue = 255L,
  probs = c(0, 1),
  stretch = NULL
)
```

## Arguments

- x:

  object of class `stars`

- dimension:

  dimension name or number to reduce

- use_alpha:

  logical; if TRUE, the fourth band will be used as alpha values

- maxColorValue:

  integer; maximum value for colors

- probs:

  probability values for quantiles used for stretching by "percent".

- stretch:

  logical or character; if `TRUE` or `"percent"`, each band is stretched
  to 0 ... maxColorValue by "percent clip" method using probs values. If
  `"histogram"`, a "histogram equalization" is performed (`probs` values
  are ignored). If stretch is `NULL` or `FALSE`, no stretching is
  performed. Other character values are interpreted as "percent" and a
  message will be printed.

## Details

the dimension's bands are mapped to red, green, blue, alpha; if a
different ordering is wanted, use [\[.stars](stars_subset.md) to reorder
a dimension, see examples. Alternatively, you can use
[plot.stars](plot.md) with the `rgb` argument to create a three-band
composition.

## See also

[st_apply](st_apply.md), [rgb](https://rdrr.io/r/grDevices/rgb.html)

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
st_rgb(x[,,,3:1])
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>  L7_ETMs.tif       
#>  Length:122848     
#>  Class :character  
#>  Mode  :character  
#> dimension(s):
#>   from  to  offset delta                     refsys point x/y
#> x    1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y    1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
r = st_rgb(x[,,,c(6,5,4,3)], 3, use_alpha=TRUE) # now R=6,G=5,B=4,alpha=3
if (require(ggplot2)) {
 ggplot() + geom_stars(data = r) + scale_fill_identity()
}

r = st_rgb(x[,,,3:1],
       probs = c(0.01, 0.99),
       stretch = "percent")
plot(r)

r = st_rgb(x[,,,3:1],
       probs = c(0.01, 0.99),
       stretch = "histogram")
plot(r)
```
