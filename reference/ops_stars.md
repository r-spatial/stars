# S3 Ops Group Generic Functions for stars objects

Ops functions for stars objects, including comparison, product and
divide, add, subtract

## Usage

``` r
# S3 method for class 'stars'
Ops(e1, e2)

# S3 method for class 'stars'
Math(x, ...)

# S3 method for class 'stars_proxy'
Ops(e1, e2)

# S3 method for class 'stars_proxy'
Math(x, ...)
```

## Arguments

- e1:

  object of class `stars`

- e2:

  object of class `stars`

- x:

  object of class stars

- ...:

  parameters passed on to the Math functions

## Value

object of class `stars`

## Details

if `e1` or `e2` is is a numeric vector, or `e2` has less or smaller
dimensions than `e1`, then `e2` is recycled such that it fits `e1`,
using usual R array recycling rules. The user needs to make sure this is
sensible; it may be needed to use `aperm` to permutate dimensions first.

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
x * x
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median    Mean 3rd Qu.  Max.
#> L7_ETMs.tif     1    2916   4761 5512.41    7396 65025
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
x / x
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median Mean 3rd Qu. Max.
#> L7_ETMs.tif     1       1      1    1       1    1
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
x + x
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif     2     108    138 137.8248     172  510
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
x + 10
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif    11      64     79 78.91242      96  265
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x       1 349  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
all.equal(x * 10, 10 * x)
#> [1] TRUE
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
a = sqrt(x)
b = log(x, base = 10)
```
