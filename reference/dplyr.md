# dplyr verbs for stars objects

dplyr verbs for stars objects; package dplyr needs to be loaded before
these methods can be used for stars objects.

## Usage

``` r
filter.stars(.data, ...)

filter.stars_proxy(.data, ...)

mutate.stars(.data, ...)

mutate.stars_proxy(.data, ...)

transmute.stars(.data, ...)

transmute.stars_proxy(.data, ...)

select.stars(.data, ...)

select.stars_proxy(.data, ...)

rename.stars(.data, ...)

rename.stars_proxy(.data, ...)

pull.stars(.data, var = -1)

pull.stars_proxy(.data, ...)

as.tbl_cube.stars(x, ...)

slice.stars(.data, along, index, ..., drop = length(index) == 1)

slice.stars_proxy(.data, along, index, ...)

replace_na.stars(data, replace, ...)

replace_na.stars_proxy(data, ...)
```

## Arguments

- .data:

  object of class `stars`

- ...:

  see [filter](https://dplyr.tidyverse.org/reference/filter.html)

- var:

  see [pull](https://dplyr.tidyverse.org/reference/pull.html)

- x:

  object of class `stars`

- along:

  name or index of dimension to which the slice should be applied

- index:

  integer value(s) for this index

- drop:

  logical; drop dimensions that only have a single index?

- data:

  data set to work on

- replace:

  see
  [replace_na](https://tidyr.tidyverse.org/reference/replace_na.html):
  list with variable=value pairs, where value is the replacement value
  for NA's

## Examples

``` r
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x1 = read_stars(tif)
if (require(dplyr, quietly = TRUE)) {
 x1 %>% slice("band", 2:3)
 x1 %>% slice("x", 50:100)
}
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>              Min. 1st Qu. Median     Mean 3rd Qu. Max.
#> L7_ETMs.tif    11      56     68 70.50099      82  252
#> dimension(s):
#>      from  to  offset delta                     refsys point x/y
#> x      50 100  288776  28.5 SIRGAS 2000 / UTM zone 25S FALSE [x]
#> y       1 352 9120761 -28.5 SIRGAS 2000 / UTM zone 25S FALSE [y]
#> band    1   6      NA    NA                         NA    NA    
```
