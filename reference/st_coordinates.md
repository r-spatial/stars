# retrieve coordinates for raster or vector cube cells

retrieve coordinates for raster or vector cube cells

## Usage

``` r
# S3 method for class 'stars'
st_coordinates(x, ..., add_max = FALSE, center = TRUE)

# S3 method for class 'stars'
as.data.frame(x, ..., add_max = FALSE, center = NA, add_coordinates = TRUE)

as_tibble.stars(.x, ..., add_max = FALSE, center = NA)
```

## Arguments

- x:

  object of class `stars`

- ...:

  ignored

- add_max:

  logical; if `TRUE`, dimensions are given with a min (x) and max
  (x_max) value

- center:

  logical; (only if `add_max` is FALSE): should grid cell center
  coordinates be returned (TRUE) or offset values (FALSE)? `center` can
  be a named logical vector or list to specify values for each
  dimension.

- add_coordinates:

  logical; if \`TRUE\`, columns with dimension values preceed the array
  values, otherwise they are omitted

- .x:

  object to be converted to a tibble
