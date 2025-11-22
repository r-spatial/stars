# Spatially join a stars and an \`sf\` object

Spatially join a stars and an \`sf\` object

## Usage

``` r
# S3 method for class 'stars'
st_join(
  x,
  y,
  join = st_intersects,
  ...,
  what = "left1",
  as_points = NA,
  warn = TRUE
)
```

## Arguments

- x:

  object of class stars

- y:

  object of class sf, or one that can be coerced into that by
  [st_as_sf](st_as_sf.md)

- join:

  the join function, which should return an sgbp object; see details

- ...:

  arguments that will be passed on to the join function

- what:

  "left1", "right" or "inner"; see details

- as_points:

  logical; controls whether grid cells in `x` will be treated as points,
  or as cell areas; the [st_intersects.stars](st_intersects.stars.md)
  method by default will derive this from `x`'s metadata, or else assume
  areas.

- warn:

  logical; if TRUE, warn on 1-to-many matches when `what` is `"left1"`

## Value

If what is "left1", an object of class stars with the (first) value of y
at spatial instances of x

## Details

When there is more than one match to a single x value, the first
matching record from y is taken (and if `warn` is TRUE a warning is
raised). If what is "inner", an object of class `sf` with all matching
records of x and y.
