# print stars or dimensions object

print stars or dimensions object

## Usage

``` r
# S3 method for class 'dimensions'
as.data.frame(
  x,
  ...,
  digits = max(3, getOption("digits") - 3),
  usetz = TRUE,
  stars_crs = getOption("stars.crs") %||% 28,
  all = FALSE
)

# S3 method for class 'dimensions'
print(x, ...)

# S3 method for class 'stars'
print(x, ..., n = 1e+05, abbrev = 30)
```

## Arguments

- x:

  object of class stars or of class dimensions

- ...:

  passed on to `as.data.frame.dimensions`

- digits:

  number of digits to print numbers

- usetz:

  logical; used to format `POSIXct` values

- stars_crs:

  maximum width of string for CRS objects

- all:

  logical; if `TRUE` print also fields entirely filled with `NA` or
  `NULL`

- n:

  when prod(dim(x)) \> 10 \* n, the first n cells are used for attribute
  summary statistics

- abbrev:

  number of characters to abbreviate attribute names to
