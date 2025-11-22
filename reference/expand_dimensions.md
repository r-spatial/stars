# expand the dimension values into a list

expand the dimension values into a list

## Usage

``` r
expand_dimensions(x, ...)

# S3 method for class 'dimensions'
expand_dimensions(x, ..., max = FALSE, center = NA)
```

## Arguments

- x:

  object of class \`stars\` or \`dimensions\`

- ...:

  ignored

- max:

  logical; if \`TRUE\` return the max (end) values of the dimensions
  intervals

- center:

  logical; if \`TRUE\` return the center values of intervals, otherwise
  return offset (start) of intervals; if \`NA\` (default) return centers
  for x/y dimensions, offsets for all others
