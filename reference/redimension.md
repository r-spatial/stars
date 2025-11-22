# redimension array, or collapse attributes into a new dimension

redimension array, or collapse attributes into a new dimension

## Usage

``` r
# S3 method for class 'stars_proxy'
st_redimension(
  x,
  new_dims = st_dimensions(x),
  along = list(new_dim = names(x)),
  ...
)

st_redimension(x, new_dims, along, ...)

# S3 method for class 'stars'
st_redimension(
  x,
  new_dims = st_dimensions(x),
  along = setNames(list(names(x)), name),
  ...,
  name = "new_dim"
)
```

## Arguments

- x:

  object of class `stars`

- new_dims:

  target dimensions: either a \`dimensions\` object or an integer vector
  with the dimensions' sizes

- along:

  named list with new dimension name and values

- ...:

  ignored

- name:

  character name of the new dimension
