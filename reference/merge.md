# merge or split stars object

merge attributes into a dimension, or split a dimension over attributes

## Usage

``` r
# S3 method for class 'stars'
split(x, f = length(dim(x)), drop = TRUE, ...)

# S3 method for class 'stars'
merge(x, y, ..., name = "attributes")
```

## Arguments

- x:

  object of class `stars`

- f:

  the name or index of the dimension to split; by default the last
  dimension

- drop:

  ignored

- ...:

  if defined, the first unnamed argument is used for dimension values,
  if not defined, attribute names are used for dimension values

- y:

  needs to be missing

- name:

  name for the new dimension

## Value

merge merges attributes of a stars object into a new dimension; split
splits a dimension over attributes

## Details

split.stars works on the first attribute, and will give an error when
more than one attribute is present
