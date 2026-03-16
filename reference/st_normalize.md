# Move raster offset such that grid start at row/col (1,1)

Move raster offset such that grid start at row/col (1,1)

## Usage

``` r
# S3 method for class 'stars_proxy'
st_normalize(x, domain = c(0, 0, 1, 1), ...)

# S3 method for class 'stars'
st_normalize(x, domain = c(0, 0, 1, 1), ...)
```

## Arguments

- x:

  object of class \`stars\` or \`stars_proxy\`

- domain:

  ignored

- ...:

  ignored

## Details

the \`stars_proxy\` method does nothing, but raises an error when a
raster does not start at cell (1,1)

the \`stars\` method "moves" the \`offset\` of the raster dimensions,
such that the \`from\` field becomes \`(1,1)\` for the raster
dimensions; as a side effect, it moves the raster dimensions \`x\` and
\`y\` to first and second position. If the \`from\` field is already
\`(1,1)\` or the object does not have raster dimensions it does nothing.
