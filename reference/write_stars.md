# write stars object to gdal dataset (typically: to file)

write stars object to gdal dataset (typically: to file)

## Usage

``` r
write_stars(obj, dsn, layer, ...)

# S3 method for class 'stars'
write_stars(
  obj,
  dsn,
  layer = 1,
  ...,
  driver = detect.driver(dsn),
  options = character(0),
  type = if (is.factor(obj[[1]]) && length(levels(obj[[1]])) < 256) "Byte" else "Float32",
  NA_value = NA_real_,
  update = FALSE,
  normalize_path = TRUE,
  scale_offset = c(1, 0)
)

# S3 method for class 'stars_proxy'
write_stars(
  obj,
  dsn,
  layer = 1,
  ...,
  driver = detect.driver(dsn),
  options = character(0),
  scale_offset = c(1, 0),
  type = "Float32",
  NA_value = NA_real_,
  chunk_size = c(dim(obj)[1], floor(2.5e+07/dim(obj)[1])),
  progress = TRUE
)

detect.driver(filename)
```

## Arguments

- obj:

  object of class `stars`

- dsn:

  gdal dataset (file) name

- layer:

  attribute name; if missing, the first attribute is written

- ...:

  passed on to
  [gdal_write](https://r-spatial.github.io/sf/reference/gdal.html)

- driver:

  driver driver name; see
  [st_drivers](https://r-spatial.github.io/sf/reference/st_drivers.html)

- options:

  character vector with dataset creation options, passed on to GDAL

- type:

  character; output binary type, one of: `Byte` for eight bit unsigned
  integer, `UInt16` for sixteen bit unsigned integer, `Int16` for
  sixteen bit signed integer, `UInt32` for thirty two bit unsigned
  integer, `Int32` for thirty two bit signed integer, `Float32` for
  thirty two bit floating point, `Float64` for sixty four bit floating
  point.

- NA_value:

  non-NA value that should represent R's `NA` value in the target raster
  file; if set to `NA`, it will be ignored.

- update:

  logical; if `TRUE`, an existing file is being updated

- normalize_path:

  logical; see [read_stars](read_stars.md)

- scale_offset:

  length 2 numeric vector with scale, offset values: raw values computed
  by raw = (value - offset) / scale are written to dsn; scale and offset
  values are written to dsn or else a warning is raised

- chunk_size:

  length two integer vector with the number of pixels (x, y) used in the
  read/write loop; see details.

- progress:

  logical; if `TRUE`, a progress bar is shown

- filename:

  character; used for guessing driver short name based on file
  extension; see examples

## Details

`write_stars` first creates the target file, then updates it
sequentially by writing blocks of `chunk_size`.

in case `obj` is a multi-file `stars_proxy` object, all files are
written as layers into the output file `dsn`

## Examples

``` r
detect.driver("L7_ETMs.tif")
#> [1] "GTiff"
```
