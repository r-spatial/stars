# build mosaic (composite) of several spatially disjoint stars objects

build mosaic (composite) of several spatially disjoint stars objects

## Usage

``` r
st_mosaic(.x, ...)

# S3 method for class 'stars'
st_mosaic(
  .x,
  ...,
  dst = tempfile(fileext = file_ext),
  options = c("-vrtnodata", "-9999", "-srcnodata", "nan"),
  file_ext = ".tif"
)

# S3 method for class 'character'
st_mosaic(
  .x,
  ...,
  dst = tempfile(fileext = file_ext),
  options = c("-vrtnodata", "-9999"),
  file_ext = ".tif"
)

# S3 method for class 'stars_proxy'
st_mosaic(
  .x,
  ...,
  dst = tempfile(fileext = file_ext),
  options = c("-vrtnodata", "-9999"),
  file_ext = ".tif"
)
```

## Arguments

- .x:

  object of class stars, or character vector with input dataset names

- ...:

  further input stars objects

- dst:

  character; destination file name; this will be a VRT file with
  references to the source file(s), see details

- options:

  character; options to the gdalbuildvrt command

- file_ext:

  character; file extension, determining the format used to write to
  (".tif" implies GeoTIFF)

## Value

the stars method returns a stars object with the composite of the input;
the `character` method returns the file name of the file with the
mosaic; see also the GDAL documentation of `gdalbuildvrt`

## Details

the gdal function buildvrt builds a mosaic of input images; these input
images can be multi-band, but not higher-dimensional data cubes or stars
objects with multiple attributes; note that for the \`stars\` method,
the \`dst\` file may contain references to temporary files that are
going to be removed at termination of the R session.

uses
[gdal_utils](https://r-spatial.github.io/sf/reference/gdal_utils.html)
to internally call `buildvrt`; no executables external to R are called.

## Examples

``` r
x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
x1 = x[,100:200,100:200,]
x2 = x[,150:300,150:300,]
plot(st_mosaic(x1, x2))
```
