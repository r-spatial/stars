# Coerce stars object into a RasterLayer, RasterBrick, or SpatRaster

Coerce stars object into a RasterLayer, RasterBrick, or SpatRaster

## Usage

``` r
# S4 method for class 'stars,Raster'
coerce(from, to = "Raster", strict = TRUE)

# S4 method for class 'stars,SpatRaster'
coerce(from, to = "SpatRaster", strict = TRUE)
```

## Arguments

- from:

  object of class \`stars\`

- to:

  to object

- strict:

  logical

## Value

RasterLayer or RasterBrick

SpatRaster

## Details

If the stars object has more than three dimensions, all dimensions
higher than the third will be collapsed into the third dimensions. If
the stars object has only an x/y raster but multiple attributes, these
are merged first, then put in a raster brick or SpatRaster.
