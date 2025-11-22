# Coerce stars object into a Raster raster or brick

Coerce stars object into a Raster raster or brick

Coerce stars object into a terra SpatRaster

## Arguments

- from:

  object to coerce

## Value

RasterLayer or RasterBrick

SpatRaster

## Details

If the stars object has more than three dimensions, all dimensions
higher than the third will be collapsed into the third dimensions. If
the stars object has only an x/y raster but multiple attributes, these
are merged first, then put in a raster brick.

If the stars object has more than three dimensions, all dimensions
higher than the third will be collapsed into the third dimensions. If
the stars object has only an x/y raster but multiple attributes, these
are merged first, then put in a SpatRaster.
