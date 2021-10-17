options(rgdal_show_exportToProj4_warnings = "none")
suppressPackageStartupMessages(library(stars))

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))
(r = as(x, "Raster"))
(y = st_as_stars(r))

# single band:
x = adrop(x[,,,1])
r = as(x, "Raster")
(y = st_as_stars(r))

# proxy:
(x = read_stars(tif, proxy = TRUE))
(r = as(x, "Raster"))
(y = st_as_stars(r))

## terra -------------
(x = read_stars(tif))
(r = as(x, "SpatRaster"))
(y = st_as_stars(r))

# single band:
x = adrop(x[,,,1])
r = as(x, "SpatRaster")
(y = st_as_stars(r))

# proxy:
(x = read_stars(tif, proxy = TRUE))
(r = as(x, "SpatRaster"))
(y = st_as_stars(r))
