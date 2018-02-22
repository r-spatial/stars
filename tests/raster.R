suppressPackageStartupMessages(library(stars))

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))
(r = as(x, "Raster"))
(y = st_as_stars(r))

library(abind)
x = adrop(x[,,,1]) # 1 band
r = as(x, "Raster")
y = st_as_stars(r)
