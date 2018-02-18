library(stars)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = st_stars(tif))
(r = as(x, "Raster"))
(y = st_as_stars(r))
