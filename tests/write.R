library(stars)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = st_stars(tif))

st_write(x, tempfile(), driver = "GTiff")
