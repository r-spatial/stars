suppressPackageStartupMessages(library(stars))

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))

st_write(x, tempfile(), driver = "GTiff")
