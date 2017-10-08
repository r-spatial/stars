library(stars)
#tif = system.file("tif/L7_ETMs.tif", package = "stars")
#(x = st_stars(tif))

nc = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
(x = st_stars(nc))
image(x)
