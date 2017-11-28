library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = st_stars(tif))

#nc = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
#(x = st_stars(nc))
image(x)
gdal_crs(tif)
plot(x, col = grey((4:10)/10), names = NULL)

geomatrix = system.file("tif/geomatrix.tif", package = "stars")
x = st_stars(geomatrix)
y = st_transform(x, st_crs(4326))

