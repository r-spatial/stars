library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = st_stars(tif))

#nc = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
#(x = st_stars(nc))
image(x)
gdal_crs(tif)
plot(x)

x + x
x * x
x[,,,1:3]
x[,1:100,100:200,]
sqrt(x)
st_apply(x, 3, min)
st_apply(x, 1:2, max)
st_apply(x, 1:2, range)

geomatrix = system.file("tif/geomatrix.tif", package = "stars")
x = st_stars(geomatrix)
y = st_transform(x, st_crs(4326))
