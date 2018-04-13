suppressPackageStartupMessages(library(stars))
set.seed(13521) # runif
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x_ = read_stars(c(tif,tif))) # FIXME: not what you'd expect
(x = read_stars(tif))
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
x = read_stars(geomatrix)
y = st_transform(x, st_crs(4326))

nc = system.file("nc/tos_O1_2001-2002.nc", package = "stars")
(x = read_stars(nc))
st_as_stars(st_bbox(x))
df = as.data.frame(x)

st_as_stars()

dimnames(x)
dimnames(x) <- letters[1:3]
dimnames(x)

# multiple sub-datasets:
nc_red = system.file("nc/reduced.nc", package = "stars")
(red = read_stars(nc_red))
plot(red)

x = st_xy2sfc(read_stars(tif)[,1:10,1:10,])
st_bbox(x)
x = read_stars(tif)
merge(split(x, "band"))

read_stars(c(tif,tif)) # merges as attributes
read_stars(c(tif,tif), along = "sensor")
read_stars(c(tif,tif), along = 4)
read_stars(c(tif,tif), along = "band")
read_stars(c(tif,tif), along = 3)
