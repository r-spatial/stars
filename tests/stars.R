suppressPackageStartupMessages(library(stars))
set.seed(13521) # runif
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x_ = read_stars(c(tif,tif))) # FIXME: not what you'd expect
(x = read_stars(tif))
image(x)
gdal_crs(tif)
plot(x)
plot(x, join_zlim = FALSE)

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
st_coordinates(x)[1:10,]

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

x = st_xy2sfc(read_stars(tif)[,1:10,1:10,], as_points = FALSE)
st_bbox(x)
x = read_stars(tif)
merge(split(x, "band"))

read_stars(c(tif,tif)) # merges as attributes
read_stars(c(tif,tif), along = "sensor")
read_stars(c(tif,tif), along = 4)
read_stars(c(tif,tif), along = "band")
read_stars(c(tif,tif), along = 3)

# cut:
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
cut(x, c(0, 50, 100, 255))
cut(x[,,,1,drop=TRUE], c(0, 50, 100, 255))
plot(cut(x[,,,1,drop=TRUE], c(0, 50, 100, 255)))

st_bbox(st_dimensions(x))
x[x < 0] = NA
x[is.na(x)] = 0

# c:
f = system.file("netcdf/avhrr-only-v2.19810902.nc", package = "starsdata")
if (f != "") {
  files = c("avhrr-only-v2.19810901.nc",
  "avhrr-only-v2.19810902.nc",
  "avhrr-only-v2.19810903.nc",
  "avhrr-only-v2.19810904.nc",
  "avhrr-only-v2.19810905.nc",
  "avhrr-only-v2.19810906.nc",
  "avhrr-only-v2.19810907.nc",
  "avhrr-only-v2.19810908.nc",
  "avhrr-only-v2.19810909.nc")
  l = list()
  for (f in files) {
	from = system.file(paste0("netcdf/", f), package = "starsdata")
  	l[[f]] = read_stars(from, sub = c("sst", "anom"))
  }
  ret = do.call(c, l)
  print(ret)
  ret = adrop(c(l[[1]], l[[2]], l[[3]], along = list(times = as.Date("1981-09-01") + 0:2)))
  print(ret)
  ret = adrop(adrop(c(l[[1]], l[[2]], l[[3]], along = "times")))
  print(ret)
}

