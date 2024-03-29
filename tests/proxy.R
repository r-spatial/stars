Sys.setenv(TZ="UTC")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(r = read_stars(tif, proxy = TRUE))
if (interactive()) {
	plot(r)
}
dim(r)
r[,,,2:3]
r+r
st_as_stars(sin(r), downsample = 2)
aperm(r, c(3,2,1))
if (interactive()) {
	plot(r)
}
(xx = c(r,r))
st_redimension(xx)
st_as_stars(r)
(y = st_apply(r, 1:2, max))
(yy = st_as_stars(y, downsample = 1))
(y = adrop(st_apply(r, c("x", "y"), max)))
(yy = st_as_stars(y, downsample = 1))
plot(yy)
(y = adrop(st_apply(r, c("x", "band"), max)))
(yy = st_as_stars(y, downsample = 1))
rm(yy)

(xx = c(r,r))
names(xx) = c("a", "b")
xx["a"]
pt = st_point(c(x = 290462.103109179, y = 9114202.32594085))
buf = st_buffer(st_sfc(pt, crs = st_crs(r)), 1500)
buf = st_sfc(st_polygon(list(st_buffer(pt, 1500)[[1]], st_buffer(pt, 1000)[[1]])),
  crs = st_crs(r))
r = r[buf] # crops
r = r[buf, epsilon = 1e-5] # crops with shrinked bounding box

# c:
f = system.file("netcdf/avhrr-only-v2.19810902.nc", package = "starsdata")
if (FALSE && f != "") {
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
  	l[[f]] = read_stars(from, sub = c("sst", "anom"), proxy = TRUE)
  }
  ret = do.call(c, l)
  print(ret)
  all = system.file(paste0("netcdf/", files), package = "starsdata")
  ret = read_stars(all, sub = c("sst", "anom"))
  print(ret)
  print(st_redimension(ret)) # collapse the two attributes into new dimension

  try(ret <- c(l[[1]], l[[2]], l[[3]], along = list(times = as.Date("1981-09-01") + 0:2)))
  #print(ret)
  #ret = adrop(adrop(c(l[[1]], l[[2]], l[[3]], along = "times")))
  #print(ret)
  ret <- st_redimension(l[[1]], along = list(times = as.Date("1981-09-01") + 0:1))
  print(ret)
}

# demonstrate that environments work:
x = read_stars(tif, proxy = TRUE)
f = function(x) {
	g = function(xx) {
		ndvi = function(z1, z2, z3, z4, z5, z6) (z2-z1)/(z2+z1)
		st_apply(xx, 1:2, ndvi)
	}
	g(x)
}
(x = f(x))
st_as_stars(x)
