suppressPackageStartupMessages(library(stars))
set.seed(13521) # runif
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x_ = read_stars(c(tif,tif))) # FIXME: not what you'd expect
(x = read_stars(tif))
image(x)
gdal_crs(tif)
plot(x)
plot(x, join_zlim = FALSE)
x %>% st_set_dimensions(names = c('a', 'b', 'c'))

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

if (version$os == "linux-gnu") { # FIXME: breaks on windows
 nc = system.file("nc/tos_O1_2001-2002.nc", package = "stars")
 print(x = read_stars(nc))
 print(st_as_stars(st_bbox(x)))
 print(st_as_stars(st_bbox(x), deltax = 20, deltay = 20))
 df = as.data.frame(x)

 print(dimnames(x))
 dimnames(x) <- letters[1:3]
 print(dimnames(x))
}
print(st_as_stars())

# multiple sub-datasets:
if (version$os == "linux-gnu") { # FIXME: breaks on windows
  nc_red = system.file("nc/reduced.nc", package = "stars")
  red = read_stars(nc_red)
  print(red)
  plot(red)
}

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

st_dimensions(list(matrix(1, 4,4))) # st_dimensions.default

if (FALSE && require("starsdata")) {
  # curvilinear:
  s5p = system.file(
      "sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc",
      package = "starsdata")
  print(s5p)
  lat_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/latitude")
  lon_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/longitude")
  nit_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column")
  lat = read_stars(lat_ds)
  lon = read_stars(lon_ds)
  nit = read_stars(nit_ds)
  nit[[1]][nit[[1]] > 9e+36] = NA
  
  ll = setNames(c(lon, lat), c("x", "y"))
  nit.c = st_as_stars(nit, curvilinear = ll)
  print(nit.c)

  s5p = system.file(
      "sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc",
      package = "starsdata")
  nit.c2 = read_stars(s5p, 
  	sub = "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column",
    curvilinear = c("//PRODUCT/latitude", "//PRODUCT/longitude"))
  print(all.equal(nit.c, nit.c2))
}
