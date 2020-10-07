# Create 'stars' object
set.seed(1331)
library(stars)
volcano = rbind(volcano, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # add NA rows
d = st_dimensions(x = 1:ncol(volcano), y = 1:nrow(volcano))
(r = st_as_stars(t(volcano)))
r = st_set_dimensions(r, 1, offset = 0, delta = 1)
r = st_set_dimensions(r, 2, offset = nrow(volcano), delta = -1)

# Create points
pnt = st_sample(st_as_sfc(st_bbox(r)), 100)
pnt = st_as_sf(pnt)

# Extract - 'st_join'
x = st_join(pnt, st_as_sf(r))

# Extract - 'st_extract'
y = st_extract(r, pnt)

# check there are NA's:
any(is.na(x))
# Compare
all.equal(x$A1, y[[1]])

## tic: segfaults
# check equal results with stars_proxy:
#x = st_extract(stars:::st_as_stars_proxy(r), pnt)
#all.equal(x$A1, y[[1]])
#all.equal(x, y)

r = c(r, 2*r, 10*r)
x = st_join(pnt, st_as_sf(r))
y = st_as_sf(st_extract(r, pnt))
all.equal(x, y)

## tic: segfaults
#x = st_extract(stars:::st_as_stars_proxy(merge(r)), pnt)
#all.equal(st_as_sf(x), y)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
xp = read_stars(tif, proxy = TRUE)
xm = read_stars(tif, proxy = FALSE)
pts = st_sample(st_as_sfc(st_bbox(xp)), 10)
pts = c(pts, st_as_sfc("POINT(0 0)"), pts)
em = st_extract(xm, pts)
if (utils::packageVersion("sf") >= "0.9-7") {
	ep = st_extract(xp, pts)
	print(all.equal(ep, em, check.attributes = TRUE))
}

# two-attribute objects:
library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
xp = read_stars(c(tif, tif), proxy = TRUE)
xm = read_stars(c(tif, tif), proxy = FALSE)
pts = st_sample(st_as_sfc(st_bbox(xp)), 10)
pts = c(pts, st_as_sfc("POINT(0 0)"), pts)
em = st_extract(xm, pts)
if (utils::packageVersion("sf") >= "0.9-7") {
	ep = st_extract(xp, pts)
	print(all.equal(ep, em, check.attributes = TRUE))
}

# single-attribute, single raster objects:
tif1 = paste0(tempfile(), ".tif")
write_stars(xm[1,,,1], "x.tif")
xp = read_stars("x.tif", proxy = TRUE)
xm = read_stars("x.tif", proxy = FALSE)
em = st_extract(xm, pts)
if (utils::packageVersion("sf") >= "0.9-7") {
	ep = st_extract(xp, pts)
	print(all.equal(ep, em, check.attributes = TRUE))
}

# multiple-file attributes:
x = c(
"avhrr-only-v2.19810901.nc",
"avhrr-only-v2.19810902.nc",
"avhrr-only-v2.19810903.nc",
"avhrr-only-v2.19810904.nc",
"avhrr-only-v2.19810905.nc",
"avhrr-only-v2.19810906.nc",
"avhrr-only-v2.19810907.nc",
"avhrr-only-v2.19810908.nc",
"avhrr-only-v2.19810909.nc"
)
file_list = system.file(paste0("netcdf/", x), package = "starsdata")
(y = read_stars(file_list, quiet = TRUE))
st_crs(y) = "OGC:CRS84"
pts = st_sample(st_as_sfc(st_bbox(y)), 10)
em = st_extract(y, pts)

(y = read_stars(file_list, quiet = TRUE, proxy = TRUE))
st_crs(y) = "OGC:CRS84"
if (utils::packageVersion("sf") >= "0.9-7") {
	ep = st_extract(y, pts)
	print(all.equal(em, ep))
}

# nearest & bilinear comparison:
if (utils::packageVersion("sf") >= "0.9-7") {
  set.seed(12331)
  s = st_as_stars(matrix(rnorm(16), 4))
  pts = st_sample(st_as_sfc(st_bbox(s)), 10000, type = "regular")
  s1 = st_extract(s, pts, bilinear = FALSE)
  s2 = st_extract(s, pts, bilinear = TRUE)
  s1$s2 = s2[[1]]
  names(s1)[c(1,3)] = c("nearest", "bilinear")
  print(s1[sample(10000, 5),])
}
