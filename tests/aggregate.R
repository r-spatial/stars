library(stars)

if (require(spacetime, quietly = TRUE)) {
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_set_crs(st_as_sfc(stations), 4326), time = dates)

blocks = st_make_grid(st_as_sfc("POLYGON ((5.871619 47.26986, 15.03811 47.26986, 15.03811 55.05653, 5.871619 55.05653, 5.871619 47.26986))", crs = 4326),
	n = c(3,3))

print(aq <- st_as_stars(list(PM10 = air), dimensions = d))
print(a <- aggregate(aq, blocks, mean, na.rm = TRUE))
}

# adapted from ?read_stars:
m = array(1:720, dim = c(x = 10, y = 12, t = 6)) # named dim
st = st_as_stars(m)
attr(st, "dimensions")$y$delta = -1
attr(st, "dimensions")$y$offset = 12
tm = as.Date("2019-02-19") + 1:6
st = st_set_crs(st_set_dimensions(st, 3, values = tm), 4326)

tmp = tempfile(fileext = ".tif")
write_stars(st, tmp)

(red <- setNames(read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXsize = 10, nYSize = 12,
   nBufXSize = 2, nBufYSize = 2)), "foo"))

sfc = st_set_crs(st_as_sfc(red, as_points = FALSE), st_crs(st))
(a = aggregate(st, st_sf(a = 1, geom = sfc), mean))
(a = aggregate(st, sfc, mean))
(a = aggregate(st, sfc, mean, exact = TRUE))
a[[1]]
sum(a[[1]])*30 == sum(1:720)

tm0 = as.Date("2019-02-19") + -1:8
(a = aggregate(st, tm0, mean, na.rm = TRUE))
(a = aggregate(st, "days", mean, na.rm = TRUE))
yd = function(x) as.POSIXlt(x)$yday
(a = aggregate(st, yd, mean, na.rm = TRUE))

# with "by" geometry not overlapping x
pt = st_point(c(-10,-10))
(sfc = c(sfc, st_sfc(pt, crs = st_crs(sfc))))
(a = aggregate(st, sfc, mean))
(a = aggregate(st, st, mean))
