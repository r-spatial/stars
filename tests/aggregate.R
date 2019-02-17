suppressPackageStartupMessages(library(stars))

library(spacetime)
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_as_sfc(stations), time = dates)

blocks = st_make_grid(st_as_sfc("POLYGON ((5.871619 47.26986, 15.03811 47.26986, 15.03811 55.05653, 5.871619 55.05653, 5.871619 47.26986))", crs = 4326),
	n = c(3,3))

(aq = st_as_stars(list(PM10 = air), dimensions = d))
(a = aggregate(aq, blocks, mean, na.rm = TRUE))

# adapted from ?read_stars:
m = array(1:720, dim = c(x = 10, y = 12, t = 6)) # named dim
st = st_as_stars(m)
attr(st, "dimensions")$y$delta = -1
attr(st, "dimensions")$y$offset = 12
tm = Sys.Date() + 1:6
st = st_set_crs(st_set_dimensions(st, 3, values = tm), 4326)

tmp = tempfile(fileext = ".tif")
write_stars(st, tmp)

(red <- read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXsize = 10, nYSize = 12,
   nBufXSize = 2, nBufYSize = 2)))

sfc = st_set_crs(st_as_sfc(red, as_points = FALSE), 4326)
(a = aggregate(st, sfc, mean))
a[[1]]
sum(a[[1]])*30 == sum(1:720)

tm0 = Sys.Date() + -1:8
(a = aggregate(st, tm0, mean, na.rm = TRUE))
