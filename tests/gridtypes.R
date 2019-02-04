library(stars)

# regular, but not spatial:
d = st_dimensions(a = 1:3, b = 1:3, band = c("foo", "bar"))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
try(x <- st_bbox(st)) # error

# regular, geotransform:
d = st_dimensions(x = 1:3, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
as.data.frame(st, add_max=TRUE)[1:4,]
st_bbox(st)

# rectilinear with offset given:
xd = c(1, 2, 4)
d = st_dimensions(x = xd, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
as.data.frame(st, add_max=TRUE)[1:4,]
st_bbox(st)

# rectilinear with midpoints given:
xd = c(1, 2, 4)
d = st_dimensions(x = xd, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"), cell_midpoints = TRUE)
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
as.data.frame(st, add_max=TRUE)[1:4,]
st_bbox(st)

# rectilinear with midpoints given, point support
xd = c(1, 2, 4)
d = st_dimensions(x = xd, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"), cell_midpoints = TRUE, point = TRUE)
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
as.data.frame(st, add_max=TRUE)[1:4,]
st_bbox(st)

# rectilinear with start/end given:
#xd = stars:::make_intervals(start = c(1,2,4), end = c(2, 4, 8))
xd = c(1, 2, 4, 8) # one more than dim of the data array
d = st_dimensions(x = xd, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
as.data.frame(st, add_max=TRUE)[1:4,]
st_bbox(st)

# with sfc:
sfc = st_sfc(st_point(0:1), st_point(2:1), st_point(4:3))
d = st_dimensions(x = sfc, y = 1:3, band = c("foo", "bar"))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
st_as_sf(st, long = TRUE)[1:4,]
st_bbox(st)

# rotated/sheared:
d = st_dimensions(x = 1:3, y = 1:3, band = c("foo", "bar"), .raster = c("x", "y"), affine = c(0.2, -0.2))
(st = st_as_stars(array(1:18, c(3,3,2)), dimension = d))
as.data.frame(st, add_max=FALSE)[1:4,]
try(as.data.frame(st, add_max=TRUE)[1:4,]) # errors
st_bbox(st)

# curvilinear:
set.seed(13531)
lon = st_as_stars(matrix(signif(runif(9), 2), 3, 3))
lat = st_as_stars(matrix(signif(runif(9), 2), 3, 3))
ll = setNames(c(lon, lat), c("lon", "lat"))
d = st_dimensions(lon = 1:3, lat = 1:3)
a = st_as_stars(list(X = array(1:9, c(3,3))), dimensions = d)
(st = st_as_stars(a, curvilinear = ll))
as.data.frame(st)[1:11,]
st_bbox(st)

# time regular:
t = as.Date("2019-01-20") + 0:3
d = st_dimensions(t, sfc)
(st = st_as_stars(list(a = matrix(1:12, 4, 3)), dimensions = d))
as.data.frame(st)

# time, rectilinear
t = as.Date("2019-01-20") + c(0, 2, 3, 10)
d = st_dimensions(t, sfc)
(st = st_as_stars(list(a = matrix(1:12, 4, 3)), dimensions = d))
as.data.frame(st)
