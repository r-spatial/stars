library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = st_stars(tif)
xy = structure(list(x = c(293253.999046018, 296400.196497684), y = c(9113801.64775462, 
9111328.49619133)), .Names = c("x", "y"))
pts = st_as_sf(data.frame(do.call(cbind, xy)), coords = c("x", "y"), crs = st_crs(x))
image(x, axes = TRUE)
plot(st_as_sfc(st_bbox(pts)), col = NA, add = TRUE)

bb = st_bbox(pts)
(xx = x[bb])
image(xx)
plot(st_as_sfc(bb), add = TRUE, col = NA)
st_contains(st_as_sfc(st_bbox(xx)), st_as_sfc(st_bbox(pts)))

image(x)
pt = st_point(c(x = 290462.103109179, y = 9114202.32594085))
buf = st_buffer(st_sfc(pt, crs = st_crs(x)), 1500)
plot(buf, add = TRUE)

buf = st_sfc(st_polygon(list(st_buffer(pt, 1500)[[1]], st_buffer(pt, 1000)[[1]])),
	crs = st_crs(x))
image(x[buf])
plot(buf, add = TRUE, col = NA)
image(x[buf,crop=FALSE])
plot(buf, add = TRUE, col = NA)

plot(x, rgb = 1:3)
