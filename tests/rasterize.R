library(stars)
demo(nc, echo = FALSE, ask = FALSE)
(x = stars:::st_rasterize(nc)) # default grid:
plot(x, axes = TRUE)
# a bit more customized grid:
(x = stars:::st_rasterize(nc, st_as_stars(st_bbox(nc), nx = 100, ny = 50, values = NA_real_)))
plot(x, axes = TRUE)
(ls = st_sf(a = 1:2, st_sfc(st_linestring(rbind(c(0.1,0), c(1.1,1))), st_linestring(rbind(c(0, 0.05),c(1, 0.05))))))
(grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0,1.0), ylim = c(0,1), values = NA_real_))
# the following two plots suggests a half-gridcell-shift problem:
sf_extSoftVersion()["GDAL"]
plot(st_rasterize(ls, grd), axes=TRUE, reset = FALSE) # ALL_TOUCHED=FALSE; 
plot(ls, add = TRUE, col = 'red')
plot(st_rasterize(ls, grd, options = "ALL_TOUCHED=TRUE"), axes=TRUE, reset = FALSE)
plot(ls, add = TRUE, col = 'red')
# add lines to existing 0 values, summing values in case of multiple lines:
(grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0,1.0), ylim = c(0,1), values = 0))
r = st_rasterize(ls, grd, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE"))
plot(r, axes=TRUE, reset = FALSE)
plot(ls, add = TRUE, col = 'red')

