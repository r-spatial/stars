suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(r = read_stars(tif, proxy = TRUE))
plot(r)
dim(r)
r[,,,2:3]
plot(r)
(xx = c(r,r))
c(xx)
st_as_stars(r)
(y = st_apply(r, 1:2, max))
(yy = st_as_stars(y))
(y = adrop(st_apply(r, 1:2, max)))
(yy = st_as_stars(y))
plot(yy)

(xx = c(r,r))
names(xx) = c("a", "b")
xx["a"]
pt = st_point(c(x = 290462.103109179, y = 9114202.32594085))
buf = st_buffer(st_sfc(pt, crs = st_crs(r)), 1500)
buf = st_sfc(st_polygon(list(st_buffer(pt, 1500)[[1]], st_buffer(pt, 1000)[[1]])),
  crs = st_crs(r))
r = r[buf] # crops
r = r[buf, epsilon = 1e-5] # crops with shrinked bounding box
