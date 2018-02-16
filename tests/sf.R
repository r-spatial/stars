library(stars)
library(sf)
jp2 = system.file("jp2/B01.jp2", package = "stars")
#(x = st_stars(jp2, options = c("OVERVIEW_LEVEL=3")))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = st_stars(tif, options = c("OVERVIEW_LEVEL=3")))
# names(x) = "L7_ETM"

# library(abind)
# x = adrop(x)
image(x)
(sfc = st_as_sfc(x, as_points = FALSE))
plot(sfc, add  =TRUE)
(sfc = st_as_sfc(x, as_points = TRUE))
plot(sfc, add = TRUE)

sf = st_as_sf(x, as_points = FALSE)
plot(sf, border = NA)

sfc1 <- st_as_sfc(x, as_points = TRUE, use_cpp = TRUE, na.rm = FALSE)
sfc2 <- st_as_sfc(x, as_points = TRUE, use_cpp = FALSE)
identical(sfc1, sfc2)

sfc1 <- st_as_sfc(x, as_points = FALSE, use_cpp = TRUE, na.rm = FALSE)
sfc2 <- st_as_sfc(x, as_points = FALSE, use_cpp = FALSE)
identical(sfc1, sfc2)

# sf -> stars -> sf
x = st_sfc(st_point(0:1), st_point(1:2), st_point(2:3))
m = matrix(1:9,3)
colnames(m) = c("a", "b", "c")
foo = st_sf(m, geom = x)
st_stars(foo)
st_stars(foo, times = NULL)
st = st_stars(foo, times = as.Date("2017-11-27") + 0:2)
st
st_as_sf(st)
st_as_sf(st_stars(foo))
