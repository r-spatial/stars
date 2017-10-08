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

sfc1 <- st_as_sfc(x, as_points = TRUE, use_cpp = TRUE)
sfc2 <- st_as_sfc(x, as_points = TRUE, use_cpp = FALSE)
identical(sfc1, sfc2)

sfc1 <- st_as_sfc(x, as_points = FALSE, use_cpp = TRUE)
sfc2 <- st_as_sfc(x, as_points = FALSE, use_cpp = FALSE)
identical(sfc1, sfc2)
