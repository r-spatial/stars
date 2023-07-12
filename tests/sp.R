options(rgdal_show_exportToProj4_warnings = "none")
if (suppressPackageStartupMessages(require(sp, quietly = TRUE))) {
demo(meuse)
# remove +init=xxx
meuse@proj4string = CRS(
"+proj=sterea +lat_0=52.1561605555556 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
)
meuse.grid@proj4string = CRS(
"+proj=sterea +lat_0=52.1561605555556 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"
)

suppressPackageStartupMessages(library(stars))
(m = st_as_stars(meuse))
(gr = st_as_stars(meuse.grid))

m2 = as(m, "Spatial")
summary(m2)
gr2 = as(gr, "Spatial")
summary(gr2)

all.equal(meuse, m2, check.attributes = FALSE)
fullgrid(meuse.grid) = TRUE
all.equal(meuse.grid, gr2, check.attributes = FALSE)

#all.equal(meuse, m2)
#all.equal(meuse.grid, gr2)
}
