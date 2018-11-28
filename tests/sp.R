library(sp)
demo(meuse)

library(stars)
(m = st_as_stars(meuse))
(gr = st_as_stars(meuse.grid))

m2 = as(m, "Spatial")
summary(m2)
gr2 = as(gr, "Spatial")
summary(gr2)

all.equal(meuse, m2, check.attributes = FALSE)
fullgrid(meuse.grid) = TRUE
all.equal(meuse.grid, gr2, check.attributes = FALSE)

all.equal(meuse, m2)
all.equal(meuse.grid, gr2)
