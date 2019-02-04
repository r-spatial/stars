suppressPackageStartupMessages(library(stars))

library(spacetime)
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_as_sfc(stations), time = dates)

blocks = st_make_grid(st_as_sfc("POLYGON ((5.871619 47.26986, 15.03811 47.26986, 15.03811 55.05653, 5.871619 55.05653, 5.871619 47.26986))", crs = 4326),
	n = c(3,3))

(aq = st_as_stars(list(PM10 = air), dimensions = d))
(a = aggregate(aq, blocks, mean, na.rm = TRUE))

