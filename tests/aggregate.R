suppressPackageStartupMessages(library(stars))

library(spacetime)
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_as_sfc(stations), time = dates)
(aq = st_as_stars(list(PM10 = air), dimensions = d))
(a = aggregate(aq, st_as_sf(DE_NUTS1), mean, na.rm = TRUE))



