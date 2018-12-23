library(spacetime)
library(stars)
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_as_sfc(stations), time = dates)
aq = st_as_stars(list(PM10 = air), dimensions = d)
image(aperm(log(aq), 2:1), main = "NA pattern (white) in PM10 station time series")
st_geometry(aq)

