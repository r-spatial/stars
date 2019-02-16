library(spacetime)
library(stars)
data(air) # this loads several datasets in .GlobalEnv
dim(air)
d = st_dimensions(station = st_as_sfc(stations), time = dates)
aq = st_as_stars(list(PM10 = air), dimensions = d)
image(aperm(log(aq), 2:1), main = "NA pattern (white) in PM10 station time series")
st_geometry(aq)

rgb = read.csv(system.file("tif/ESACCI-LC-Legend.csv", package = "starsdata"), header = TRUE, sep = ";")
luc = read_stars(system.file("tif/LUC3.tif", package = "starsdata"))
plot(luc, rgb = rgb, axes = TRUE)
