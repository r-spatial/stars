suppressPackageStartupMessages(library(stars))
if (suppressPackageStartupMessages(require(sp, quietly = TRUE)) && require(spacetime, quietly = TRUE)) {
 data(air) # this loads several datasets in .GlobalEnv
 dim(air)
 d = st_dimensions(station = st_as_sfc(stations), time = dates)
 aq = st_as_stars(list(PM10 = air), dimensions = d)
 image(aperm(log(aq), 2:1), main = "NA pattern (white) in PM10 station time series")
 st_geometry(aq)
}

csv = system.file("tif/ESACCI-LC-Legend.csv", package = "starsdata")
if (csv != "") {
	rgb = read.csv(csv, header = TRUE, sep = ";")
	luc = read_stars(system.file("tif/LUC3.tif", package = "starsdata"))
	plot(luc, rgb = rgb, axes = TRUE, key.pos = NULL)
}

# https://github.com/r-spatial/stars/issues/233 :
nc = read_sf(system.file("shape/nc.shp", package = "sf"))
output = st_as_stars(st_bbox(nc), nx = 1, ny = 1, values = 1)
plot(output)
