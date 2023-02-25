suppressPackageStartupMessages(library(sf))

library(stars)
if (suppressPackageStartupMessages(require(spatstat.geom, quietly = TRUE))) {
tif = system.file("tif/L7_ETMs.tif", package = "stars")
s = adrop(read_stars(tif)[,,,1]) > 70
plot(s)
m = as.owin(s)
plot(m)
print(table(m$m))
}
