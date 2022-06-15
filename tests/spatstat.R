suppressPackageStartupMessages(library(sf))

if (require(spatstat.geom, quietly = TRUE)) {
library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
s = adrop(read_stars(tif)[,,,1]) > 70
plot(s)
m = as.owin(s)
plot(m)
table(m$m)
}
