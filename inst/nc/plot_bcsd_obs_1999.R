## data-getting is described here
## https://github.com/r-spatial/stars/issues/94#issuecomment-459583295

f <- system.file("nc/test_stageiv_xyt.nc", package = "stars")
library(stars)

x <- read_ncdf(f, curvilinear = c("lon", "lat"))

brks <- c(0.129999995231628, 0.879999995231628, 1.5, 2.12999987602234, 
          2.87999987602234, 3.5, 4.19000005722046, 5.05999994277954, 5.80999994277954, 
          6.75, 7.87999963760376, 8.88000011444092, 10.1300001144409, 11.25, 
          12.5, 13.75, 15, 16.25, 17.5, 18.75, 20, 21.25, 22.5, 23.75, 
          25, 26.3799991607666, 27.75, 29.1299991607666, 30.3799991607666, 
          32.25, 34, 35.6300010681152, 37.75, 39.629997253418, 42.129997253418, 
          45.25, 49.25, 58.75)
qu_0_omit <- function(x, ..., n = 22) {
  x <- na.omit(x)
  c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
}
plot(stars::slice.stars(x, index = 17, along = "time"), border = NA, breaks = qu_0_omit(x[[1]][,,17]), reset = FALSE, col = viridis::viridis(22))
plot(sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg"), add = TRUE, reset = FALSE, col = NA)

