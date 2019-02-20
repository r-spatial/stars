suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
(xx = x[,c(1:347,349),c(1:350,352),]) # rectilinear: one-but-last row & col missing
image(xx) # chooses useRaster itself
  # see also tidy.R for a ggplot example, leaving out the row.

m = matrix(1:16, 4, 4)
x = 1:4
y = 2:5
(st = st_as_stars(list(m = m), dimensions = st_dimensions(x = x, y = y)))
st = st_set_dimensions(st, 1, values = c(1,2,3,4,6))
st = st_set_dimensions(st, 2, values = c(2,3,4,5,8))
try(st_set_dimensions(st, 2, values = c(2,3,4,5,8,20))) # one too many

# $<-.stars:
st$foo = st$m * 2
st

# st_as_sf:

if (utils::packageVersion("sf") > "0.7.2") {
	plot(st_as_sf(st, as_points = FALSE), axes = TRUE)
}

image(st, text_values = TRUE, axes = TRUE)
