suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars_meta(tif)
geomatrix = system.file("tif/geomatrix.tif", package = "stars")
x = read_stars_meta(geomatrix)
nc = system.file("nc/tos_O1_2001-2002.nc", package = "stars")
x = read_stars_meta(nc)

# multiple sub-datasets:
nc_red = system.file("nc/reduced.nc", package = "stars")
red = read_stars_meta(nc_red)
