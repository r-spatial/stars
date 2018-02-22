suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
xx = x[,c(1:347,349),c(1:350,352),] # rectilinear: one-but-last row & col missing
image(xx) # chooses useRaster itself
