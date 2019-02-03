suppressPackageStartupMessages(library(stars))

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))

# write stars object:
write_stars(x, tempfile(), driver = "GTiff")

# write stars_proxy object:
(x = read_stars(tif, proxy = TRUE))
write_stars(x, tempfile(), driver = "GTiff")
