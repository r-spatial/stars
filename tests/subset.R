suppressPackageStartupMessages(library(stars))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
y = st_set_dimensions(x, "band", point = TRUE)
y[,1:10, 1:10, c(1,4,3,2)]
y = st_set_dimensions(x, "band", point = NA)
y[,1:10, 1:10, c(1,4,3,2)]
y = st_set_dimensions(x, "band", point = FALSE)
y[,1:10, 1:10, c(1,4,3,2)]
y = st_set_dimensions(x, "band", values = letters[1:6])
y[,1:10, 1:10, c(1,4,3,2)]
y = st_set_dimensions(x, "band", values = letters[1:6], point = NA)
y[,1:10, 1:10, c(1,4,3,2)]
y = st_set_dimensions(x, "band", values = letters[1:6], point = TRUE)
y[,1:10, 1:10, c(1,4,3,2)]
