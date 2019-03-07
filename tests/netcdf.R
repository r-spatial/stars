Sys.setenv(TZ="UTC")
suppressPackageStartupMessages(library(stars))
# read_ncdf:
f <- system.file("nc/reduced.nc", package = "stars")
read_ncdf(f)
read_ncdf(f, var = c("anom"))
read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), count = c(10, 12, 1, 1)))
