suppressPackageStartupMessages(library(sf))
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
m = st_set_geometry(nc, NULL)
n = as.matrix(m[c("BIR74", "SID74", "NWBIR74", "BIR79", "SID79", "NWBIR79")])
dim(n) = c(county = 100, var = 3, year = 2)
dimnames(n) = list(county = nc$NAME, var = c("BIR", "SID", "NWBIR"), year = c(1974, 1979))
suppressPackageStartupMessages(library(stars))
(st = st_as_stars(pop = n))
foo <- st %>% st_set_dimensions(1, st_geometry(nc)) # %>% st_set_dimensions(3, c(1974, 1979))
foo
frac = function(x) x[2] / x[1]
frac2 = function(x) c(sidsr = x[2] / x[1], nwbr = x[3] / x[1])
st_apply(foo, c(1,3), frac)
st_apply(foo, c(1,3), frac2)
library(abind)
aperm(st_apply(foo, c(1,3), frac2), c(2,3,1))

split(foo, 2)
split(foo, 3)

st_crs(foo)
plot(foo)
