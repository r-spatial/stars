options(rgdal_show_exportToProj4_warnings = "none")
suppressPackageStartupMessages(library(stars))

if (require("raster", quietly = TRUE) && require("terra", quietly = TRUE)) {

tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))
(r = as(x, "Raster"))
(y = st_as_stars(r))

# single band:
x = adrop(x[,,,1])
r = as(x, "Raster")
(y = st_as_stars(r))

# proxy:
(x = read_stars(tif, proxy = TRUE))
(r = as(x, "Raster"))
(y = st_as_stars(r))
(y2 = st_as_stars(r, proxy = TRUE))
}

if (require(terra, quietly = TRUE)) {
## terra -------------
stopifnot(require(terra))
(x = read_stars(tif))
(r = as(x, "SpatRaster"))
(y = st_as_stars(r))

# single band:
x = adrop(x[,,,1])
r = as(x, "SpatRaster")
(y = st_as_stars(r))

# proxy:
(x = read_stars(tif, proxy = TRUE))
(r = as(x, "SpatRaster"))
(y = st_as_stars(r))
(y2 = st_as_stars(r, proxy = TRUE))
<<<<<<< HEAD
=======

f = system.file("tif/lc.tif", package = "stars")
lc = read_stars(system.file("tif/lc.tif", package = "stars"))
levels(lc[[1]])
r = rast(lc)
e = attr(lc[[1]], "exclude")

if (packageVersion("terra") >= "1.5.48") {

print(all.equal(terra::levels(r)[[1]][,2], terra::levels(rast(f))[[1]][!e,2]))
print(all.equal(terra::coltab(r)[[1]][!e,], terra::coltab(rast(f))[[1]][!e,], check.attributes = FALSE))

r<- rast(nrow=10, ncol=10)
set.seed(5)
values(r)<- sample(1:3, size = ncell(r), replace = TRUE)
levels(r) <- data.frame(ID=1:3, category = c("c1", "c2", "c3"))

r2<- rast(nrow=10, ncol=10)
set.seed(6)
values(r2)<- sample(c(0,2,3), size = ncell(r2), replace = TRUE)
levels(r2) <- data.frame(ID=c(0,2,3), category = c("x1", "x2", "x3"))

r3<- c(r,r2)
names(r3)<- c("lyr.1", "lyr.2")

r3_stars<- st_as_stars(r3)

st_redimension(r3_stars) |> print()
rast(r3_stars) |> levels() |> print()
}
>>>>>>> factor
}
