suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(lwgeom))
geomatrix = system.file("tif/geomatrix.tif", package = "stars")
(x = read_stars(geomatrix))
new = st_crs(4326)
y = st_transform(x, new)
plot(st_transform(st_as_sfc(st_bbox(x)), new), col = NA, border = 'red')
plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
image(y, add = TRUE)
plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
plot(st_transform(st_as_sfc(x, as_points=FALSE), new), add = TRUE)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)[,1:10,1:10,1:3]
x_ = st_transform(x, st_crs(4326))
x__ = st_transform_proj(x, st_crs(4326)$proj4string)
st_crs(x_) = st_crs(x__)
all.equal(x_,x__)

# x__ = st_transform(x, x_) #now in st_warp 
# all.equal(x_, x__)

x = st_xy2sfc(x, as_points = FALSE)
(x_ = st_transform(x, st_crs(4326)))
(x__ = st_transform_proj(x, st_crs(4326)))
all.equal(x_,x__,check.attributes = FALSE)

# nothing to transform: now errs:
try(st_transform(st_as_stars(list(matrix(1,10,10))), st_crs(4326)))
try(st_transform_proj(st_as_stars(list(matrix(1,10,10))), st_crs(4326)))
