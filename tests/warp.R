suppressPackageStartupMessages(library(stars))
geomatrix = system.file("tif/geomatrix.tif", package = "stars")
x = read_stars(geomatrix)
# can stars reproduce what gdal does, by default?
x2 = st_warp(x, use_gdal = TRUE, no_data_value = -9999)
y = st_warp(x, x2)
if (interactive()) { plot(x2, breaks = "equal", axes=TRUE) }
if (interactive()) { plot(y, breaks = "equal", axes=TRUE) }
names(x2) = names(y)
all.equal(x2, y) # yes?

# does gdal reproduce with stars template object?
(x2 = setNames(st_warp(x, y, use_gdal = TRUE, no_data_value=-1), "file.tif"))

# does gdal reproduce what stars does, default cell size?
(x2 = st_warp(x, crs = st_crs(x), use_gdal = FALSE))
(y = setNames(st_warp(x, x2, use_gdal = TRUE, debug = FALSE, no_data_value=-1), "file.tif"))

# try with multiple bands:
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x1 = read_stars(tif))
tifcp = tempfile(fileext = ".tif")
file.copy(tif, tifcp)
x1p = read_stars(tifcp, proxy = TRUE)
st_dimensions(x1p)
(x1a = st_warp(x1, crs = st_crs(4326)))
(x1b = setNames(st_warp(x1, x1p, use_gdal = TRUE, no_data_value=-1), "file.tif"))

# does gdal reproduce what stars does? Smaller grid:
x2 = st_warp(x, crs = st_crs(x), use_gdal = FALSE, cellsize = 3)
# x2 = x2[,2:43,2:43]
if (interactive()) { plot(x2, breaks = "equal", axes=TRUE, reset = FALSE) }
if (interactive()) { plot(st_as_sfc(st_bbox(x2)), add = TRUE, col = NA, border = 'red') }
### doesn't work: FIXME: check with more recent GDAL:
(y = setNames(st_warp(x, x2, use_gdal = TRUE, debug = FALSE, no_data_value=-1), "file.tif"))
if (interactive()) { plot(y, breaks = "equal") }
names(x2) = names(y)
# isTRUE(all.equal(x2, y, check.attributes=FALSE))
m = mean(as.vector(x2[[1]]==y[[1]]), na.rm = TRUE)
u = unique(as.vector(x2[[1]]-y[[1]]))

g = st_as_stars()
attr(g, "dimensions")$x$offset = 0
g$values = as.vector(t(matrix(seq_len(prod(dim(g))), 180, 360)))
if (interactive()) { plot(g, axes=TRUE) }
a = st_warp(g, st_as_stars(), use_gdal=FALSE)
b = st_warp(g, st_as_stars(), use_gdal=TRUE, no_data_value = -1)
all.equal(a, b, check.attributes = FALSE)
