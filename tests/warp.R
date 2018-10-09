library(stars)
geomatrix = system.file("tif/geomatrix.tif", package = "stars")
x = read_stars(geomatrix)
# can stars reproduce what gdal does, by default?
x2 = st_warp(x, use_gdal = TRUE) #, options = c("-dstnodata", "-9999"))
y = st_warp(x, x2)
plot(x2, breaks = "equal", axes=TRUE)
plot(y, breaks = "equal", axes=TRUE)
names(x2) = names(y)
all.equal(x2, y) # yes

# does gdal reproduce what stars does? Smaller grid:
(x2 = st_warp(x, crs = st_crs(x), use_gdal = FALSE, cellsize = 3))
# x2 = x2[,2:43,2:43]
plot(x2, breaks = "equal", axes=TRUE, reset = FALSE)
plot(st_as_sfc(st_bbox(x2)), add = TRUE, col = NA, border = 'red')
### doesn't work: FIXME: check with more recent GDAL:
#(y = st_warp(x, x2, use_gdal = TRUE, debug = TRUE))
#plot(y, breaks = "equal")
#names(x2) = names(y)
#all.equal(x2, y) 

