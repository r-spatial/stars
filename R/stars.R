#' read raster/array dataset from file or connection

#' read raster/array dataset from file or connection
#' @param file character; file name to read
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @return object of class \code{stars}
#' @export
#' @example
#' x = st_stars("stars/inst/tif/L7_ETMs.tif")
st_stars = function(file, options = character(0), driver = character(0)) {
	properties = CPL_read_gdal(file, options, driver, TRUE)
	structure(attr(properties, "data"),
		class = "stars", 
		crs = st_crs(properties$proj4string),
		properties = structure(properties, data = NULL))
}

#' @export
#' @example
#' x = st_stars("stars/inst/tif/L7_ETMs.tif")
#' image(x)
image.stars = function(x, ..., band = 1) {
	x = x[ , rev(seq_len(dim(x)[2])), band]
	image.default(x, ...)
}

#' @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
xy_from_colrow = function(x, geotransform) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

#' @export
as.data.frame.stars = function(x, ...) {
	meta = attr(x, "properties")
	xc = xy_from_colrow(cbind(do.call(seq, as.list(meta$cols - 1)) + .5, 0), meta$geotransform)[,1]
	yc = xy_from_colrow(cbind(0, do.call(seq, as.list(meta$rows - 1)) + .5), meta$geotransform)[,2]
	band = do.call(seq, as.list(meta$bands))
	coords = expand.grid(x = xc, y = yc, band = band)
	data.frame(coords, z = c(x))
}
