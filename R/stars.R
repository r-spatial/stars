#' read raster/array dataset from file or connection

#' read raster/array dataset from file or connection
#' @param file character; file name to read
#' @param ... ignored
#' @return object of class \code{stars}
#' @export
#' @example
#' x = st_stars("stars/inst/tif/L7_ETMs.tif")
st_stars = function(file, ...) {
	structure(CPL_read_gdal(file), class = "stars")
}

#' @export
#' @example
#' x = st_stars("stars/inst/tif/L7_ETMs.tif")
#' image(x)
image.stars = function(x, ..., band = 1) {
	x = x[ , rev(seq_len(dim(x)[2])), band]
	image.default(x, ...)
}
