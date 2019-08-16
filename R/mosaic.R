#' build mosaic (composite) of several spatially disjoint stars objects
#' 
#' build mosaic (composite) of several spatially disjoint stars objects
#' @param ... a list of input stars objects
#' @param options character; options to the gdalbuildvrt command
#' @return a stars object with the composite of the input; see also the documentation of \code{gdalbuildvrt}
#' @details the gdal function buildvrt builds a mosaic of input images; these imput images can be multi-band, but not higher-dimensional data cubes or stars objects with multiple attributes
#' 
#' uses \link[sf]{gdal_utils} to internally call \code{buildvrt}; no executables external to R are called.
#' @export
st_mosaic = function(..., options = c("-vrtnodata", "-9999")) {
	lst_write = function(obj) {
		fname = tempfile(fileext = ".tif")
		write_stars(obj, fname)
		fname
	}
	src = sapply(list(...), lst_write)
	dst = tempfile(fileext = ".tif")
	sf::gdal_utils("buildvrt", src, dst, options)
	ret = read_stars(dst)
	unlink(c(dst, src))
	ret
}
