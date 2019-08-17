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
#' @examples
#' x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
#' x1 = x[,100:200,100:200,]
#' x2 = x[,150:300,150:300,]
#' plot(st_mosaic(x1, x2))
st_mosaic = function(..., options = c("-vrtnodata", "-9999")) {
	lst_write = function(obj) {
		fname = tempfile(fileext = ".tif")
		write_stars(obj, fname)
		fname
	}
	src = sapply(list(...), lst_write)
	dst = tempfile(fileext = ".tif")
	on.exit(unlink(c(dst, src)))
	sf::gdal_utils("buildvrt", src, dst, options)
	setNames(read_stars(dst), names(list(...)[[1]])[1])
}
