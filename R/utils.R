#' Native interface to gdal utils
#' @name gdal_utils
#' @param name character; name of input layer
#' @param source character; name(s) of input layer(s)
#' @param destination character; name of output layer
#' @param quiet logical; suppress printed output
#' @param options layer opening options
#' @return \code{st_gdalwarp} and \code{st_gdalrasterize} return \code{NULL} (invisibly) on success, an error message on failure.
#' \code{st_gdalinfo} returns the raster metadata as character vector
#' @export
st_gdalinfo = function(name, options = character(0), quiet = FALSE) {
	ret = CPL_gdalinfo(name, options)
	if (! quiet)
		cat(ret)
	invisible(ret)
}

#' @name gdal_utils
#' @export
st_gdalwarp = function(source, destination, options = character(0)) {
	if (! CPL_gdalwarp(source, destination, options))
		stop("st_gdalwarp: an error occured")
	else
		invisible(NULL)
}

#' @name gdal_utils
#' @export
st_gdalrasterize = function(source, destination, options = character(0)) {
	if (! CPL_gdalrasterize(source, destination, options))
		stop("st_gdalrasterize: an error occured")
	else
		invisible(NULL)
}
