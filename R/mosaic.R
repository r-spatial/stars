#' build mosaic (composite) of several spatially disjoint stars objects
#' 
#' build mosaic (composite) of several spatially disjoint stars objects
#' @param .x object of class stars, or character vector with input dataset names
#' @param ... further input stars objects
#' @param dst character; destination file name
#' @param options character; options to the gdalbuildvrt command
#' @param file_ext character; file extension, determining the format used to write to (".tif" implies GeoTIFF)
#' @return the stars method returns a stars object with the composite of the input; the \code{character} method returns the file name of the file with the mosaic; see also the GDAL documentation of \code{gdalbuildvrt}
#' @details the gdal function buildvrt builds a mosaic of input images; these imput images can be multi-band, but not higher-dimensional data cubes or stars objects with multiple attributes
#' 
#' uses \link[sf]{gdal_utils} to internally call \code{buildvrt}; no executables external to R are called.
#' @export
#' @examples
#' x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
#' x1 = x[,100:200,100:200,]
#' x2 = x[,150:300,150:300,]
#' plot(st_mosaic(x1, x2))
st_mosaic = function(.x, ...) UseMethod("st_mosaic")

#' @export
#' @name st_mosaic
st_mosaic.stars = function(.x, ..., dst = tempfile(fileext = file_ext), 
		options = c("-vrtnodata", "-9999", "-srcnodata", "nan"),
# -srcnodata "nan": see https://github.com/r-spatial/stars/issues/274
		file_ext = ".tif") {
	lst_write = function(obj) {
		fname = tempfile(fileext = file_ext)
		write_stars(obj, fname)
		fname
	}
	objs = if (missing(.x)) list(...) else append(list(.x), list(...))
	src = sapply(objs, lst_write)
	st_mosaic(src, dst = dst, file_ext = file_ext, options = options)
	ret = setNames(read_stars(dst), names(objs[[1]])[1])
	if (!inherits(ret, "stars_proxy")) {
		unlink(src)
		if (missing(dst))
			unlink(dst)
	}
	ret
}

#' @export
#' @name st_mosaic
st_mosaic.character = function(.x, ...,  dst = tempfile(fileext = file_ext), 
		options = c("-vrtnodata", "-9999"), file_ext = ".tif") {
	sf::gdal_utils("buildvrt", .x, dst, options)
	dst
}

#' @export
#' @name st_mosaic
st_mosaic.stars_proxy = function(.x, ..., dst = tempfile(fileext = file_ext),
		options = c("-vrtnodata", "-9999"), file_ext = ".tif") {
	if (length(.x) > 1 || length(.x[[1]]) > 1)
		stop("st_mosaic.stars_proxy only implemented for single-file proxy objects")
	objs = if (missing(.x)) list(...) else append(list(.x), list(...))
	files = sapply(objs, function(sp) sp[[1]])	
	read_stars(st_mosaic(files, dst = dst, options = options, file_ext = file_ext), proxy = TRUE)
}
