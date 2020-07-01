# reset the offset when x is a sub-raster (i.e, starts at index larger than 1)
reset_sub = function(x) {
	d = st_dimensions(x)
	xy = attr(d, "raster")$dimensions
	if (all(is.na(xy)))
		return(x)
	
	for (i in xy) {
		if (d[[ i ]]$from > 1) {
			if (!is_regular_grid(x))
				stop("can only write sub-rasters for regular grids")
			ioff = d[[ i ]]$from - 1
			d[[ i ]]$offset = d[[ i ]]$offset + ioff * d[[ i ]]$delta
			d[[ i ]]$to = d[[ i ]]$to - ioff
			d[[ i ]]$from = 1
		}
	}
	structure(x, dimensions = d)
}

st_write.stars = function(obj, dsn, layer, ...) {
	.Deprecated("read_stars") # nocov
}


#' @name write_stars
#' @export
write_stars = function(obj, dsn, layer, ...) UseMethod("write_stars")


#' write stars object to gdal dataset (typically: to file)
#' 
#' @param obj object of class \code{stars}
#' @param dsn gdal dataset (file) name
#' @param layer attribute name; if missing, the first attribute is written
#' @param ... passed on to \link[sf]{gdal_write}
#' @param driver driver driver name; see \link[sf]{st_drivers}
#' @param options character vector with options
#' @param type character; output binary type, one of: \code{Byte} for eight bit unsigned integer, \code{UInt16} for sixteen bit unsigned integer, \code{Int16} for sixteen bit signed integer, \code{UInt32} for thirty two bit unsigned integer, \code{Int32} for thirty two bit signed integer, \code{Float32} for thirty two bit floating point, \code{Float64} for sixty four bit floating point.
#' @param NA_value non-NA value that should represent R's \code{NA} value in the target raster file; if set to \code{NA}, it will be ignored.
#' @param update logical; if \code{TRUE}, an existing file is being updated
#' @param normalize_path logical; see \link{read_stars}
#' @name write_stars
#' @export
write_stars.stars = function(obj, dsn, layer = 1, ..., driver = detect.driver(dsn), 
		options = character(0), type = "Float32", NA_value = NA_real_, update = FALSE,
		normalize_path = TRUE) {
	if (missing(layer) && length(obj) > 1)
		warning("all but first attribute are ignored")
	obj = st_upfront(obj[layer])
	if (! update) # new file: should not be a sub-array
		obj = reset_sub(obj)
	if (normalize_path)
		dsn = enc2utf8(maybe_normalizePath(dsn, TRUE))
	sf::gdal_write(obj, ..., file = dsn, driver = driver, options = options, 
		type = type, NA_value = NA_value, geotransform = get_geotransform(obj), 
		update = update)
	invisible(obj)
}

#' @name write_stars
#' @param chunk_size length two integer vector with the number of pixels (x, y) used in the read/write loop; see details.
#' @param progress logical; if \code{TRUE}, a progress bar is shown
#' @details \code{write_stars} first creates the target file, then updates it sequentially by writing blocks of \code{chunk_size}.
#' @export
write_stars.stars_proxy = function(obj, dsn, layer = 1, ..., driver = detect.driver(dsn), 
		options = character(0), type = "Float32", NA_value = NA_real_, 
		chunk_size = c(dim(obj)[1], floor(25e6 / dim(obj)[1])), progress = TRUE) {

	if (missing(layer) && length(obj) > 1)
		warning("all but first attribute are ignored")
	if (layer != 1)
		stop("only first attribute of a stars_proxy object can be written; consider using merge")
	if (length(obj[[1]]) > 1) { # collapse bands:
		out_file = tempfile(fileext = ".vrt")
		gdal_utils("buildvrt", x[[1]], out_file, options = "-separate")
		x[[1]] = out_file
	}
	if (progress) {
		pb = txtProgressBar()
		setTxtProgressBar(pb, 0)
	}

	dim_obj = dim(obj)
	if (prod(chunk_size) > prod(dim_obj[1:2])) {
		write_stars(st_as_stars(obj), dsn, layer, ..., driver = driver, options = options,
			type = type, NA_value = NA_value)
		return(invisible(obj))
	}

	# write chunks:
	di = st_dimensions(obj)
	if (di[[1]]$from > 1 || di[[2]]$from > 1)
		message("chunked writing may not work for subsetted rasters: in case of failure use write_stars(st_as_stars(object))")

	created = FALSE

	ncol = ceiling(dim_obj[1] / chunk_size[1])
	nrow = ceiling(dim_obj[2] / chunk_size[2])
	for (col in 1:ncol) { 
		di[[1]]$from = 1 + (col - 1) * chunk_size[1]
		di[[1]]$to   = min(col * chunk_size[1], dim_obj[1])
		for (row in 1:nrow) {
			di[[2]]$from = 1 + (row - 1) * chunk_size[2]
			di[[2]]$to   = min(row * chunk_size[2], dim_obj[2])
			chunk = st_as_stars(structure(obj, dimensions = di))
			if (! created) { # create:
				d = st_dimensions(chunk)
				d_obj = st_dimensions(obj)
				d[[1]]$from = d[[2]]$from = 1
				d[[1]]$to = d_obj[[1]]$to
				d[[2]]$to = d_obj[[2]]$to
				# reset dimensions 1/2 to original:
				sf::gdal_write(structure(obj, dimensions = d), ..., file = dsn, driver = driver, options = options, 
					type = type, NA_value = NA_value, geotransform = get_geotransform(obj)) # branches on stars_proxy
			}
			created = TRUE
			write_stars(chunk, dsn = dsn, layer = layer, driver = driver,
				options = options, type = type, update = TRUE)
			if (progress)
				setTxtProgressBar(pb, ((col-1) * nrow + row) / (ncol * nrow))
		}
	}
	if (progress)
		close(pb)
	invisible(obj)
}

#' @name write_stars
#' @export
#' @param filename character; used for guessing driver short name based on file 
#' extension; see examples
#' @examples 
#' detect.driver("L7_ETMs.tif")
detect.driver = function(filename) { #nocov start
	# from raster::.getFormat:
	ext <- tolower(tools::file_ext(filename))
	if (nchar(ext) < 2) {
		warning("file without extension, using driver GTiff", call. = FALSE)
		"GTiff"
	} else {
		if (ext == 'tif' | ext == 'tiff') { return('GTiff')
		} else if (ext == 'grd') { return('raster')
		} else if (ext == 'asc') { return('ascii')
		} else if (ext == 'nc' || ext == 'cdf' || ext == 'ncdf') { return('netcdf')
		} else if (ext == 'kml') { return('KML')
		} else if (ext == 'kmz') { return('KML')		
		} else if (ext == 'big') { return('big.matrix')
		} else if (ext == 'sgrd') { return('SAGA')
		} else if (ext == 'sdat') { return('SAGA')
		} else if (ext == 'bil') { return('BIL')
		} else if (ext == 'bsq') { return('BSQ')
		} else if (ext == 'bip') { return('BIP')
		} else if (ext == 'bmp') { return('BMP') 
		} else if (ext == 'gen') { return('ADRG') 
		} else if (ext == 'bt') { return('BT') 
		} else if (ext == 'envi') { return('ENVI')
		} else if (ext == 'ers') { return('ERS') 
		} else if (ext == 'img') { return( 'HFA') 
		} else if (ext == 'rst') { return('RST') 
		} else if (ext == 'mpr') { return('ILWIS')
		} else if (ext == 'rsw') { return('RMF')
		} else if (ext == 'flt') { return('EHdr')
		} else { 
			warning('extension ', ext, ' is unknown. Using default driver GTiff.')
			return('GTiff') 
		}
	}
}
#nocov end
