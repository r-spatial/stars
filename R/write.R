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
#' @export
st_write.stars = function(obj, dsn, layer = 1, ..., driver = detect.driver(dsn), 
		options = character(0), type = "Float32", NA_value = NA_real_) {
	if (length(obj) > 1 && missing(layer))
		warning("all but first attribute are ignored")
	sf::gdal_write(obj[layer], ..., file = dsn, driver = driver, options = options, 
		type = type, na_val = NA_value)
}

#nocov start
detect.driver = function(filename) {
	# from raster::.getFormat:
	ext <- tolower(tools::file_ext(filename))
	if (nchar(ext) < 2) {
		warning("file without extension, using driver GTiff", call. = FALSE)
		"GTiff"
	} else {
		if (ext == 'tif' | ext == 'tiff') { return('GTiff')
		} else if (ext == 'grd') { return('raster')
		} else if (ext == 'asc') { return('ascii')
		} else if (ext == 'nc' | ext == 'cdf' | ext == 'ncdf') { return('CDF')
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
