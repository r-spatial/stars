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
	.Deprecated("write_stars") # nocov
}


#' @name write_stars
#' @export
write_stars = function(obj, dsn, layer, ...) UseMethod("write_stars")


#' write stars object to gdal dataset (typically: to file)
#' 
#' @param obj object of class \code{stars}
#' @param dsn gdal dataset (file) name
#' @param layer attribute name; if missing, the first attribute is written
#' @param ... passed on to \link[sf:gdal]{gdal_write}
#' @param driver driver driver name; see \link[sf]{st_drivers}
#' @param options character vector with dataset creation options, passed on to GDAL
#' @param type character; output binary type, one of: \code{Byte} for eight bit unsigned integer, \code{UInt16} for sixteen bit unsigned integer, \code{Int16} for sixteen bit signed integer, \code{UInt32} for thirty two bit unsigned integer, \code{Int32} for thirty two bit signed integer, \code{Float32} for thirty two bit floating point, \code{Float64} for sixty four bit floating point.
#' @param NA_value non-NA value that should represent R's \code{NA} value in the target raster file; if set to \code{NA}, it will be ignored.
#' @param update logical; if \code{TRUE}, an existing file is being updated
#' @param normalize_path logical; see \link{read_stars}
#' @param scale_offset length 2 numeric vector with scale, offset values: raw values computed by raw = (value - offset) / scale are written to dsn; scale and offset values are written to dsn or else a warning is raised
#' @name write_stars
#' @export
write_stars.stars = function(obj, dsn, layer = 1, ..., driver = detect.driver(dsn), 
		options = character(0), 
		type = if (is.factor(obj[[1]]) && length(levels(obj[[1]])) < 256) "Byte" else "Float32", 
		NA_value = NA_real_, update = FALSE, normalize_path = TRUE, scale_offset = c(1.0, 0.0)) {

	if (missing(layer) && length(obj) > 1)
		warning("all but first attribute are ignored")
	if (length(layer) > 1)
		stop("layer should have length 1; for writing multi-band images use merge() to merge layers into a dimension")
	obj = st_upfront(obj[layer])
	if (! update) # new file: should not be a sub-array
		obj = reset_sub(obj)
	if (!update && !is.null(attr(obj[[1]], "colors"))) # add r g b alpha table from colors:
		obj[[1]] = structure(obj[[1]], rgba = t(col2rgb(attr(obj[[1]], "colors"), alpha = TRUE)))
	if (normalize_path)
		dsn = enc2utf8(maybe_normalizePath(dsn, TRUE))
	sf::gdal_write(obj, ..., file = dsn, driver = driver, options = options, 
		type = type, NA_value = NA_value, geotransform = get_geotransform(obj), 
		update = update, scale_offset = scale_offset)
	invisible(obj)
}

#' @name write_stars
#' @param chunk_size length two integer vector with the number of pixels (x, y) used in the read/write loop; see details.
#' @param progress logical; if \code{TRUE}, a progress bar is shown
#' @details \code{write_stars} first creates the target file, then updates it sequentially by writing blocks of \code{chunk_size}.
#' @details in case \code{obj} is a multi-file \code{stars_proxy} object, all files are written as layers into the output file \code{dsn}
#' @export
write_stars.stars_proxy = function(obj, dsn, layer = 1, ..., driver = detect.driver(dsn), 
		options = character(0), scale_offset = c(1.0, 0.0), type = "Float32", NA_value = NA_real_, 
		chunk_size = c(dim(obj)[1], floor(25e6 / dim(obj)[1])), progress = TRUE) {

	if (!missing(layer))
		obj = obj[layer]

	cl = attr(obj, "call_list")
	if (is.null(cl) && (length(obj[[1]]) > 1 || length(obj) > 1)) { # collapse bands:
		out_file = tempfile(fileext = ".vrt")
		gdal_utils("buildvrt", unlist(obj), out_file, options = "-separate")
		obj[[1]] = out_file
	}

	if (progress) {
		pb = txtProgressBar()
		setTxtProgressBar(pb, 0)
	}

	dim_obj = dim(obj)
	if (prod(chunk_size) > prod(dim_obj[1:2]))
		write_stars(st_as_stars(obj), dsn, layer, ..., driver = driver, options = options,
			scale_offset = scale_offset, type = type, NA_value = NA_value)
	else { # write chunked: https://github.com/r-spatial/stars/pull/291/files

		di_write = di_read = st_dimensions(obj)
		di_from = c(di_read[[1]]$from, di_read[[2]]$from)
		created = FALSE
		ncol = ceiling(dim_obj[1] / chunk_size[1])
		nrow = ceiling(dim_obj[2] / chunk_size[2])
		for (col in 1:ncol) {
			di_write[[1]]$from = 1 + (col - 1) * chunk_size[1]
			di_write[[1]]$to   = min(col * chunk_size[1], dim_obj[1])
			di_read[[1]]$from = di_write[[1]]$from + di_from[1] - 1
			di_read[[1]]$to   = di_write[[1]]$to + di_from[1] - 1
			di_write[[1]]$offset = with(di_read[[1]], offset + delta * (from - 1))
			for (row in 1:nrow) {
				di_write[[2]]$from = 1 + (row - 1) * chunk_size[2]
				di_write[[2]]$to   = min(row * chunk_size[2], dim_obj[2])
				di_read[[2]]$from = di_write[[2]]$from + di_from[2] - 1
				di_read[[2]]$to   = di_write[[2]]$to + di_from[2] -1
				di_write[[2]]$offset = with(di_read[[2]], offset + delta * (from - 1))
				chunk = st_as_stars(structure(obj, dimensions = di_read))
				attr(chunk, "dimensions")[[1]] <- di_write[[1]] # x
				attr(chunk, "dimensions")[[2]] <- di_write[[2]] # y

				if (! created) { # create:
					d = st_dimensions(chunk)
					d_obj = st_dimensions(obj)
					d[[1]]$from = d[[2]]$from = 1
					d[[1]]$to = d[[1]]$from + dim_obj[1] - 1
					d[[2]]$to = d[[2]]$from + dim_obj[2] - 1
					gt = get_geotransform(structure(obj, dimensions = d))
					sf::gdal_write(structure(obj, dimensions = d), ..., file = dsn, driver = driver, options = options,
						type = type, scale_offset = scale_offset, NA_value = NA_value, geotransform = gt) # branches on stars_proxy
				}
				created = TRUE
				write_stars(chunk, dsn = dsn, layer = layer, driver = driver,
					options = options, type = type, scale_offset = scale_offset,
					update = TRUE, NA_value = NA_value)
				if (progress)
					setTxtProgressBar(pb, ((col-1) * nrow + row) / (ncol * nrow))
			}
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
		'GTiff'
	} else {
		switch(ext, 
			tif  = ,
			tiff = 'GTiff',
			grd  = 'rraster',
			nc   = ,
			cdf  = ,
			ncdf = 'NetCDF',
			zarr = 'ZARR',
			kml  = ,
			kmz  = 'KML',
			big  = 'big.matrix',
			sgrd = ,
			sdat = 'SAGA',
			bil  = 'BIL',
			bsq  = 'BSQ',
			bip  = 'BIP',
			bmp  = 'BMP',
			gen  = 'ADRG',
			bt   = 'BT',
			envi = 'ENVI',
			ers  = 'ERS',
			img  = 'HFA',
			rst  = 'RST',
			mpr  = 'ILWIS',
			rsw  = 'RMF',
			flt  = 'EHdr',
			gpkg = 'GPKG',
			{
				warning("extension ", ext, " is unknown. Using default driver GTiff.")
				'GTiff'
			}
		)
		}
}
#nocov end

