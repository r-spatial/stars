maybe_normalizePath = function(.x, np = FALSE) {
	prefixes = c("NETCDF:", "HDF5:", "HDF4:", "HDF4_EOS:", "SENTINEL2_L1", "SENTINEL2_L2", "GPKG:")
	has_prefix = function(pf, x) substr(x, 1, nchar(pf)) == pf
	if (is.function(.x) || !np || any(sapply(prefixes, has_prefix, x = .x)))
		.x
	else
		normalizePath(.x, mustWork = FALSE)
}

enc2utf8char = function(x) {
	if (is.character(x))
		enc2utf8(x)
	else
		x
}

get_names = function(x) {
	if (is.function(x))
		x()
	else
		x
}

is_functions = function(x) {
	is.function(x) || all(sapply(x, is.function))
}


#' read raster/array dataset from file or connection
#'
#' read raster/array dataset from file or connection
#' @param .x character vector with name(s) of file(s) or data source(s) to be read, or a function that returns such a vector
#' @param options character; opening options
#' @param driver character; driver to use for opening file. To override fixing for subdatasets and autodetect them as well, use \code{NULL}.
#' @param sub character, integer or logical; name, index or indicator of sub-dataset(s) to be read
#' @param quiet logical; print progress output?
#' @param NA_value numeric value to be used for conversion into NA values; by default this is read from the input file
#' @param along length-one character or integer, or list; determines how several arrays are combined, see Details.
#' @param RasterIO list with named parameters for GDAL's RasterIO, to further control the extent, resolution and bands to be read from the data source; see details.
#' @param proxy logical; if \code{TRUE}, an object of class \code{stars_proxy} is read which contains array
#' metadata only; if \code{FALSE} the full array data is read in memory. Always \code{FALSE} for curvilinear girds.
#' If not set, defaults to \code{TRUE} when the number of cells to be read is larger than \code{options(stars.n_proxy},
#' or to 1e8 if that option was not set.
#' @param curvilinear length two character vector with names of subdatasets holding longitude and latitude values for all raster cells, or named length 2 list holding longitude and latitude matrices; the names of this list should correspond to raster dimensions referred to
#' @param normalize_path logical; if \code{FALSE}, suppress a call to \link{normalizePath} on \code{.x}
#' @param RAT character; raster attribute table column name to use as factor levels
#' @param tolerance numeric; passed on to \link{all.equal} for comparing dimension parameters.
#' @param ... passed on to \link{st_as_stars} if \code{curvilinear} was set
#' @return object of class \code{stars}
#' @details In case \code{.x} contains multiple files, they will all be read and combined with \link{c.stars}. Along which dimension, or how should objects be merged? If \code{along} is set to \code{NA} it will merge arrays as new attributes if all objects have identical dimensions, or else try to merge along time if a dimension called \code{time} indicates different time stamps. A single name (or positive value) for \code{along} will merge along that dimension, or create a new one if it does not already exist. If the arrays should be arranged along one of more dimensions with values (e.g. time stamps), a named list can passed to \code{along} to specify them; see example.
#'
#' \code{RasterIO} is a list with zero or more of the following named arguments:
#' \code{nXOff}, \code{nYOff} (both 1-based: the first row/col has offset value 1),
#' \code{nXSize}, \code{nYSize}, \code{nBufXSize}, \code{nBufYSize}, \code{bands}, code{resample}.
#' see https://www.gdal.org/classGDALDataset.html for their meaning;
#' \code{bands} is an integer vector containing the band numbers to be read (1-based: first band is 1)
#' Note that if \code{nBufXSize} or \code{nBufYSize} are specified for downsampling an image,
#' resulting in an adjusted geotransform. \code{resample} reflects the resampling method and
#' has to be one of: "nearest_neighbour" (the default),
#' "bilinear", "cubic", "cubic_spline", "lanczos", "average", "mode", or "Gauss".
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' (x1 = read_stars(tif))
#' (x2 = read_stars(c(tif, tif)))
#' (x3 = read_stars(c(tif, tif), along = "band"))
#' (x4 = read_stars(c(tif, tif), along = "new_dimensions")) # create 4-dimensional array
#' x1o = read_stars(tif, options = "OVERVIEW_LEVEL=1")
#' t1 = as.Date("2018-07-31")
#' # along is a named list indicating two dimensions:
#' read_stars(c(tif, tif, tif, tif), along = list(foo = c("bar1", "bar2"), time = c(t1, t1+2)))
#'
#' m = matrix(1:120, nrow = 12, ncol = 10)
#' dim(m) = c(x = 10, y = 12) # named dim
#' st = st_as_stars(m)
#' attr(st, "dimensions")$y$delta = -1
#' attr(st, "dimensions")$y$offset = 12
#' st
#' tmp = tempfile(fileext = ".tif")
#' write_stars(st, tmp)
#' (red <- read_stars(tmp))
#' read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 12,
#'    nBufXSize = 2, nBufYSize = 2))[[1]]
#' (red <- read_stars(tmp, RasterIO = list(nXOff = 1, nYOff = 1, nXSize = 10, nYSize = 12,
#'    nBufXSize = 2, nBufYSize = 2)))
#' red[[1]] # cell values of subsample grid:
#' \dontrun{
#'   plot(st, reset = FALSE, axes = TRUE, ylim = c(-.1,12.1), xlim = c(-.1,10.1),
#'     main = "nBufXSize & nBufYSize demo", text_values = TRUE)
#'   plot(st_as_sfc(red, as_points = TRUE), add = TRUE, col = 'red', pch = 16)
#'   plot(st_as_sfc(st_as_stars(st), as_points = FALSE), add = TRUE, border = 'grey')
#'   plot(st_as_sfc(red, as_points = FALSE), add = TRUE, border = 'green', lwd = 2)
#' }
#' file.remove(tmp)
read_stars = function(.x, ..., options = character(0), driver = character(0),
		sub = TRUE, quiet = FALSE, NA_value = NA_real_, along = NA_integer_,
		RasterIO = list(), proxy = is_functions(.x) || (!length(curvilinear) && 
				is_big(.x, sub = sub, driver=driver, normalize_path = normalize_path, ...)),
		curvilinear = character(0), normalize_path = TRUE, RAT = character(0),
		tolerance = 1e-10) {

	x = if (is.list(.x)) {
			f = function(y, np) enc2utf8char(maybe_normalizePath(y, np))
			rapply(.x, f, classes = c("character", "function"), how = "replace", np = normalize_path)
		} else
			enc2utf8char(maybe_normalizePath(.x, np = normalize_path))

	if (length(curvilinear) == 2 && is.character(curvilinear)) {
		lon = adrop(read_stars(.x, sub = curvilinear[1], driver = driver, quiet = quiet, NA_value = NA_value,
			RasterIO = RasterIO, proxy = FALSE, ..., sub_required = TRUE))
		lat = adrop(read_stars(.x, sub = curvilinear[2], driver = driver, quiet = quiet, NA_value = NA_value,
			RasterIO = RasterIO, proxy = FALSE, ..., sub_required = TRUE))
		curvilinear = setNames(c(st_set_dimensions(lon, names = c("x", "y")),
			st_set_dimensions(lat, names = c("x", "y"))), c("x", "y"))
	}

	if (length(x) > 1) { # loop over data sources and RETURNS:
		ret = lapply(x, read_stars, options = options, driver = driver, sub = sub, quiet = quiet,
			NA_value = NA_value, RasterIO = as.list(RasterIO), proxy = proxy, curvilinear = curvilinear,
			along = if (length(along) > 1) along[-1] else NA_integer_)
		return(do.call(c, append(ret, list(along = along, tolerance = tolerance))))
	}

	# else:
	data = sf::gdal_read(get_names(x),
		options = options, driver = driver, read_data = !proxy,
		NA_value = NA_value, RasterIO_parameters = as.list(RasterIO))
	if (!is.null(data$default_geotransform) && data$default_geotransform == 1) {
		## we have the 0 1 0 0 0 1 transform indicated
		## so stars policy is flip-y and shift to be in 0, ncol, 0, nrow
		data$geotransform <- c(0, 1, 0, data$rows[2L], 0, -1)
	}
	if (length(data$bands) == 0) { # read sub-datasets: different attributes
		sub_names = split_strings(data$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		# sub_datasets = gdal_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only tested for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)
		names(sub_datasets) = nms
		if (any(sapply(sub_datasets[sub], is.null)))
			sub = sub("^//", "/", sub) # GDAL2->3, HDF5, double to single slash?
		sub_datasets = sub_datasets[sub]
		nms = names(sub_datasets)

		.read_stars = function(x, options, driver, quiet, proxy, curvilinear) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			read_stars(x, options = options, driver = driver, NA_value = NA_value,
				RasterIO = as.list(RasterIO), proxy = proxy, curvilinear = curvilinear)
		}

		driver = if (is.null(driver) || data$driver[1] == "HDF5") # to override auto-detection:
				character(0)
			else
				data$driver[1]

		ret = lapply(sub_datasets, .read_stars, options = options,
			driver = driver, quiet = quiet, proxy = proxy, curvilinear = curvilinear)
		if (! quiet)
			cat("\n")
		# return:
		if (length(ret) == 1)
			ret[[1]]
		else {
			ret = do.call(c, append(ret, list(try_hard = TRUE, nms = nms)))
			if (length(nms) == length(ret)) # lost my ability to solve this here...
				setNames(ret, nms)
			else
				ret
		}
	} else { # we have one single array:
		if (!isTRUE(sub)) {
			sub_required = if (is.null(list(...)$sub_required))
					FALSE
				else
					list(...)$sub_required
			if (sub_required)
				stop(paste("only one array present in", .x, ": cannot resolve subdataset", sub))
			else
				warning("only one array present: argument 'sub' will be ignored")
		}
		meta_data = structure(data, data = NULL) # take meta_data only
		data = if (proxy)
				.x
			else
				get_data_units(attr(data, "data")) # extract data array; sets units if present
		if (meta_data$driver[1] == "netCDF")
			meta_data = parse_netcdf_meta(meta_data, get_names(x)) # sets all kind of units
		if (! proxy && !is.null(meta_data$units) && !is.na(meta_data$units)
				&& !inherits(data, "units")) # set units
			units(data) = try_as_units(meta_data$units)
		meta_data = parse_gdal_meta(meta_data)

		newdims = lengths(meta_data$dim_extra)
		if (length(newdims) && !proxy)
			dim(data) = c(dim(data)[1:2], newdims)

		# handle color table and/or attribute table
		ct = meta_data$color_tables
		at = meta_data$attribute_tables
		# FIXME: how to handle multiple color, category or attribute tables?
		if (!proxy && (any(lengths(ct) > 0) || any(lengths(at) > 0))) {
			min_value = if (!is.null(meta_data$ranges) && meta_data$ranges[1,2] == 1)
					meta_data$ranges[1,1]
				else
					min(data, na.rm = TRUE)
			data[data < min_value] = NA
			if (min_value < 0)
				stop("categorical values should have minimum value >= 0")
			if (any(lengths(ct) > 0)) {
				ct = ct[[ which(length(ct) > 0)[1] ]]
				co = apply(ct, 1, function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue = 255))
				if (min_value > 0)
					co = co[-seq_len(min_value)] # removes [0,...,(min_value-1)]
				f = factor(as.vector(data), levels = seq(min_value, length.out = length(co)))
				data = structure(f, dim = dim(data), colors = co)
			}
			if (any(lengths(at) > 0)) {
				which.at = which(lengths(at) > 0)[1]
				which.column = if (length(RAT))
						RAT
					else
						which(sapply(at[[which.at]], class) == "character")[1]
				at = at[[ which.at ]][[ which.column ]]
				if (min_value > 0)
					at = at[-seq_len(min_value)]
				if (!is.factor(data) && min_value == 0) {
					data = data + 1
					warning("categorical data values starting at 0 are shifted with one to start at 1")
				}
				attr(data, "levels") = at
			}
			data = structure(data, class = "factor")
		}

		dims = if (proxy) {
				if (length(meta_data$bands) > 1)
					c(x = meta_data$cols[2],
					  y = meta_data$rows[2],
					  band = length(meta_data$bands),
					  lengths(meta_data$dim_extra))
				else
					c(x = meta_data$cols[2],
					  y = meta_data$rows[2],
					  lengths(meta_data$dim_extra))
			} else
				NULL

		# return:
		name_x = if (is.function(.x))
				names(.x()) %||% .x()
			else
				x
		ret = if (proxy) { # no data present, subclass of "stars":
				st_stars_proxy(setNames(list(x), names(.x) %||% tail(strsplit(name_x, '[\\\\/]+')[[1]], 1)),
					create_dimensions_from_gdal_meta(dims, meta_data), NA_value = NA_value,
					resolutions = NULL)
			} else
				st_stars(setNames(list(data), names(.x) %||% tail(strsplit(name_x, '[\\\\/:]+')[[1]], 1)),
					create_dimensions_from_gdal_meta(dim(data), meta_data))
	
		if (is.list(curvilinear))
			st_as_stars(ret, curvilinear = curvilinear, ...)
		else
			ret
	}
}

#' @export
#' @name read_stars
#' @param x object to be read with \link{read_stars}
#' @param n_proxy integer; number of cells above which .x will be read as stars
#' proxy object, i.e. not as in-memory arrays but left on disk
is_big = function(x, ..., sub = sub, n_proxy = options("stars.n_proxy")[[1]] %||% 1.e8) {
	prod(dim(read_stars(x, ..., sub = sub, proxy = TRUE, quiet = TRUE))) > n_proxy
}

get_data_units = function(data) {
	units = unique(attr(data, "units")) # will fail parsing in as_units() when more than one
	if (length(units) > 1) {
		warning(paste("more than one unit available for array: ignoring all")) # nocov
		structure(data, units = NULL)
	} else {
		if (!is.null(units) && nzchar(units)) {
			if (units == "arc-second") # datum grids may have this
				units = "arcsecond"
			units = try_as_units(units)
		}
		if (inherits(units, "units"))
			units::set_units(structure(data, units = NULL), units, mode = "standard")
		else
			structure(data, units = NULL)
	}
}

read_mdim = function(x, variable = character(0), ..., options = character(0), raster = NULL) {
	if (packageVersion("sf") <= "0.9-8")
		gdal_read_mdim = function(...) stop("sf > 0.9-8 required")
	ret = gdal_read_mdim(x, variable, options)
	create_units = function(x) {
		u <- attr(x, "units")
		if (is.null(u) || u == "")
			x 
		else {
			u = units::set_units(x, u, mode = "standard")
			p = try(as.POSIXct(u), silent = TRUE)
			if (inherits(p, "POSIXct"))
				p
			else
				u
		}
	}
	l = lapply(ret$dimensions, function(x) create_units(x$values[[1]]))
	d = rev(lapply(l, function(x) create_dimension(values = x)))
	if (is.null(raster))
		raster = get_raster(dimensions = names(d)[1:2])
	lst = lapply(ret$array_list, function(x) structure(x, dim = rev(dim(x))))
	st_set_crs(st_stars(lst, dimensions = structure(d, raster = raster, class = "dimensions")),
		ret$srs)
}

write_mdim = function(x, filename, ...) {
	stopifnot(inherits(x, "stars"))
	if (packageVersion("sf") <= "0.9-8")
		gdal_write_mdim = function(...) stop("sf > 0.9-8 required")
	to_units = function(x) if (inherits(x, c("POSIXct", "Date"))) units::as_units(x) else x
	dimension_values = rev(lapply(expand_dimensions(x), to_units))
	units = sapply(dimension_values, function(x) if(inherits(x, "units")) as.character(units(x)) else "")
	gdal_write_mdim(st_as_stars(x), filename, dimension_values, units)
	invisible(x)
}
