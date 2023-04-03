maybe_normalizePath = function(.x, np = FALSE) {
	prefixes = c("NETCDF:", "HDF5:", "HDF4:", "HDF4_EOS:", "SENTINEL2_L1", "SENTINEL2_L2", "GPKG:", "/vsi", "http://", "https://")
	has_prefix = function(pf, x) substr(x, 1, nchar(pf)) == pf
	if (is.function(.x) || !np || any(sapply(prefixes, has_prefix, x = .x)))
		.x
	else
		setNames(normalizePath(.x, mustWork = FALSE), names(.x))
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

is_big = function(x, ..., sub = sub, n_proxy) {
	prod(dim(read_stars(x, ..., sub = sub, proxy = TRUE, quiet = TRUE))) > n_proxy
}

# strip common start & end parts of x, if remainder != ""
unique_part = function(x, prefix) {
	stopifnot(is.character(x), length(x) > 1)
	original = x
	while (nchar(x[1]) && length(unique(substr(x, 1, 1))) == 1)
		x = sapply(x, substring, 2)
	while ((n <- nchar(x[1])) && length(unique(substr(x, n, n))) == 1)
		x = sapply(x, substring, 1, n - 1)
	if (any(x == ""))
		original
	else if (is.character(prefix))
		paste0(prefix, x)
	else
		x
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
#' If set to a number, defaults to \code{TRUE} when the number of cells to be read is larger than that number.
#' @param curvilinear length two character vector with names of subdatasets holding longitude and latitude values for all raster cells, or named length 2 list holding longitude and latitude matrices; the names of this list should correspond to raster dimensions referred to
#' @param normalize_path logical; if \code{FALSE}, suppress a call to \link{normalizePath} on \code{.x}
#' @param RAT character; raster attribute table column name to use as factor levels
#' @param tolerance numeric; passed on to \link{all.equal} for comparing dimension parameters.
#' @param ... passed on to \link{st_as_stars} if \code{curvilinear} was set
#' @param exclude character; vector with category value(s) to exclude
#' @param shorten logical or character; if \code{TRUE} and \code{length(.x) > 1}, remove common start and end parts of array names; if character a new prefix
#' @return object of class \code{stars}
#' @details In case \code{.x} contains multiple files, they will all be read and combined with \link{c.stars}. Along which dimension, or how should objects be merged? If \code{along} is set to \code{NA} it will merge arrays as new attributes if all objects have identical dimensions, or else try to merge along time if a dimension called \code{time} indicates different time stamps. A single name (or positive value) for \code{along} will merge along that dimension, or create a new one if it does not already exist. If the arrays should be arranged along one of more dimensions with values (e.g. time stamps), a named list can passed to \code{along} to specify them; see example.
#'
#' \code{RasterIO} is a list with zero or more of the following named arguments:
#' \code{nXOff}, \code{nYOff} (both 1-based: the first row/col has offset value 1),
#' \code{nXSize}, \code{nYSize}, \code{nBufXSize}, \code{nBufYSize}, \code{bands}, \code{resample}.
#' See \url{https://gdal.org/doxygen/classGDALDataset.html} for their meaning;
#' \code{bands} is an integer vector containing the band numbers to be read (1-based: first band is 1).
#' Note that if \code{nBufXSize} or \code{nBufYSize} are specified for downsampling an image,
#' resulting in an adjusted geotransform. \code{resample} reflects the resampling method and
#' has to be one of: "nearest_neighbour" (the default),
#' "bilinear", "cubic", "cubic_spline", "lanczos", "average", "mode", or "Gauss".
#'
#' Data that are read into memory (\code{proxy=FALSE}) are read into a numeric (double) array, except for categorical variables which are read into an numeric (integer) array of class \code{factor}.
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
read_stars = function(.x, sub = TRUE, ..., options = character(0), 
		driver = character(0), quiet = FALSE, NA_value = NA_real_, along = NA_integer_,
		RasterIO = list(), proxy = getOption("stars.n_proxy") %||% 1.e8,
		curvilinear = character(0), normalize_path = TRUE, RAT = character(0),
		tolerance = 1e-10, exclude = "", shorten = TRUE) {

	is_false = function(x) { length(x) == 1 && is.logical(x) && !x }
	if (!is.list(.x) && length(.x) > 1 && is.null(names(.x)) && !is_false(shorten) &&
				any((short_names <- unique_part(.x, shorten)) != .x))
		names(.x) = short_names
	if (is.numeric(proxy)) {
		if (is_functions(.x))
			proxy = TRUE
		else if (length(curvilinear))
			proxy = FALSE
	}

	x = if (is.list(.x)) {
			f = function(y, np) enc2utf8char(maybe_normalizePath(y, np))
			rapply(.x, f, classes = c("character", "function"), how = "replace", np = normalize_path)
		} else
			enc2utf8char(maybe_normalizePath(.x, np = normalize_path))

	if (missing(curvilinear)) { # see if we can get it from metadata item "GEOLOCATION":
		geolocation <- try(gdal_metadata(.x, "GEOLOCATION"), silent = TRUE)
		if (!inherits(geolocation, "try-error")) { # the thing has x and y arrays:
			# read coordinate arrays:
			gxy = c(geolocation$X_DATASET, geolocation$Y_DATASET)
			lon = adrop(read_stars(gxy[1], driver = driver, quiet = quiet, NA_value = NA_value,
				RasterIO = RasterIO, proxy = FALSE, curvilinear = character(0), ...))
			lat = adrop(read_stars(gxy[2], driver = driver, quiet = quiet, NA_value = NA_value,
				RasterIO = RasterIO, proxy = FALSE, curvilinear = character(0), ...))
			if (all(dim(lon) > 1) && all(dim(lat) > 1)) # both are 2-D matrices:
				curvilinear = setNames(c(st_set_dimensions(lon, names = c("x", "y")),
					st_set_dimensions(lat, names = c("x", "y"))), c("x", "y"))
			else {
				ulon = unique(diff(as.vector(lon[[1]])))
				ulat = unique(diff(as.vector(lat[[1]])))
				if (length(ulon) > 1 || length(ulat) > 1)
					warning("ignoring irregular (rectilinear) x and/or y coordinates!")
			}
		}
	} else if (length(curvilinear) == 2 && is.character(curvilinear)) { # user-set, read them
		lon = adrop(read_stars(.x, sub = curvilinear[1], driver = driver, quiet = quiet, NA_value = NA_value,
			RasterIO = RasterIO, proxy = FALSE, ..., sub_required = TRUE))
		lat = adrop(read_stars(.x, sub = curvilinear[2], driver = driver, quiet = quiet, NA_value = NA_value,
			RasterIO = RasterIO, proxy = FALSE, ..., sub_required = TRUE))
		curvilinear = setNames(c(st_set_dimensions(lon, names = c("x", "y")),
			st_set_dimensions(lat, names = c("x", "y"))), c("x", "y"))
	}
	if (length(curvilinear) && proxy)
		warning("proxy = TRUE may not work for curvilinear rasters")

	if (length(x) > 1) { # loop over data sources, read, c() all, and RETURN:
		ret = lapply(x, read_stars, options = options, driver = driver, sub = sub, quiet = quiet,
			NA_value = NA_value, RasterIO = as.list(RasterIO), proxy = proxy, curvilinear = curvilinear,
			along = if (length(along) > 1) along[-1] else NA_integer_, normalize_path = normalize_path)
		return(do.call(c, append(ret, list(along = along, tolerance = tolerance))))
	}

	# else: do it --
	data = sf::gdal_read(get_names(x),
		options = options, driver = driver, read_data = ifelse(is.logical(proxy), !proxy, proxy),
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
	} else { # we have one single array: read it
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
		if (is.numeric(proxy) && !is.null(data$proxy))
			proxy = data$proxy
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
		if (meta_data$driver[1] == "AIG" && identical(names(at[[1]]), c("VALUE", "COUNT")))
			at = list() # skip it: https://github.com/r-spatial/stars/issues/435
		# FIXME: how to handle multiple color, category or attribute tables?
		if (!proxy && (any(lengths(ct) > 0) || any(lengths(at) > 0))) {
			if (any(meta_data$ranges[1, c(2,4)] == 1)) { # adjust data to min/max values from image metadata
				r = range(data, na.rm = TRUE)
				min_value = if (meta_data$ranges[1,2] == 1)
						meta_data$ranges[1,1]
					else
						r[1]
				max_value = if (meta_data$ranges[1,4] == 1)
						meta_data$ranges[1,3]
					else
						r[2]
				data[data < min_value | data > max_value] = NA
			}

			# convert color table ct to a vector of R colors:
			co = if (any(lengths(ct) > 0)) {
				ct = ct[[ which(length(ct) > 0)[1] ]]
				apply(ct, 1, function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue = 255))
			} else
				NULL

			if (any(lengths(at) > 0)) {
				# select attribute table:
				which.at = which(lengths(at) > 0)[1]
				which.column = if (length(RAT))
						RAT[1]
					else
						which(sapply(at[[which.at]], class) == "character")[1]
				if (length(which.column) == 0)
					which.column = 1
				labels = at = at[[ which.at ]][[ which.column ]]
				levels = 0:(length(at) - 1)
				if (length(exclude)) {
					ex = at %in% exclude
					labels = labels[!ex]
					levels = levels[!ex]
					if (!is.null(co))
						co = co[!ex]
				} else
					ex = rep(FALSE, length(levels))
			} else {
				levels = sort(unique(as.vector(data)))
				labels = as.character(levels)
				if (!is.null(co))
					co = co[levels + 1]
				ex = rep(FALSE, length(levels))
			}
			# f = factor(as.vector(data), levels = levels, labels = labels) # is too costly;
			# see https://github.com/r-spatial/stars/issues/565:
			# construct factor array manually:
			data = structure(match(as.integer(as.vector(data)), levels),
							 levels = labels, dim = dim(data), 
							 colors = co, exclude = ex, class = "factor")
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
		name_x = if (!is.null(meta_data$long_name) && !is.na(meta_data$long_name))
				meta_data$long_name
			else if (is.function(.x))
				names(.x()) %||% .x()
			else
				x
		name_x = tail(strsplit(name_x, '[\\\\/:]+')[[1]], 1)
		ret = if (proxy) { # no data present, subclass of "stars":
				dims = create_dimensions_from_gdal_meta(dims, meta_data)
				st_stars_proxy(setNames(list(x), names(.x) %||% name_x), dims,
					NA_value = NA_value, resolutions = NULL, RasterIO = RasterIO, 
					file_dim = matrix(dim(dims), 1, dimnames = list(NULL, names(dims))))
			} else
				st_stars(setNames(list(data), names(.x) %||% name_x),
					create_dimensions_from_gdal_meta(dim(data), meta_data))

		if (is.list(curvilinear))
			st_as_stars(ret, curvilinear = curvilinear, ...)
		else
			ret
	}
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
