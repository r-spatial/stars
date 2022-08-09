#' Read data using GDAL's multidimensional array API
#' 
#' Read data using GDAL's multidimensional array API
#' @param x data source name
#' @param variable name of the array to be read
#' @param options array opening options
#' @param raster names of the raster variables (default: first two)
#' @param offset integer; offset for each dimension (pixels) of sub-array to read (default: 0,0,0,...) (requires sf >= 1.0-9)
#' @param count integer; size for each dimension (pixels) of sub-array to read (default: read all) (requires sf >= 1.0-9)
#' @param step integer; step size for each dimension (pixels) of sub-aray to read (requires sf >= 1.0-9)
#' @param proxy logical; return proxy object? (not functional yet)
#' @param debug logical; print debug info?
#' @details it is assumed that the first two dimensions are easting / northing
#' @param ... ignored
#' @export
read_mdim = function(x, variable = character(0), ..., options = character(0), raster = NULL,
					 offset = integer(0), count = integer(0), step = integer(0), proxy = FALSE, 
					 debug = FALSE) {

	# when releasing to CRAN, require sf 1.0-9 and drop second option
	ret = if (packageVersion("sf") >= "1.0-9")
			gdal_read_mdim(x, variable, options, rev(offset), rev(count), rev(step), proxy, debug)
		else
			gdal_read_mdim(x, variable, options)

#	if (packageVersion("sf") >= "1.0-9") {
#		message("update stars to > 0.5-6")
#		return(NULL)
#	}
#	ret = gdal_read_mdim(x, variable, options)
	create_units = function(x) {
		u <- attr(x, "units")
		if (is.null(u) || u == "")
			x
		else {
			if (!is.null(a <- attr(x, "attributes")) && !is.na(cal <- a["calendar"]) && 
						cal %in% c("360_day", "365_day", "noleap"))
				get_pcict(x, u, cal)
			else {
				u = units::set_units(x, u, mode = "standard")
				p = try(as.POSIXct(u), silent = TRUE)
				if (inherits(p, "POSIXct"))
					p
				else
					u
			}
		}
	}
	l = rev(lapply(ret$dimensions, function(x) create_units(x$values[[1]])))
	if (length(offset) != 0 || length(step) != 0 || length(count) != 0) {
		if (length(offset) == 0)
			offset = rep(0, length(l))
		if (length(step) == 0)
			step = rep(1, length(l))
		if (length(count) == 0)
			count = floor((lengths(l) - offset)/step)
		for (i in seq_along(l)) {
			l[[i]] = l[[i]][seq(from = offset[i]+1, length.out = count[i], by = step[i])]
		}
	}
	d = mapply(function(x, i) create_dimension(values = x, is_raster = i %in% 1:2), l, seq_along(l),
			SIMPLIFY = FALSE)
	if (is.null(raster))
		raster = get_raster(dimensions = names(d)[1:2])
	else
		raster = get_raster(dimensions = raster)

	if (proxy)
		stop("proxy not yet implemented in read_mdim()")

	# handle array units:
	for (i in seq_along(ret$array_list))
		if (nchar(u <- attr(ret$array_list[[i]], "units")))
			ret$array_list[[i]] = units::set_units(ret$array_list[[i]], u, mode = "standard")
	lst = lapply(ret$array_list, function(x) structure(x, dim = rev(dim(x))))
	st_set_crs(st_stars(lst, dimensions = structure(d, raster = raster, class = "dimensions")),
		ret$srs)
}

add_attr = function(x, at) { # append at to attribute "attrs"
		structure(x, attrs = c(attr(x, "attrs"), at))
}

add_units_attr = function(l) {
		f = function(x) {
			if (inherits(x, "units"))
				add_attr(x, c(units = as.character(units(x))))
			else if (inherits(x, c("POSIXct", "PCICt"))) {
				cal = if (!is.null(cal <- attr(x, "cal")))
					c(calendar = paste0(cal, "_day")) # else NULL, intended
				if (all(as.numeric(x) %% 86400 == 0))
					add_attr(as.numeric(x)/86400, c(units = "days since 1970-01-01", cal))
				else if (all(as.numeric(x) %% 3600 == 0))
					add_attr(as.numeric(x)/3600, c(cal, units = "hours since 1970-01-01 00:00:00", cal))
				else
					add_attr(x, c(units = "seconds since 1970-01-01 00:00:00", cal))
			} else if (inherits(x, "Date"))
				add_attr(x, c(units = "days since 1970-01-01"))
			else
				x
		}
		lapply(l, f)
}

#' Write stars object using GDAL multidimensional array interface
#'
#' Write stars object using GDAL multidimensional array interface
#' @param x stars object 
#' @param filename destination file name
#' @param driver character; driver name
#' @param ... ignored
#' @export
write_mdim = function(x, filename, driver = detect.driver(filename), ...) {
	d = st_dimensions(x)
	curvilinear = character(0)
	wkt = if (is.na(st_crs(x)))
			character(0)
		else
			st_crs(x)$wkt
	e = add_units_attr(expand_dimensions(d))
	r = add_units_attr(x) # unclasses, so that r$lat <- ... doesn't use the $<-.stars method
	if (is_curvilinear(x)) {
		stopifnot(!inherits(r, "stars"))
		# att lat and lon as data arrays
		arrs = names(r)
		r$lat = d$y$values # FIXME: could overwrite? use raster attr to identify x/y?
		r$lon = d$x$values
		d$x$values = numeric(0)
		d$y$values = numeric(0)
		r[["lat"]] = add_attr(r[["lat"]], c(units = "degrees_north", "_CoordinateAxisType" = "Lat", axis = "Y"))
		r[["lon"]] = add_attr(r[["lon"]], c(units = "degrees_east",  "_CoordinateAxisType" = "Lon", axis = "X"))
		cc = paste(rev(setdiff(names(d), c("x", "y", "lat", "lon"))), "lat lon")
		for (i in arrs)
			r[[i]] = add_attr(r[[i]], c(coordinates = cc))
		curvilinear = c("lon", "lat")
		e$x = e$y = numeric(0);
	} else {
		# FIXME: use raster attr to identify x/y:
		e[[1]] = add_attr(e[[1]], c(axis = "X"))
		e[[2]] = add_attr(e[[2]], c(axis = "Y"))
	}

	ret = sf:::write_mdim(filename, driver, r, d, e, wkt, curvilinear)
	invisible(ret)
}

write_mdim_old = function(x, filename, ...) {

	stopifnot(inherits(x, "stars"), is.character(filename), length(filename) == 1)
	if (length(x) > 1) {
		warning("only writing of the first attribute is supported")
		x = x[1]
	}
	x = st_upfront(x)
	d = st_dimensions(x)
	e = expand_dimensions(d)
	mdi = character(0) # metadataitems
	mdi["NETCDF_VARNAME"] = names(x)[1] # doesn't help!
	if (inherits(x[[1]], "units"))
		mdi["Band1#units"] = as.character(units(x[[1]]))
	if (length(d) > 2) {
		mdi["NETCDF_DIM_EXTRA"] = paste0("{", paste0(rev(names(d)[-(1:2)]), collapse=","), "}")
		for (i in setdiff(seq_along(d), 1:2)) {
			name = names(d)[i]
			mdi[paste0("NETCDF_DIM_", name, "_DEF")] = paste0("{", length(e[[i]]), ",6}")
			values = paste0(as.numeric(e[[i]]), collapse = ",")
			if (name == "time" || inherits(e[[i]], c("POSIXt", "Date", "PCICt"))) {
				mdi["time#axis"] = "T"
				if (inherits(e[[i]], c("POSIXt", "PCICt"))) {
					if (all(as.numeric(e[[i]]) %% 86400 == 0)) {
						values = paste0(as.numeric(e[[i]])/86400, collapse = ",")
						mdi["time#units"] = "days since 1970-01-01"
					} else
						mdi["time#units"] = "seconds since 1970-01-01 00:00:00"
					if (inherits(e[[i]], "PCICt")) {
						if (!is.null(cal <- attr(e[[i]], "cal")))
							mdi["time#calendar"] = paste0(cal, "_day")
					}
				}
				if (inherits(e[[i]], "Date"))
					mdi["time#units"] = "days since 1970-01-01"
			}
			mdi[paste0("NETCDF_DIM_", name, "_VALUES")] = paste0("{", values, "}")
			# handle Z?
		}
	}
	wkt = if (is.na(st_crs(x)))
			character(0)
		else
			st_crs(x)$wkt
	gt = get_geotransform(d)
	sf::gdal_write_mdim_old(x, filename, mdi, wkt, gt)
	invisible(x)
}
