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
					add_attr(as.numeric(x)/3600, c(units = "hours since 1970-01-01 00:00:00", cal))
				else
					add_attr(x, c(units = "seconds since 1970-01-01 00:00:00", cal))
			} else if (inherits(x, "Date"))
				add_attr(x, c(units = "days since 1970-01-01"))
			else
				x
		}
		lapply(l, f)
}

cdl_add_polygons = function(e, i, sfc) {
	stopifnot(inherits(sfc, "sfc_MULTIPOLYGON"))
	cc = st_coordinates(sfc)
	e$node = structure(seq_len(nrow(cc)), dim = c(node = nrow(cc)))
	e$x = structure(cc[, 1], dim = c(node = nrow(cc)))
	e$y = structure(cc[, 2], dim = c(node = nrow(cc)))
 # int node_count(instance) ;
 # int part_node_count(part) ;
 # int interior_ring(part) ;
	e$node_count = structure(rle(cc[,"L3"])$lengths, dim = setNames(length(sfc), names(e)[i]))
	part = rle(cc[,"L3"] * 2 * max(cc[,"L2"]) + 2 * (cc[,"L2"] - 1) + cc[,"L1"])$lengths
	e$part_node_count = structure(part, dim = c(part = length(part)))
	e$interior_ring = structure(as.numeric(cc[cumsum(part), "L1"] > 1), dim = c(part = length(part)))
	attr(e, "dims") = c(node = nrow(cc), part = length(part))
	e$geometry = add_attr(structure(numeric(0), dim = c("somethingNonEx%isting" = 0)),
		c(geometry_type = "polygon", node_count = "node_count", node_coordinates = "x y",
		  part_node_count = "part_node_count", interior_ring = "interior_ring",
		  grid_mapping = if (!is.na(st_crs(sfc))) "crs" else NULL))
	e
}

# convert stars object into a list of CDL-like variables with named dimensions
st_as_cdl = function(x) {
	e = add_units_attr(expand_dimensions(x)) # TODO: handle sfc dimensions
	d = st_dimensions(x)
	dimx = dim(x)
	xy = attr(st_dimensions(x), "raster")$dimensions
	co = NULL
	for (i in seq_along(e)) {
		if (is.null(dim(e[[i]])) && !is.list(e[[i]]))
			e[[i]] = structure(e[[i]], dim = setNames(length(e[[i]]), names(e)[i]))
		else if (is_curvilinear(x)) { # curvilinear coordinate matrices:
			if (names(e)[i] == xy[1]) names(e)[i] = "lon"
			if (names(e)[i] == xy[2]) names(e)[i] = "lat"
		} else if (inherits(e[[i]], "sfc")) { # vector data cube:
			sfc = e[[i]]
			e[[i]] = structure(seq_along(sfc), dim = setNames(length(sfc), names(e)[i]))
			if (inherits(sfc, "sfc_POINT")) {
				cc = st_coordinates(sfc)
				if (isTRUE(st_is_longlat(x))) {
					e$lon = add_attr(structure(cc[, 1], dim = setNames(nrow(cc), names(e)[i])), 
								 	c(units = "degrees_north", standard_name = "longitude"))
					e$lat = add_attr(structure(cc[, 2], dim = setNames(nrow(cc), names(e)[i])), 
								 	c(units = "degrees_east", standard_name = "latitude"))
					co = c(coordinates = "lat lon")
					xy = c("lon", "lat") # hack
				} else {
					e$x = structure(cc[, 1], dim = setNames(nrow(cc), names(e)[i]))
					e$y = structure(cc[, 2], dim = setNames(nrow(cc), names(e)[i]))
					co = c(coordinates = "x y")
					xy = c("x", "y") # hack
				}
			} else if (inherits(sfc, c("sfc_MULTIPOLYGON", "sfc_POLYGON"))) {
				e = cdl_add_polygons(e, i, st_cast(sfc, "MULTIPOLYGON"))
				dimx = c(dimx, attr(e, "dims"))
				co = c(coordinates = "x y")
				xy = c("x", "y") # hack
			} else
				stop("only support for sfc_POINT so far")
		}
	}

	if (is_curvilinear(x)) {
		e[["lat"]] = add_attr(e[["lat"]], c(units = "degrees_north", "_CoordinateAxisType" = "Lat", axis = "Y"))
		e[["lon"]] = add_attr(e[["lon"]], c(units = "degrees_east",  "_CoordinateAxisType" = "Lon", axis = "X"))
		co = paste(rev(setdiff(names(d), c("x", "y", "lat", "lon"))), "lat lon")
	} else {
		if (!any(is.na(xy))) {
			e[[ xy[1] ]] = add_attr(e[[ xy[1] ]], c(axis = "X"))
			e[[ xy[2] ]] = add_attr(e[[ xy[2] ]], c(axis = "Y"))
		}
	}
	for (i in seq_along(x))
		x[[i]] = add_attr(x[[i]], co)

	x = add_units_attr(x) # unclasses
	for (i in seq_along(x))
		if (is.null(names(dim(x[[i]])))) # FIXME: read_ncdf() doesn't name dim
			names(dim(x[[i]])) = names(d)
	for (i in names(e))
		x[[i]] = e[[i]]
	which_dims = function(a, dimx) {
		m = match(names(dim(a)), names(dimx), nomatch = numeric(0)) - 1
		if (all(is.na(m)))
			integer(0)
		else
			m
	}
	ret = lapply(x, function(a) structure(a, which_dims = which_dims(a, dimx)))
	structure(ret, which_crs = !(names(ret) %in% names(e)), dims = attr(e, "dims"))
}


#' Write stars object using GDAL multidimensional array interface
#'
#' Write stars object using GDAL multidimensional array interface
#' @param x stars object 
#' @param filename destination file name
#' @param driver character; driver name
#' @param root_group_options character; driver specific options regarding the creation of the root group
#' @param options character; driver specific options regarding the creation of the dataset
#' @param ... ignored
#' @export
#' @examples
#' set.seed(135)
#' m = matrix(runif(10), 2, 5)
#' names(dim(m)) = c("stations", "time")
#' times = as.Date("2022-05-01") + 1:5
#' pts = st_as_sfc(c("POINT(0 1)", "POINT(3 5)"))
#' s = st_as_stars(list(Precipitation = m)) |>
#'  st_set_dimensions(1, values = pts) |>
#'  st_set_dimensions(2, values = times)
#' nc = tempfile(fileext=".nc")
#' if (compareVersion(sf_extSoftVersion()["GDAL"], "3.4.0") > -1) {
#'   write_mdim(s, nc)
#'   # try ncdump on the generated file
#'   print(read_mdim(nc))
#' }
write_mdim = function(x, filename, driver = detect.driver(filename), ..., 
					  root_group_options = character(0), options = character(0)) {

	cdl = st_as_cdl(x)
	wkt = if (is.na(st_crs(x)))
			character(0)
		else
			st_crs(x)$wkt
	xy = attr(st_dimensions(x), "raster")$dimensions
	gdal_write_mdim(filename, driver, c(dim(x), attr(cdl, "dims")), cdl, wkt, xy, 
					root_group_options = root_group_options, options = options)
	invisible(x)
}
