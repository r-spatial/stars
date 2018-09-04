split_strings = function(md, split = "=") {
	splt = strsplit(md, split)
	lst = lapply(splt, function(x) if (length(x) <= 1) NA_character_ else x[[2]])
	structure(lst, names = sapply(splt, function(x) x[[1]]), class = "gdal_metadata")
}

#' read raster/array dataset from file or connection
#'
#' read raster/array dataset from file or connection
#' @param .x character vector with name(s) of file(s) or data source(s) to be read
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param sub integer or logical; sub-datasets to be read
#' @param quiet logical; print progress output?
#' @param NA_value numeric value to be used for conversion into NA values; by default this is read from the input file
#' @param along length-one character or integer, or list; determines how several arrays are combined, see Details.
#' @param RasterIO list with named parameters for GDAL's RasterIO, to further control the extent, resolution and bands to be read from the data source; see details.
#' @param proxy logical; if \code{TRUE}, an object of class \code{stars_proxy} is read which contains array metadata only; if \code{FALSE} the full array data is read in memory.
#' @param ... ignored
#' @return object of class \code{stars}
#' @details In case \code{.x} contains multiple files, they will all be read and combined with \link{c.stars}. Along which dimension, or how should objects be merged? If \code{along} is set to \code{NA} it will merge arrays as new attributes if all objects have identical dimensions, or else try to merge along time if a dimension called \code{time} indicates different time stamps. A single name (or positive value) for \code{along} will merge along that dimension, or create a new one if it does not already exist. If the arrays should be arranged along one of more dimensions with values (e.g. time stamps), a named list can passed to \code{along} to specify them; see example.
#'
#' \code{RasterIO} is a list with zero or more of the following named arguments: 
#' \code{nXOff}, \code{nYOff} (both 1-based: the first row/col has offset value 1), 
#' \code{nXSize}, \code{nYSize}, \code{nBufXSize}, \code{nBufYSize}, \code{bands}.
#' see https://www.gdal.org/classGDALDataset.html#a80d005ed10aefafa8a55dc539c2f69da for their meaning;
#' \code{bands} is an integer vector containing the band numbers to be read (1-based: first band is 1)
#' Note that if \code{nBufXSize} or \code{nBufYSize} are specified for downsampling an image, 
#' the resulting, adjusted geotransform may be approximate (this is not clear, right now).
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
read_stars = function(.x, ..., options = character(0), driver = character(0), 
		sub = TRUE, quiet = FALSE, NA_value = NA_real_, along = NA_integer_,
		RasterIO = list(), proxy = FALSE) {

	x = .x
	if (length(x) > 1) { # loop over data sources:
		ret = lapply(x, read_stars, options = options, driver = driver, sub = sub, quiet = quiet,
			NA_value = NA_value, RasterIO = as.list(RasterIO), proxy = proxy)
		dims = length(dim(ret[[1]][[1]]))
		return(do.call(c, append(ret, list(along = along))))
	}

	data = sf::gdal_read(x, options = options, driver = driver, read_data = !proxy, 
		NA_value = NA_value, RasterIO_parameters = as.list(RasterIO))

	if (length(data$bands) == 0) { # read sub-datasets: different attributes
		sub_names = split_strings(data$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		# sub_datasets = gdal_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)
		names(sub_datasets) = nms
		sub_datasets = sub_datasets[sub]

		nms = names(sub_datasets)

		.read_stars = function(x, options, driver, quiet, proxy) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			read_stars(x, options = options, driver = driver, NA_value = NA_value, 
				RasterIO = as.list(RasterIO), proxy = proxy)
		}
		ret = lapply(sub_datasets, .read_stars, options = options, 
			driver = data$driver[1], quiet = quiet, proxy = proxy)
		if (! quiet)
			cat("\n")
		# return:
		if (length(ret) == 1)
			ret[[1]]
		else
			structure(do.call(c, ret), names = nms)
	} else { # we have one single array:
		meta_data = structure(data, data = NULL) # take meta_data only
		data = if (proxy)
				.x # names only
			else
				attr(data, "data") # extract data arrays
		if (meta_data$driver[1] == "netCDF")
			meta_data = parse_netcdf_meta(meta_data, x)
		meta_data = parse_gdal_meta(meta_data)
		if (! proxy && !is.null(meta_data$units) && !is.na(meta_data$units)) # set units
			units(data) = try_as_units(meta_data$units)

		newdims = lengths(meta_data$dim_extra)
		if (length(newdims) && !proxy)
			dim(data) = c(dim(data)[1:2], newdims)

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
		if (proxy) # no data present, subclass of "stars":
			st_stars_proxy(setNames(list(.x), tail(strsplit(x, .Platform$file.sep)[[1]], 1)),
				create_dimensions_from_gdal_meta(dims, meta_data))
		else
			st_stars(setNames(list(data), tail(strsplit(x, .Platform$file.sep)[[1]], 1)),
				create_dimensions_from_gdal_meta(dim(data), meta_data))
	}
}

#' convert objects into a stars object
#' 
#' convert objects into a stars object
#' @export
#' @param .x object to convert
#' @param ... ignored
st_as_stars = function(.x, ...) UseMethod("st_as_stars")

#' @name st_as_stars
#' @param dimensions object of class dimensions
#' @export
st_as_stars.list = function(.x, ..., dimensions = NULL) {
	if (length(.x) > 1) {
		for (i in seq_along(.x)[-1])
			if (!identical(dim(.x[[1]]), dim(.x[[i]])))
				stop("dim attributes not identical")
		if (!is.null(names(.x)))
			names(.x) = make.names(names(.x), unique = TRUE)
	}
	if (is.null(dimensions))
		dimensions = create_dimensions(dim(.x[[1]]))
	st_stars(.x, dimensions)
}

st_stars = function(x, dimensions) {
	# sanity checks:
	stopifnot(is.list(x))
	stopifnot(inherits(dimensions, "dimensions"))
	stopifnot(!is.null(attr(dimensions, "raster")))
	structure(x, dimensions = dimensions, class = "stars")
}


#' @name st_as_stars
#' @export
#' @param raster character; the names of the dimensions that denote raster dimensions
st_as_stars.default = function(.x = NULL, ..., raster = NULL) {
	args = if (is.null(.x))
			list(...)
		else
			append(list(.x), list(...))

	if (length(args) == 0)
		return(st_as_stars(st_bbox()))

	isdim = sapply(args, inherits, what = "dimensions")
	dimensions = if (! any(isdim)) {
			if (is.array(args[[1]]) && !is.null(dimnames(args[[1]])))
				st_dimensions(args[[1]])
			else
				do.call(st_dimensions, lapply(dim(args[[1]]), function(x) seq_len(x) - 1))
		} else
			args[[ which(isdim)[1] ]]
	if (missing(raster) && !has_sfc(dimensions)) {
		w = which(sapply(dimensions, function(x) is.null(x$values)))
		raster = get_raster(dimensions = names(dimensions)[w[1:2]])
	}
	dimensions = create_dimensions(dimensions, raster)
	if (any(isdim))
		args = args[-which(isdim)]
	if (is.null(names(args)))
		names(args) = paste0("A", seq_along(args))
	st_as_stars.list(args, dimensions = dimensions)
}

#' @export
st_as_stars.bbox = function(.x, ..., nx = 360, ny = 180, crs) {
	if (missing(crs))
		crs = st_crs(.x)
	dx = .x["xmax"] - .x["xmin"]
	dy = .x["ymax"] - .x["ymin"]
	gt = c(.x["xmin"], dx/nx, 0.0, .x["ymax"], 0.0, -dy/ny)
	x = create_dimension(from = 1, to = nx, offset = .x["xmin"], delta =  dx/nx, refsys = crs)
	y = create_dimension(from = 1, to = ny, offset = .x["ymax"], delta = -dy/ny, refsys = crs)
	st_as_stars(values = array(runif(nx * ny), c(x = nx, y = ny)),
		dims = create_dimensions(list(x = x, y = y), get_raster()))
}

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
xy_from_colrow = function(x, geotransform, inverse = FALSE) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	if (inverse) {
		geotransform = gdal_inv_geotransform(geotransform)
		if (any(is.na(geotransform)))
			stop("geotransform not invertible")
	}
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

colrow_from_xy = function(x, geotransform) {
	xy_from_colrow(x, geotransform, inverse = TRUE)
}

has_rotate_or_shear = function(x) {
	dimensions = st_dimensions(x)
	has_raster(x) && any(attr(dimensions, "raster")$affine != 0.0)
}

has_raster = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in% names(x))
}

is_rectilinear = function(x) {
	d = st_dimensions(x)
	has_raster(x) && (is.na(d$x$delta) || is.na(d$y$delta)) &&
		(length(unique(diff(d$x$values))) > 1 || length(unique(diff(d$y$values))) > 1)
}

has_sfc = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	any(sapply(x, function(i) inherits(i$values, "sfc")))
}


#' @export
st_coordinates.stars = function(x, ...) {
	if (has_rotate_or_shear(x))
		stop("affine transformation needed") 
	do.call(expand.grid, expand_dimensions(x))
}

st_coordinates.dimensions = function(x, ...) {
	st_coordinates(st_as_stars(list(), dimensions = x))
}

#' @export
as.data.frame.stars = function(x, ...) {
	data.frame(st_coordinates(x), lapply(x, c))
}

#' @export
print.stars = function(x, ..., n = 1e5) {
	add_units = function(x) {
		f = function(obj) if (inherits(obj, "units")) paste0("[", as.character(units(obj)), "]") else ""
		paste(names(x), sapply(x, f))
	}
	cat("stars object with", length(dim(x)), "dimensions and", 
		length(x), if (length(x) > 1) "attributes\n" else "attribute\n")
	cat("attribute(s)")
	df = if (prod(dim(x)) > 10 * n) {
		cat(paste0(", summary of first ", n, " cells:\n"))                       # nocov
		as.data.frame(lapply(x, function(y) as.vector(y)[1:n]), optional = TRUE) # nocov
	} else {
		cat(":\n")
		as.data.frame(lapply(x, as.vector), optional = TRUE)
	}
	names(df) = add_units(x)
	print(summary(df))
	cat("dimension(s):\n")
	print(st_dimensions(x), ...)
}

#' @export
aperm.stars = function(a, perm = NULL, ...) {
	if (is.null(perm))
		perm = rev(seq_along(dim(a)))
	if (all(perm == seq_along(dim(a))) || isTRUE(all(match(perm, names(dim(a))) == seq_along(dim(a)))))
		return(a)
	if (is.character(perm) && is.null(dimnames(a[[1]]))) {
		ns = names(attr(a, "dimensions"))
		dn = lapply(as.list(dim(a)), seq_len)
		names(dn) = ns
		for (i in seq_along(a))
			dimnames(a[[i]]) = dn
	}
	st_stars(lapply(a, aperm, perm = perm, ...), st_dimensions(a)[perm])
}

#' @export
dim.stars = function(x) {
	d = st_dimensions(x)
	if (length(x) == 0)
		integer(0)
	else {
		stopifnot(length(d) == length(dim(x[[1]])))
		structure(dim(x[[1]]), names = names(d))
	}
}

propagate_units = function(new, old) {
	for (i in seq_along(new))
		if (inherits(old[[i]], "units"))
			units(new[[i]]) <- units(old[[i]])
	new
}

#' combine multiple stars objects, or combine multiple attributes in a single stars object into a single array
#' 
#' combine multiple stars objects, or combine multiple attributes in a single stars object into a single array
#' @param ... object(s) of class \code{star}: in case of multiple arguments, these are combined into a single stars object, in case of a single argument, its attributes are combined into a single attribute
#' @param along integer; see \link{read_stars}
#' @export
c.stars = function(..., along = NA_integer_) {
	dots = list(...)
	# Case 1: merge attributes of several objects by simply putting them together in a single stars object;
	# dim does not change:
	if (is.na(along) && length(dots) > 1) { 
		if (identical_dimensions(dots))
			st_as_stars(do.call(c, lapply(dots, unclass)), dimensions = attr(dots[[1]], "dimensions"))
		else {
			# currently catches only the special case of ... being a broken up time series:
			along = sort_out_along(dots)
			if (is.na(along))
				stop("don't know how to merge arrays: please specify parameter along")
			do.call(c, c(dots, along = along))
		}
	} else {
		# Case 2: single stars object, collapse attributes into new array dimension:
		if (length(dots) == 1) {
			if (is.list(along)) {
				values = along[[1]]
				dim_name = names(along)[1]
			} else {
				values = names(dots[[1]])
				dim_name = "new_dim"
			}
			old_dim = st_dimensions(dots[[1]])
			new_dim = create_dimension(values = values)
			dims = create_dimensions(c(old_dim, new_dim = list(new_dim)), attr(old_dim, "raster"))
			names(dims)[names(dims) == "new_dim"] = dim_name
			st_as_stars(attr = do.call(abind, c(dots, along = length(dim(dots[[1]])) + 1)), dimensions = dims)
		} else if (is.list(along)) { # custom ordering of ... over dimension(s) with values specified
			if (prod(lengths(along)) != length(dots))
				stop("number of objects does not match the product of lenghts of the along argument", call. = FALSE)
			# abind all:
			d = st_dimensions(dots[[1]])
			ret = mapply(abind, ..., along = length(d) + 1, SIMPLIFY = FALSE)
			# make dims:
			newdim = c(dim(dots[[1]]), lengths(along))
			ret = lapply(ret, function(x) { dim(x) = newdim; x })
			ret = propagate_units(ret, dots[[1]])
			# make dimensions:
			for (i in seq_along(along))
				d[[ names(along)[i] ]] = create_dimension(values = along[[i]])
			st_as_stars(ret, dimensions = d)
		} else { # loop over attributes, abind them:
			# along_dim: the number of the dimension along which we merge arrays
			d = st_dimensions(dots[[1]])
			along_dim = if (is.character(along)) {
				along_dim = which(along == names(d))
				if (length(along_dim) == 0)
					length(d) + 1
				else
					along_dim
			} else
				along
			ret = propagate_units(mapply(abind, ..., along = along_dim, SIMPLIFY = FALSE), dots[[1]])
			dims = combine_dimensions(dots, along_dim)
			if (along_dim == length(d) + 1)
				names(dims)[along_dim] = if (is.character(along)) along else "new_dim"
			st_as_stars(ret, dimensions = dims)
		}
	}
}

#' @export
adrop.stars = function(x, drop = which(dim(x) == 1), ...) {
	st_as_stars(lapply(x, adrop, drop = drop, ...), dimensions = st_dimensions(x)[-drop])
}

#' @export
st_bbox.default = function(obj, ...) {
	if (!missing(obj))
		stop(paste("no st_bbox method available for object of class", class(obj)))
	obj = st_sfc(st_point(c(-180,-90)), st_point(c(180, 90)), crs = 4326)
	st_bbox(obj)
}

#' @export
st_bbox.dimensions = function(obj, ...) {
	if (has_raster(obj)) { # raster
		r = attr(obj, "raster")
		x = obj[[ r$dimensions[1] ]]
		y = obj[[ r$dimensions[2] ]]
		bb = if (is.null(x$values) && is.null(y$values)) {
			gt = get_geotransform(obj)
			stopifnot(length(gt) == 6 && !any(is.na(gt)))
			bb = rbind(c(x$from - 1, y$from - 1), c(x$to, y$from - 1), c(x$to, y$to), c(x$from - 1, y$to))
			xy = xy_from_colrow(bb, gt)
			c(xmin = min(xy[,1]), ymin = min(xy[,2]), xmax = max(xy[,1]), ymax = max(xy[,2]))
		} else {
			e = expand_dimensions(obj)
			rx = range(e$x)
			ry = range(e$y)
			c(xmin = rx[1], ymin = ry[1], xmax = rx[2], ymax = ry[2])
		}
		structure(bb, crs = st_crs(x$refsys), class = "bbox")
	} else {
		if (! has_sfc(obj))
			stop("dimensions table does not have x & y, nor an sfc dimension") # nocov
		ix = which(sapply(obj, function(i) inherits(i$values, "sfc")))
		st_bbox(obj[[ ix[1] ]]$values) # FIXME: what if there is more than one, e.g. O.D.?
	}
}

#' @export
st_bbox.stars = function(obj, ...) {
	st_bbox(st_dimensions(obj), ...)
}

#' @export
st_crs.stars = function(x, ...) {
	d = st_dimensions(x)
	if ("x" %in% names(d))
		st_crs(d$x$refsys)
	else { # search for simple features:
		i = sapply(d, function(y) inherits(y$values, "sfc"))
		if (any(i))
			st_crs(d[[i[1]]]$values)
		else
			st_crs(NA)
	}
}

#' @export
`st_crs<-.stars` = function(x, value) {
	crs = st_crs(value)
	d = st_dimensions(x)
	if ("x" %in% names(d))
		d$x$refsys = crs$proj4string
	if ("y" %in% names(d))
		d$y$refsys = crs$proj4string
	if ("sfc" %in% names(d))
		d$sfc$refsys = crs$proj4string
	st_as_stars(unclass(x), dimensions = d)
}

#' subset stars objects
#' 
#' subset stars objects
#' @name stars_subset
#' @param x object of class \code{stars}
#' @param i first selector: integer, logical or character vector indicating attributes to select, or object of class \code{sf} or \code{sfc} used as spatial selector; see details
#' @param drop 
#' @param ... further (logical or integer vector) selectors, matched by order, to select on individual dimensions
#' @param drop logical; if \code{TRUE}, degenerate dimensions (with only one value) are dropped 
#' @param crop logical; if \code{TRUE} and parameter \code{i} is a spatial geometry (\code{sf} or \code{sfc}) object, the extent (bounding box) of the result is cropped to match the extent of \code{i} using \link{st_crop}.
#' @details if \code{i} is an object of class \code{sf}, \code{sfc} or \code{bbox}, the spatial subset covering this geometry is selected, possibly followed by cropping the extent. Array values for which the cell centre is not inside the geometry are assigned \code{NA}.
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' x[,,,1:3] # select bands
#' x[,1:100,100:200,] # select x and y by range
#' x["L7_ETMs.tif"] # select attribute
#' xy = structure(list(x = c(293253.999046018, 296400.196497684), y = c(9113801.64775462,
#' 9111328.49619133)), .Names = c("x", "y"))
#' pts = st_as_sf(data.frame(do.call(cbind, xy)), coords = c("x", "y"), crs = st_crs(x))
#' image(x, axes = TRUE)
#' plot(st_as_sfc(st_bbox(pts)), col = NA, add = TRUE)
#' bb = st_bbox(pts)
#' (xx = x[bb])
#' image(xx)
#' plot(st_as_sfc(bb), add = TRUE, col = NA)
#' image(x)
#' pt = st_point(c(x = 290462.103109179, y = 9114202.32594085))
#' buf = st_buffer(st_sfc(pt, crs = st_crs(x)), 1500)
#' plot(buf, add = TRUE)
#' 
#' buf = st_sfc(st_polygon(list(st_buffer(pt, 1500)[[1]], st_buffer(pt, 1000)[[1]])),
#'    crs = st_crs(x))
#' image(x[buf])
#' plot(buf, add = TRUE, col = NA)
#' image(x[buf, crop=FALSE])
#' plot(buf, add = TRUE, col = NA)
#' plot(x, rgb = 1:3)
"[.stars" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
  missing.i = missing(i)
  # special case:
  if (! missing.i && inherits(i, c("sf", "sfc", "bbox")))
  	return(st_crop(x, i, crop = crop))
  mc <- match.call(expand.dots = TRUE)
  # select list elements from x, based on i:
  d = attr(x, "dimensions")
  ed = expand_dimensions(d)
  x = unclass(x)[i]
  # selects also on dimensions:
  if (length(mc) > 3) {
    mc[[1]] <- `[`
    if (! missing(i))
		mc[[3]] <- NULL # remove i
	mc[["drop"]] = FALSE
	for (i in names(x)) {
		mc[[2]] = as.name(i)
		x[[i]] = eval(mc, x, parent.frame())
	}
	mc0 = mc[1:3] # "[", x, first dim
	j = 3 # first dim
	for (i in names(d)) {
		mc0[[2]] = as.name(i)
		mc0[[3]] = mc[[j]]
		mc0[["values"]] = ed[[i]]
		d[[i]] = eval(mc0, d, parent.frame())
		j = j + 1
	}
  }
  if (drop)
  	adrop(st_as_stars(x, dimensions = d))
  else
  	st_as_stars(x, dimensions = d)
}

#' crop a stars object
#' 
#' crop a stars object
#' @name st_crop
#' @export
#' @param x object of class \code{stars}
#' @param y object of class \code{sf}, \code{sfc} or \code{bbox}
#' @param ... ignored
#' @param crop logical; if \code{TRUE}, the spatial extent of the returned object is cropped to still cover \code{obj}
st_crop.stars = function(x, y, ..., crop = TRUE) {
	obj = y
	d = dim(x)
	dm = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(d)+1)
	if (st_crs(x) != st_crs(y))
		stop("for cropping, the CRS of both objects has to be identical")
	if (crop) {
		bb = if (!inherits(y, "bbox"))
				st_bbox(y)
			else
				y
		cr = colrow_from_xy(matrix(bb, 2, byrow=TRUE), get_geotransform(dm))
		for (i in seq_along(d)) {
			if (names(d[i]) == "x")
				args[[i+1]] = seq(max(1, floor(cr[1, 1])), min(d["x"], ceiling(cr[2, 1])))
			if (names(d[i]) == "y") {
				if (dm$y$delta < 0)
					cr[1:2, 2] = cr[2:1, 2]
				args[[i+1]] = seq(max(1, floor(cr[1, 2])), min(d["y"], ceiling(cr[2, 2])))
			}
		}
		x = eval(rlang::expr(x[!!!args]))
	}
	if (inherits(y, "bbox"))
		y = st_as_sfc(y)
	xy_grd = st_as_sf(do.call(expand.grid, expand_dimensions.stars(x)[c("x", "y")]),
		coords = c("x", "y"), crs = st_crs(x))
	inside = st_intersects(y, xy_grd)[[1]]
	d = dim(x) # cropped x
	raster = rep(NA_real_, prod(d[c("x", "y")]))
	raster[inside] = 1
	x * array(raster, d) # replicates over secondary dims
}

#' @export
split.stars = function(x, f, drop = TRUE, ...) {
	d = st_dimensions(x)
	if (is.character(f))
		f = which(names(d) == f)
	ret = lapply(seq_len(dim(x)[f]), function(y) asub(x[[1]], y, f, drop = TRUE))
	spl = st_as_stars(ret, dimensions = d[-f])
	if (is.null(names(spl)))
		names(spl) = if (!is.null(d[[f]]$values))
				d[[f]]$values
			else
				make.names(seq_along(spl))
	spl
}

#' @export
merge.stars = function(x, y, ...) {
	dots = list(...)
	if (!missing(y))
		stop("argument y needs to be missing: merging attributes of x")
	old_dim = st_dimensions(x)
	out = do.call(abind, c(x, along = length(dim(x[[1]]))+1))
	new_dim = if (length(dots))
			create_dimension(values = dots[[1]])
		else
			create_dimension(values = names(x))
	d = create_dimensions(c(old_dim, list(new_dim)), raster = attr(old_dim, "raster"))
	if (!is.null(names(dots)))
		names(d)[length(d)] = names(dots)
	st_as_stars(out, dimensions = d)
}

sort_out_along = function(ret) { 
	d1 = st_dimensions(ret[[1]])
	d2 = st_dimensions(ret[[2]])
	if ("time" %in% names(d1) && d1$time$offset != d2$time$offset)
		"time"
	else
		NA_integer_
}
