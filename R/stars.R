split_strings = function(md, split = "=") {
	splt = strsplit(md, split)
	lst = lapply(splt, function(x) if (length(x) <= 1) NA_character_ else x[[2]])
	structure(lst, names = sapply(splt, function(x) x[[1]]))
	structure(lst, class = "gdal_metadata")
}

#' read raster/array dataset from file or connection
#'
#' read raster/array dataset from file or connection
#' @param .x if character, name of file(s) to read; if list: list with arrays
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param sub integer or logical; sub-datasets to be read
#' @param quiet logical; print progress output?
#' @param NA_value numeric value to be used for conversion into NA values; by default this is read from the input file
#' @param ... arrays to be compiled into a stars object
#' @return object of class \code{stars}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' # x1 = read_stars(nv, options = "OVERVIEW_LEVEL=1")
read_stars = function(.x, ..., options = character(0), driver = character(0), 
		sub = TRUE, quiet = FALSE, NA_value = NA_real_) {

	x = .x
	if (length(x) > 1) { # recurse:
		ret = lapply(x, read_stars, options = options, driver = driver, sub = sub, quiet = quiet)
		return(do.call(c, c(ret, along = 3))) # FIXME: along = 3? or the highest?
	}

	properties = gdal_read(x, options = options, driver = driver, read_data = TRUE, NA_value = NA_value)

	if (properties$bands[2] == 0) { # read sub-datasets: different attributes
		sub_names = split_strings(properties$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		# sub_datasets = gdal_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)
		names(sub_datasets) = nms
		sub_datasets = sub_datasets[sub]
		nms = names(sub_datasets)

		.read_stars = function(x, options, driver, keep_meta, quiet) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			read_stars(x, options = options, driver = driver)
		}
		ret = lapply(sub_datasets, .read_stars, options = options, 
			driver = properties$driver[1], quiet = quiet)
		if (! quiet)
			cat("\n")
		structure(do.call(c, ret), names = nms)
	} else  { # we have one single array:
		data = attr(properties, "data")
		properties = structure(properties, data = NULL) # remove data from properties
		if (properties$driver[1] == "netCDF")
			properties = parse_netcdf_meta(properties, x)
		properties = parse_meta(properties)
		if (! is.null(properties$units) && ! is.na(properties$units))
			units(data) = try_as_units(properties$units)

		newdims = lengths(properties$dim_extra)
		data = if (length(newdims))
				structure(data, dim = c(dim(data)[1:2], newdims))
			else
				structure(data, dim = dim(data))
		structure(list(data), names = tail(strsplit(x, .Platform$file.sep)[[1]], 1),
			dimensions = create_dimensions(dim(data), properties),
			class = "stars")
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
	}
	if (is.null(dimensions))
		dimensions = create_dimensions(dim(.x[[1]]))
	structure(.x, dimensions = dimensions, class = "stars")
}

#' @name st_as_stars
#' @export
st_as_stars.default = function(.x = NULL, ...) {
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
	x = create_dimension(from = 1, to = nx, offset = .x["xmin"], delta = dx/nx, refsys = crs, geotransform = gt)
	y = create_dimension(from = 1, to = ny, offset = .x["ymax"], delta = -dy/ny, refsys = crs, geotransform = gt)
	st_as_stars(values = array(runif(nx * ny), c(x = nx, y = ny)), 
		dims = structure(list(x = x, y = y), class = "dimensions"))
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
	has_raster(x) &&
		any(sapply(dimensions, function(x) 
		! all(is.na(x$geotransform)) && any(x$geotransform[c(3, 5)] != 0)))
}

has_raster = function(x)
	all(c("x", "y") %in% names(st_dimensions(x)))

is_rectilinear = function(x) {
	d = st_dimensions(x)
	has_raster(x) && (is.na(d$x$delta) || is.na(d$y$delta))
}

has_sfc = function(x)
	all(c("sfc") %in% names(st_dimensions(x)))

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
		cat(paste0(", of first ", n, " cells:\n"))
		as.data.frame(lapply(x, function(y) as.vector(y)[1:n]), optional = TRUE)
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
	if (is.character(perm) && is.null(dimnames(a[[1]]))) {
		ns = names(attr(a, "dimensions"))
		dn = lapply(as.list(dim(a)), seq_len)
		names(dn) = ns
		for (i in seq_along(a))
			dimnames(a[[i]]) = dn
	}
	dimensions = structure(attr(a, "dimensions")[perm], class = "dimensions")
	structure(lapply(a, aperm, perm = perm, ...), 
		dimensions = dimensions, class = "stars")
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

#' @export
c.stars = function(..., along = NA_integer_, dim_name, values = names(dots[[1]])) {
	dots = list(...)
	if (is.na(along) && length(dots) > 1) { # merge attributes of several objects, retaining dim
		check_equal_dimensions(dots)
		st_as_stars(do.call(c, lapply(dots, unclass)), dimensions = attr(dots[[1]], "dimensions"))
	} else {
		if (length(dots) == 1 && (is.na(along) || along == length(dim(dots[[1]])) + 1)) { 
			# collapse attributes into new array dimension:
			along = length(dim(dots[[1]])) + 1
			new_dim = create_dimension(values = values)
			dims = structure(c(st_dimensions(dots[[1]]), new_dim = list(new_dim)), class = "dimensions")
			if (! missing(dim_name))
				names(dims)[names(dims) == "new_dim"]= dim_name
			st_as_stars(attr = do.call(abind, c(dots, along = along)), dimensions = dims)
		} else { # loop over attributes:
			ret = propagate_units(mapply(abind, ..., along = along, SIMPLIFY = FALSE), dots[[1]])
			dims = combine_dimensions(dots, along)
			st_as_stars(ret, dimensions = dims)
		}
	}
}

#' @export
adrop.stars = function(x, drop = which(dim(x) == 1), ...) {
	dims = structure(attr(x, "dimensions")[-drop], class = "dimensions")
	st_as_stars(lapply(x, adrop, drop = drop, ...), dimensions = dims)
}

#' @export
st_bbox.default = function(obj, ...) {
	obj = st_sfc(st_point(c(-180,-90)), st_point(c(180, 90)), crs = 4326)
	st_bbox(obj)
}

#' @export
st_bbox.stars = function(obj, ...) {
	d = st_dimensions(obj)
	stopifnot(all(c("x", "y") %in% names(d)))
	gt = attr(obj, "dimensions")$x$geotransform
	stopifnot(length(gt) == 6 && !any(is.na(gt)))

	bb = if (is.null(d$x$values) && is.null(d$y$values)) {
		bb = rbind(c(d$x$from-1,d$y$from-1), c(d$x$to, d$y$from-1), c(d$x$to, d$y$to), c(d$x$from-1, d$y$to))
		xy = xy_from_colrow(bb, gt)
		c(xmin = min(xy[,1]), ymin = min(xy[,2]), xmax = max(xy[,1]), ymax = max(xy[,2]))
	} else {
		e = expand_dimensions(d)
		rx = range(e$x)
		ry = range(e$y)
		c(xmin = min(e$x), ymin = min(e$y), xmax = max(e$x), ymax = max(e$y))
	}
	structure(bb, crs = st_crs(obj), class = "bbox")
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

#' @export
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
		x[[i]] = eval(mc, x)
	}
	mc0 = mc[1:3] # "[", x, first dim
	j = 3 # first dim
	for (i in names(d)) {
		mc0[[2]] = as.name(i)
		mc0[[3]] = mc[[j]]
		mc0[["values"]] = ed[[i]]
		d[[i]] = eval(mc0, d)
		j = j + 1
	}
  }
  if (drop)
  	adrop(st_as_stars(x, dimensions = d))
  else
  	st_as_stars(x, dimensions = d)
}

st_downsample = function(x, n) {
	stopifnot(all(n >= 0))
	d = dim(x)
	n = rep(n, length.out = length(d))
	args = rep(list(rlang::missing_arg()), length(d)+1)
	for (i in seq_along(d))
		if (n[i] > 1)
			args[[i+1]] = seq(1, d[i], n[i])
	eval(rlang::expr(x[!!!args]))
}

st_crop = function(x, obj, crop = TRUE) {
	d = dim(x)
	dm = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(d)+1)
	if (crop) {
		bb = if (!inherits(obj, "bbox"))
				st_bbox(obj)
			else
				obj
		cr = colrow_from_xy(matrix(bb, 2, byrow=TRUE), dm$x$geotransform)
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
	if (inherits(obj, "bbox"))
		obj = st_as_sfc(obj)
	xy_grd = st_as_sf(do.call(expand.grid, expand_dimensions.stars(x)[c("x", "y")]),
		coords = c("x", "y"), crs = st_crs(x))
	inside = st_intersects(obj, xy_grd)[[1]]
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
	d = st_dimensions(x)
	out = do.call(abind, c(x, along = length(dim(x[[1]]))+1))
	new_dim = if (length(dots))
			create_dimension(values = dots[[1]])
		else
			create_dimension(values = names(x))
	d = structure(c(d, list(new_dim)), class = "dimensions")
	if (!is.null(names(dots)))
		names(d)[length(d)] = names(dots)
	st_as_stars(out, dimensions = d)
}
