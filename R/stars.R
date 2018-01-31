## read raster/array dataset from file or connection

#' read raster/array dataset from file or connection
#' @param .x if character, name of file(s) to read; if list: list with arrays
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param sub integer or logical; sub-datasets to be read
#' @param quiet logical; print progress output?
#' @param ... arrays to be compiled into a stars object
#' @return object of class \code{stars}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' # x1 = st_stars(nv, options = "OVERVIEW_LEVEL=1")
st_stars = function(.x, ...) UseMethod("st_stars")

#' @name st_stars
#' @export
st_stars.character = function(.x, ..., options = character(0), driver = character(0), sub = TRUE, quiet = FALSE) {

	x = .x
	if (length(x) > 1) { # recurse:
		ret = lapply(x, st_stars, options = options, driver = driver, sub = sub, quiet = quiet)
		return(do.call(c, c(ret, along = 3))) # FIXME: along = 3? or the highest?
	}

	properties = CPL_read_gdal(x, options, driver, TRUE)

	if (properties$bands[2] == 0) { # read sub-datasets: different attributes
		sub_names = split_strings(properties$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		# sub_datasets = gdal_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)
		names(sub_datasets) = nms
		sub_datasets = sub_datasets[sub]
		nms = names(sub_datasets)

		read_stars = function(x, options, driver, keep_meta, quiet) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			st_stars(x, options = options, driver = driver)
		}
		ret = lapply(sub_datasets, read_stars, options = options, 
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
		structure(list(data), names = x,
			dimensions = create_dimensions(dim(data), properties),
			class = "stars")
	}
}

#' @name st_stars
#' @param dimensions object of class dimensions
#' @export
st_stars.list = function(.x, ..., dimensions = NULL) {
	if (length(.x) > 1) {
		for (i in seq_along(.x)[-1])
			if (!identical(dim(.x[[1]]), dim(.x[[i]])))
				stop("dim attributes not identical")
	}
	if (is.null(dimensions))
		dimensions = create_dimensions(dim(.x[[1]]))
	structure(.x, dimensions = dimensions, class = "stars")
}

#' @name st_stars
#' @export
st_stars.default = function(.x = NULL, ...) {
	args = if (is.null(.x))
			list(...)
		else
			append(list(.x), list(...))
	isdim = sapply(args, inherits, what = "dimensions")
	dimensions = if (!any(isdim))
			do.call(st_dimensions, lapply(dim(args[[1]]), function(x) seq_len(x) - 1))
		else
			args[[ which(isdim)[1] ]]
	if (any(isdim))
		args = args[-which(isdim)]
	if (is.null(names(args)))
		names(args) = paste0("A", seq_along(args))
	st_stars(args, dimensions = dimensions)
}

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
xy_from_colrow = function(x, geotransform, inverse = FALSE) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	if (inverse) {
		geotransform = CPL_inv_geotransform(geotransform)
		if (any(is.na(geotransform)))
			stop("geotransform not invertible")
	}
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
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
	st_coordinates(st_stars(list(), dimensions = x))
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
		print(dn)
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
c.stars = function(..., along = NA_integer_) {
	dots = list(...)
	if (is.na(along)) { # merge attributes
		check_equal_dimensions(dots)
		st_stars(do.call(c, lapply(dots, unclass)), dimensions = attr(dots[[1]], "dimensions"))
	} else {
		if (length(dots) == 1 && is.na(along)) # attributes to array dimension:
			along = length(dim(dots[[1]]) + 1)
		ret = if (length(dots) == 1 && along == length(dim(dots[[1]])) + 1) { # collapse:
			dn = names(dots[[1]])
			do.call(abind, c(dots, along = along))
		} else { # loop over attributes:
			propagate_units(mapply(abind, ..., along = along, SIMPLIFY = FALSE), dots[[1]])
		}
		dims = combine_dimensions(dots, along)
		structure(ret, dimensions = dims, class = "stars")
	}
}

#' @export
adrop.stars = function(x, drop = which(dim(x) == 1), ...) {
	dims = structure(attr(x, "dimensions")[-drop], class = "dimensions")
	structure(lapply(x, adrop, drop = drop, ...), dimensions = dims, class = "stars")
	# TODO: deal with dimensions table
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
"[.stars" = function(x, i = TRUE, ..., drop = FALSE) {
  #st_stars(unclass(x)[i], dimensions = st_dimensions(x))
  missing.i = missing(i)
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
  	adrop(st_stars(x, dimensions = d))
  else
  	st_stars(x, dimensions = d)
}
