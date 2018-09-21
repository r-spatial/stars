split_strings = function(md, split = "=") {
	splt = strsplit(md, split)
	lst = lapply(splt, function(x) if (length(x) <= 1) NA_character_ else x[[2]])
	structure(lst, names = sapply(splt, function(x) x[[1]]), class = "gdal_metadata")
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
	if (is.null(raster) && !has_sfc(dimensions)) {
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
st_as_stars.stars = function(.x, ..., curvilinear = NULL) {
	if (is.null(curvilinear))
		.x
	else {
		dimensions = st_dimensions(.x)
		xy = names(curvilinear)
		dimensions[[ xy[1] ]]$values = curvilinear[[1]]
		dimensions[[ xy[2] ]]$values = curvilinear[[2]]
		raster = get_raster(dimensions = names(curvilinear), curvilinear = TRUE)
		st_stars(.x, create_dimensions(dimensions, raster))
	}
}

#' @export
st_as_stars.bbox = function(.x, ..., nx = 360, ny = 180, crs = st_crs(.x), 
		xlim = .x[c("xmin", "xmax")], ylim = .x[c("ymin", "ymax")]) {
	x = create_dimension(from = 1, to = nx, offset = xlim[1], delta =  diff(xlim)/nx, refsys = crs)
	y = create_dimension(from = 1, to = ny, offset = ylim[2], delta = -diff(ylim)/ny, refsys = crs)
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
	xy_from_colrow(x, geotransform, inverse = TRUE) # will return floating point col/row numbers!!
}

has_rotate_or_shear = function(x) {
	dimensions = st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!any(is.na(r$affine)) && r$affine != 0.0
	} else
		FALSE
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

is_curvilinear = function(x) {
	d = st_dimensions(x)
	has_raster(x) && attr(d, "raster")$curvilinear
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
			st_stars(list(attr = do.call(abind, c(dots, along = length(dim(dots[[1]])) + 1))), dimensions = dims)
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
#' @param crop logical; if \code{TRUE} and parameter \code{i} is a spatial geometry (\code{sf} or \code{sfc}) object, the extent (bounding box) of the result is cropped to match the extent of \code{i} using \link{sti_crop}.
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
  d = st_dimensions(x)
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
		x[[i]] = eval(mc, x, parent.frame()) # subset array
	}
	xy = attr(d, "raster")$dimensions
	if (is_curvilinear(d)) { # subset curvilinear lat/lon matrices/rasters: can't do one-at-a-time!
		mc[[2]] = as.name("values")
		d[[ xy[1] ]]$values = eval(mc, d[[ xy[1] ]], parent.frame())
		d[[ xy[2] ]]$values = eval(mc, d[[ xy[2] ]], parent.frame())
	}
	d_backup = d
	# dimensions:
	mc0 = mc[1:3] # "[", x, first dim
	j = 3 # first dim
	for (i in names(d)) { # one-at-a-time:
		mc0[[2]] = as.name(i)
		mc0[[3]] = mc[[j]]
		if (! (is_curvilinear(d) && i %in% xy))
			mc0[["values"]] = ed[[i]]
		d[[i]] = eval(mc0, d, parent.frame()) # subset dimension
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
#' @param y object of class \code{sf}, \code{sfc} or \code{bbox}; see Details below.
#' @param epsilon numeric; shrink the bounding box of \code{y} to its center with this factor.
#' @param ... ignored
#' @param crop logical; if \code{TRUE}, the spatial extent of the returned object is cropped to still cover \code{obj}, if \code{FALSE}, the extent remains the same but cells outside \code{y} are given \code{NA} values.
#' @details for raster \code{x}, \code{st_crop} selects cells for which the cell centre is inside the bounding box; see the examples below.
#' @examples
#' l7 = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
#' d = st_dimensions(l7)
#' 
#' # area around cells 3:10 (x) and 4:11 (y):
#' offset = c(d[["x"]]$offset, d[["y"]]$offset)
#' res = c(d[["x"]]$delta, d[["y"]]$delta)
#' bb = st_bbox(c(xmin = offset[1] + 2 * res[1],
#' 	ymin = offset[2] + 11 * res[2],
#' 	xmax = offset[1] + 10 * res[1],
#' 	ymax = offset[2] +  3 * res[2]), crs = st_crs(l7))
#' l7[bb]
#' 
#' plot(l7[,1:13,1:13,1], reset = FALSE)
#' image(l7[bb,,,1], add = TRUE, col = sf.colors())
#' plot(st_as_sfc(bb), add = TRUE, border = 'green', lwd = 2)
#' 
#' # slightly smaller bbox:
#' bb = st_bbox(c(xmin = offset[1] + 2.1 * res[1],
#' 	ymin = offset[2] + 10.9 * res[2],
#' 	xmax = offset[1] +  9.9 * res[1],
#' 	ymax = offset[2] +  3.1 * res[2]), crs = st_crs(l7))
#' l7[bb]
#' 
#' plot(l7[,1:13,1:13,1], reset = FALSE)
#' image(l7[bb,,,1], add = TRUE, col = sf.colors())
#' plot(st_as_sfc(bb), add = TRUE, border = 'green', lwd = 2)
#' 
#' # slightly larger bbox:
#' bb = st_bbox(c(xmin = offset[1] + 1.9 * res[1],
#' 	ymin = offset[2] + 11.1 * res[2],
#' 	xmax = offset[1] + 10.1 * res[1],
#' 	ymax = offset[2] +  2.9 * res[2]), crs = st_crs(l7))
#' l7[bb]
#' 
#' plot(l7[,1:13,1:13,1], reset = FALSE)
#' image(l7[bb,,,1], add = TRUE, col = sf.colors())
#' plot(st_as_sfc(bb), add = TRUE, border = 'green', lwd = 2)
#' 
#' # half a cell size larger bbox:
#' bb = st_bbox(c(xmin = offset[1] + 1.49 * res[1],
#' 	ymin = offset[2] + 11.51 * res[2],
#' 	xmax = offset[1] + 10.51 * res[1],
#' 	ymax = offset[2] +  2.49 * res[2]), crs = st_crs(l7))
#' l7[bb]
#' 
#' plot(l7[,1:13,1:13,1], reset = FALSE)
#' image(l7[bb,,,1], add = TRUE, col = sf.colors())
#' plot(st_as_sfc(bb), add = TRUE, border = 'green', lwd = 2)
st_crop.stars = function(x, y, ..., crop = TRUE, epsilon = 0) {
	d = dim(x)
	dm = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(d)+1)
	if (st_crs(x) != st_crs(y))
		stop("for cropping, the CRS of both objects has to be identical")
	if (crop && has_raster(x)) {
		rastxy = attr(dm, "raster")$dimensions
		xd = rastxy[1]
		yd = rastxy[2]
		bb = if (!inherits(y, "bbox"))
				st_bbox(y)
			else
				y
		if (epsilon != 0)
			bb = bb_shrink(bb, epsilon)
		cr = round(colrow_from_xy(matrix(bb, 2, byrow=TRUE), get_geotransform(dm)) + 0.5)
		for (i in seq_along(d)) {
			if (names(d[i]) == xd)
				args[[i+1]] = seq(max(1, cr[1, 1]), min(d[xd], cr[2, 1]))
			if (names(d[i]) == yd) {
				if (dm[[ yd ]]$delta < 0)
					cr[1:2, 2] = cr[2:1, 2]
				args[[i+1]] = seq(max(1, cr[1, 2]), min(d[yd], cr[2, 2]))
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
	mask = rep(NA_real_, prod(d[c("x", "y")]))
	mask[inside] = 1
	x * array(mask, d) # replicates over secondary dims
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
