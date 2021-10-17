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
#' @param ... in case \code{.x} is of class \code{bbox}, arguments passed on to \link{pretty}
st_as_stars = function(.x, ...) UseMethod("st_as_stars")

#' @name st_as_stars
#' @param dimensions object of class dimensions
#' @export
st_as_stars.list = function(.x, ..., dimensions = NULL) {
	if (length(.x)) {
		for (i in seq_along(.x)[-1])
			if (!all(dim(.x[[1]]) == dim(.x[[i]])))
				stop("dim attributes not identical")
		if (!is.null(names(.x)))
			names(.x) = make.names(names(.x), unique = TRUE)

		# check dimensions, if set:
		if (!is.null(dimensions)) {
			dx = dim(.x[[1]])
			dd = dim(dimensions)
			stopifnot(!is.null(dx), !is.null(dd))
			for (i in seq_along(dimensions)) {
				if (dx[i] < dd[i]) { # create_dimension was called with $values one longer than corresponding array dim,
					v = dimensions[[i]]$values
					if (is.null(v)) # regularly spaced, meaning offset/delta have replaced $values:
						dimensions[[i]]$to = dimensions[[i]]$to - 1
					else if (length(v) == dx[i] + 1) { # convert the one-too-long values into an intervals object:
						dimensions[[i]]$values = head(v, -1)
						dimensions[[i]]$to = dimensions[[i]]$to - 1
					} else
						stop(paste("incorrect length of dimensions values for dimension", i))
					dimensions[[i]]$point = FALSE
				}
			}
		}
	}
	st_stars(.x, dimensions %||% create_dimensions(dim(.x[[1]])))
}

st_stars = function(x, dimensions) {
	# sanity checks:
	stopifnot(is.list(x))
	stopifnot(inherits(dimensions, "dimensions"))
	stopifnot(!is.null(attr(dimensions, "raster")))
#	for (i in seq_along(x))
#		names(dim(x[[i]])) = names(dimensions)
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
		} else {
			d = args[[ which(isdim)[1] ]]
			if (is.null(raster))
				raster = attr(d, "raster")
			d
		}

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

#' @param curvilinear only for creating curvilinear grids: named length 2 list holding longitude and latitude matrices; the names of this list should correspond to raster dimensions referred to
#' @param crs object of class \code{crs} with the coordinate reference system of the values in \code{curvilinear}; see details
#' @details if \code{curvilinear} is a \code{stars} object with longitude and latitude values, its coordinate reference system is typically not that of the latitude and longitude values.
#' @export
#' @name st_as_stars
st_as_stars.stars = function(.x, ..., curvilinear = NULL, crs = st_crs(4326)) {
	if (is.null(curvilinear))
		.x
	else {
		stopifnot(is.list(curvilinear), names(curvilinear) %in% names(dim(.x)))
		if (inherits(curvilinear[[1]], "stars"))
			curvilinear[[1]] = curvilinear[[1]][[1]]
		if (inherits(curvilinear[[2]], "stars"))
			curvilinear[[2]] = curvilinear[[2]][[1]]
		dimensions = st_dimensions(.x)
		xy = names(curvilinear)
		dimensions[[ xy[1] ]]$values = structure(curvilinear[[1]], dim = setNames(dim(curvilinear[[1]]), xy))
		dimensions[[ xy[2] ]]$values = structure(curvilinear[[2]], dim = setNames(dim(curvilinear[[1]]), xy))
		# erase regular grid coefficients $offset and $delta:
		dimensions[[ xy[1] ]]$offset = dimensions[[ xy[1] ]]$delta = NA_real_
		dimensions[[ xy[2] ]]$offset = dimensions[[ xy[2] ]]$delta = NA_real_
		raster = get_raster(dimensions = names(curvilinear), curvilinear = TRUE)
		st_set_crs(st_stars(.x, create_dimensions(dimensions, raster)), crs)
	}
}

pretty_cut = function(lim, n, inside = FALSE, ...) {
	stopifnot(n > 2)
	p = pretty(x = lim, n = n, ...)
	d = diff(p[1:2])
	if (! inside)
		lim = c(lim[1] - 0.5 * d, lim[2] + 0.5 * d) # extended limits half a cell
	p[p >= lim[1] & p <= lim[2]]
}

#' @param nx integer; number of cells in x direction; see details
#' @param ny integer; number of cells in y direction; see details
#' @param nz integer; number of cells in z direction; if missing no z-dimension is created.
#' @param dx numeric; cell size in x direction; see details
#' @param dy numeric; cell size in y direction; see details
#' @param xlim length 2 numeric vector with extent (min, max) in x direction
#' @param ylim length 2 numeric vector with extent (min, max) in y direction
#' @param values value(s) to populate the raster values with
#' @param n the (approximate) target number of grid cells
#' @param pretty logical; should cell coordinates have \link{pretty} values?
#' @param inside logical; should all cells entirely fall inside the bbox, potentially not covering it completely?
#' @details For the \code{bbox} method: if \code{pretty} is \code{TRUE}, raster cells may extend the coordinate range of \code{.x} on all sides. If in addition to \code{nx} and \code{ny}, \code{dx} and \code{dy} are also missing, these are set to a single value computed as \code{sqrt(diff(xlim)*diff(ylim)/n)}. If \code{nx} and \code{ny} are missing, they are computed as the ceiling of the ratio of the (x or y) range divided by (dx or dy), unless \code{inside} is \code{TRUE}, in which case ceiling is replaced by floor. Postive \code{dy} will be made negative. Further named arguments (\code{...}) are passed on to \code{pretty}.
#' @export
#' @name st_as_stars
st_as_stars.bbox = function(.x, ..., nx, ny, dx = dy, dy = dx,
		xlim = .x[c("xmin", "xmax")], ylim = .x[c("ymin", "ymax")], 
		values = 0., n = 64800, pretty = FALSE, inside = FALSE, nz) {

	if (xor(missing(nx), missing(ny)))
		stop("either specify both nx and ny, or none of them")

	adx = abs(diff(xlim))
	ady = abs(diff(ylim))

	if (missing(dx) && missing(dy)) {
		if (missing(nx))
			dx = dy = sqrt(adx * ady / n)
		else {
			dx = diff(xlim)/nx
			dy = -diff(ylim)/ny
		}
	}

	if (missing(nx))
		nx = ifelse(inside, floor(diff(xlim) / dx), ceiling(diff(xlim) / dx))

	if (missing(ny)) {
		if (dy > 0)
			dy = -dy
		ny = ifelse(inside, floor(-diff(ylim) / dy), ceiling(-diff(ylim) / dy))
	}

	if (pretty) {
		vx = pretty_cut(xlim, nx, inside, ...)
		nx = length(vx)
		x = create_dimension(values = vx, refsys = st_crs(.x))
		vy = pretty_cut(ylim, ny, inside, ...)
		ny = length(vy)
		y = create_dimension(values = vy, refsys = st_crs(.x))
	} else {
		x = create_dimension(from = 1, to = nx, offset = unname(xlim[1]), 
			delta = unname(dx), refsys = st_crs(.x))
		y = create_dimension(from = 1, to = ny, offset = unname(ylim[2]),
			delta = unname(dy), refsys = st_crs(.x))
	}
	if (missing(nz)) # 2D:
		st_as_stars(values = array(values, c(x = nx[[1L]], y = ny[[1L]])), # [[1]] unnames
			dims = create_dimensions(list(x = x, y = y), get_raster()))
	else {
		z = create_dimension(from = 1, to = nz[[1]])
		st_as_stars(values = array(values, c(x = nx[[1L]], y = ny[[1L]], z = nz[[1]])), # [[1]] unnames
			dims = create_dimensions(list(x = x, y = y, z = z), get_raster()))
	}
}

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's centre; 
xy_from_colrow = function(x, geotransform) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	stopifnot(ncol(x) == 2, length(geotransform) == 6, !any(is.na(geotransform)))
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

colrow_from_xy = function(x, obj, NA_outside = FALSE) {
	if (inherits(obj, "stars"))
		obj = st_dimensions(obj)
	xy = attr(obj, "raster")$dimensions
	if (inherits(obj, "dimensions"))
		gt = get_geotransform(obj)

	if (isTRUE(st_is_longlat(st_crs(obj)))) {
		bb = st_bbox(obj)
		sign = ifelse(x[,1] < bb["xmin"], 1., ifelse(x[,1] > bb["xmax"], -1., 0.))
		x[,1] = x[,1] + sign * 360.
	}
	if (!any(is.na(gt))) { # have geotransform
		inv_gt = gdal_inv_geotransform(gt)
		if (any(is.na(inv_gt)))
			stop("geotransform not invertible")
		ret = floor(xy_from_colrow(x, inv_gt) + 1.)# will return floating point col/row numbers!!
		if (NA_outside)
			ret[ ret[,1] < 1 | ret[,1] > obj[[ xy[1] ]]$to | ret[,2] < 1 | ret[,2] > obj[[ xy[2] ]]$to, ] = NA
		ret
	} else if (is_rectilinear(obj)) {
		ix = obj[[ xy[1] ]]$values 
		if (!inherits(ix, "intervals"))
			ix = as_intervals(ix, add_last = length(ix) == dim(obj)[ xy[1] ])
		cols = find_interval(x[,1], ix)
		iy = obj[[ xy[2] ]]$values 
		if (!inherits(iy, "intervals"))
			iy = as_intervals(iy, add_last = length(iy) == dim(obj)[ xy[2] ])
		rows = find_interval(x[,2], iy) # always NA_outside
		cbind(cols, rows)
	} else if (is_curvilinear(obj)) {
		stop("colrow_from_xy not supported for curvilinear objects")
	} else
		stop("colrow_from_xy not supported for this object")
}

has_rotate_or_shear = function(x) {
	dimensions = st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!any(is.na(r$affine)) && any(r$affine != 0.0)
	} else
		FALSE
}

has_raster = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in% names(x))
}

is_regular_grid = function(x) {
	has_raster(x) && !(has_rotate_or_shear(x) || is_rectilinear(x) || is_curvilinear(x))
}

is_rectilinear = function(x) {
	d = st_dimensions(x)
	if (has_raster(x) && !is_curvilinear(x)) {
		xy = attr(d, "raster")$dimensions
		dimx = d[[ xy[1] ]]
		dimy = d[[ xy[2] ]]
		(is.na(dimx$delta) || is.na(dimy$delta)) && (!regular_intervals(dimx$values) || !regular_intervals(dimy$values))
	} else
		FALSE 
}

is_curvilinear = function(x) {
	d = st_dimensions(x)
	has_raster(x) && isTRUE(attr(d, "raster")$curvilinear)
}

which_sfc = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	which(sapply(x, function(i) inherits(i$values, "sfc")))
}

which_time = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	which(sapply(x, function(i) 
		inherits(i$values, c("POSIXct", "Date", "PCICt")) ||
		(is.character(i$refsys) && i$refsys %in% c("POSIXct", "Date", "PCICt"))))
}

has_sfc = function(x) {
	length(which_sfc(x)) > 0
}

#' retrieve coordinates for raster or vector cube cells
#'
#' retrieve coordinates for raster or vector cube cells
#' @param x object of class \code{stars}
#' @param add_max logical; if \code{TRUE}, dimensions are given with a min (x) and max (x_max) value
#' @param center logical; (only if \code{add_max} is FALSE): should grid cell center coordinates be returned (TRUE) or offset values (FALSE)? \code{center} can be a named logical vector or list to specify values for each dimension.
#' @name st_coordinates
#' @param ... ignored
#' @export
st_coordinates.stars = function(x, ..., add_max = FALSE, center = TRUE) {
	dims = st_dimensions(x)
	xy = attr(dims, "raster")$dimensions
	if (is_curvilinear(x)) {
		x = st_upfront(x) # x and y first...
		cc = setNames(data.frame(as.vector(dims[[ xy[1] ]]$values), as.vector(dims[[ xy[2] ]]$values)), xy)
		dims[ xy ] = NULL # remove
		out = do.call(expand.grid, append(list(ix = seq_len(nrow(cc))), expand_dimensions(dims, center = center))) 
			# cell offsets
		ix = out$ix
		out$ix = NULL
		out = cbind(cc[[1]][ix], cc[[2]][ix], out)
		names(out)[1:2] = xy
		out
	} else if (has_rotate_or_shear(x)) {
		if (add_max)
			stop("add_max will not work for rotated/shared rasters")
		if (isTRUE(!center)) # center = FALSE
			warning("center values are given for spatial coordinates")
		d = dim(x)
		nx = d[ xy[1] ]
		ny = d[ xy[2] ]
		setNames(as.data.frame(xy_from_colrow(as.matrix(expand.grid(seq_len(nx), seq_len(ny))) - 0.5,
			get_geotransform(x))), xy) # gives cell centers
	} else {
		if (add_max) {
			cbind(
				do.call(expand.grid, expand_dimensions(x, center = FALSE)), # cell offsets
				setNames(do.call(expand.grid, expand_dimensions(dims[xy], max = TRUE)),
					paste0(xy, "_max"))
			)
		} else {
			ed = expand_dimensions(x, center = center) # cell centers for x/y if raster
			if (length(ed) > 1)
				do.call(expand.grid, ed)
			else
				data.frame(ed)
		}
	}
}

#' @export
st_coordinates.dimensions = function(x, ...) {
	st_coordinates(st_as_stars(list(), dimensions = x), ...)
}

#' @name st_coordinates
#' @export
as.data.frame.stars = function(x, ..., add_max = FALSE, center = NA) {
	data.frame(st_coordinates(x, add_max = add_max, center = center), 
		lapply(x, function(y) structure(y, dim = NULL)))
}

#' @export
print.stars = function(x, ..., n = 1e5, abbrev = 30) {
	add_units = function(x) {
		f = function(obj) if (inherits(obj, "units")) paste0("[", enc2utf8(as.character(units(obj))), "]") else ""
		paste(names(x), sapply(x, f))
	}
	shorten = function(s) {
		if (nchar(s) > abbrev)
			paste0(substr(s, 1, abbrev), "...")
		else
			s
	}
	cat("stars object with", length(dim(x)), "dimensions and", 
		length(x), if (length(x) != 1) "attributes\n" else "attribute\n")
	if (length(x)) {
		cat("attribute(s)")
		names(x) = sapply(names(x), shorten)
		df = if (prod(dim(x)) > 10 * n) {
			cat(paste0(", summary of first ", n, " cells:\n"))                       # nocov
			as.data.frame(lapply(x, function(y) structure(y, dim = NULL)[1:n]), optional = TRUE) # nocov
		} else {
			cat(":\n")
			as.data.frame(lapply(x, function(y) structure(y, dim = NULL)), optional = TRUE)
		}
		names(df) = add_units(x)
		if (all(sapply(x, is.numeric))) {
			m_summary = function(x) { s = summary(x); if (!"NA's" %in% names(s)) s["NA's"] = 0; s }
			sums = lapply(df, summary)
			if (length(unique(lengths(sums))) > 1)
				sums = lapply(df, m_summary)
			print(do.call(rbind, sums))
		} else
			print(summary(df))
	}
	cat("dimension(s):\n")
	print(st_dimensions(x), ...)
	invisible(x)
}

#' @export
aperm.stars = function(a, perm = NULL, ...) {
	if (is.null(perm))
		perm = rev(seq_along(dim(a)))
	if (all(perm == seq_along(dim(a))) || isTRUE(all(match(perm, names(dim(a))) == seq_along(dim(a)))))
		return(a)
	d = st_dimensions(a)
	if (is.character(perm))
		perm = match(perm, names(d))
	st_stars(lapply(a, aperm, perm = perm, ...), d[perm])
}

#' @export
dim.stars = function(x) {
	d = st_dimensions(x)
	if (length(x) == 0)
		dim(d)
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
#' @param ... object(s) of class \code{star}: in case of multiple arguments, these are combined into a single stars object, in case of a single argument, its attributes are combined into a single attribute. In case of multiple objects, all objects should have the same dimensionality.
#' @param along integer; see \link{read_stars}
#' @param try_hard logical; if \code{TRUE} and some arrays have different dimensions, 
#' @param tolerance numeric; values used in \link{all.equal} to compare dimension values
#' combine those that dimensions matching to the first array
#' @param nms character; vector with array names
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' (new = c(x, x))
#' c(new) # collapses two arrays into one with an additional dimension
#' c(x, x, along = 3)
c.stars = function(..., along = NA_integer_, try_hard = FALSE, nms = names(list(...)), tolerance = sqrt(.Machine$double.eps)) {
	dots = list(...)
	if (length(dots) == 1) {
		if (!missing(along))
			warning("along argument ignored; maybe you wanted to use st_redimension?")
		dots[[1]]
	} else if (identical(along, NA_integer_)) { 
		# Case 1: merge attributes of several objects by simply putting them together in a single stars object;
		# dim does not change:
		if (identical_dimensions(dots, tolerance = tolerance))
			st_as_stars(do.call(c, lapply(dots, unclass)), dimensions = st_dimensions(dots[[1]]))
		else {
			# currently catches only the special case of ... being a broken up time series:
			along = sort_out_along(dots)
			if (!is.na(along))
				do.call(c, c(dots, along = along))
			else if (!try_hard)
				stop("don't know how to merge arrays: please specify parameter along")
			else {
				d = lapply(dots, st_dimensions)
				ident = c(TRUE, sapply(d[-1], identical, d[[1]]))
				if (!all(ident))
					warning(paste(
					"ignored subdataset(s) with dimensions different from first subdataset:", 
					paste(which(!ident), collapse = ", "), 
					"\nuse gdal_subdatasets() to find all subdataset names"))
				setNames(st_as_stars(do.call(c, 
						lapply(dots[ident], unclass)), dimensions = st_dimensions(dots[[1]])),
						nms[ident])
			}
		}
	} else {
		if (is.list(along)) { # custom ordering of ... over dimension(s) with values specified
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
	if (is.logical(drop))
		drop = which(drop)
	if (any(dim(x) > 1) && length(drop) > 0) {
		l = vector("list", length = length(x))
		f = sapply(x, is.factor)
		l[!f] = lapply(x[!f], adrop, drop = drop, one.d.array = TRUE, ...)
		l[f] = lapply(x[f], function(x) structure(x, dim = dim(x)[-drop]))
		st_as_stars(setNames(l, names(x)), dimensions = st_dimensions(x)[-drop])
	} else 
		x
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
				if (length(gt) == 6 && !any(is.na(gt))) {
					bb = rbind(c(x$from - 1, y$from - 1), c(x$to, y$from - 1), c(x$to, y$to), c(x$from - 1, y$to))
					xy = xy_from_colrow(bb, gt)
					c(xmin = min(xy[,1]), ymin = min(xy[,2]), xmax = max(xy[,1]), ymax = max(xy[,2]))
				} else
					c(xmin = x$from - 0.5, ymin = y$from - 0.5, xmax = x$to + 0.5, ymax = y$to + 0.5)
			} else {
				if (is_curvilinear(obj))
					c(xmin = min(x$values, na.rm = TRUE),
						ymin = min(y$values, na.rm = TRUE),
						xmax = max(x$values, na.rm = TRUE),
						ymax = max(y$values, na.rm = TRUE))
				else {
					rx = range(x) # dispatches into range.dimension
					ry = range(y)
					c(xmin = rx[1], ymin = ry[1], xmax = rx[2], ymax = ry[2])
				}
			}
		structure(bb, crs = st_crs(x$refsys), class = "bbox")
	} else {
		if (! has_sfc(obj))
			stop("dimensions table does not have x & y, nor an sfc dimension") # nocov
		ix = which_sfc(obj)
		if (length(ix) > 1)
			warning("returning the bounding box of the first geometry dimension")
		st_bbox(obj[[ ix[1] ]]$values)
	}
}

#' @export
st_bbox.stars = function(obj, ...) {
	st_bbox(st_dimensions(obj), ...)
}

#' set bounding box parameters of regular grid
#' @param x object of class dimensions, stars or stars_proxy
#' @param value object of class bbox
#' @param ... ignored
#' @export
st_set_bbox = function(x, value, ...) UseMethod("st_set_bbox")

#' @export
st_set_bbox.dimensions = function(x, value, ...) {
	stopifnot(inherits(value, "bbox"), is_regular_grid(x))
	xy = attr(x, "raster")$dimensions
	if (x[[ xy[1] ]]$from != 1 || x[[ xy[2] ]]$from != 1)
		stop("use st_normalize first so that dimensions start at index 1")
	d = dim(x)
	xsign = sign(x[[ xy[1] ]]$delta)
	ysign = sign(x[[ xy[2] ]]$delta)
	x[[ xy[1] ]]$offset = ifelse(xsign < 0, value["xmax"], value["xmin"])
	x[[ xy[2] ]]$offset = ifelse(ysign < 0, value["ymax"], value["ymin"])
	x[[ xy[1] ]]$delta = xsign * (value["xmax"] - value["xmin"]) / d[ xy[1] ]
	x[[ xy[2] ]]$delta = ysign * (value["ymax"] - value["ymin"]) / d[ xy[2] ]
	if (!is.na(st_crs(value)))
		st_crs(x) = st_crs(value)
	x
}

#' @export
st_set_bbox.stars = function(x, value, ...) {
	structure(x, dimensions = st_set_bbox(st_dimensions(x), value))
}

#' @export
st_set_bbox.stars_proxy = function(x, value, ...) {
	structure(x, dimensions = st_set_bbox(st_dimensions(x), value))
}


#' @export
st_crs.stars = function(x, ...) {
	st_crs(st_dimensions(x), ...)
}

#' @export
st_crs.dimensions = function(x, ...) {
	xy = attr(x, "raster")$dimensions
	if (!all(is.na(xy)))
		st_crs(x[[ xy[1] ]]$refsys)
	else if (has_sfc(x)) # search for simple features:
		st_crs(x[[ which_sfc(x)[1] ]]$values)
	else
		NA_crs_
}

#' @export
`st_crs<-.stars` = function(x, value) {
	structure(x, dimensions = st_set_crs(st_dimensions(x), value))
}

#' @export
`st_crs<-.dimensions` = function(x, value) {
	value = if (is.na(value))
			NA_crs_
		else if (is.numeric(value) || is.character(value))
			st_crs(value)
		else if (inherits(value, "crs"))
			value
		else
			stop(paste("crs of class", class(value), "not recognized"))

	# set CRS in dimensions:
	xy = attr(x, "raster")$dimensions
	if (!all(is.na(xy))) { # has x/y spatial dimensions:
		x[[ xy[1] ]]$refsys = value
		x[[ xy[2] ]]$refsys = value
	}

	# set crs of sfc's, if any:
	for (j in which_sfc(x))
		x[[ j ]]$refsys = value
	x
}


#' @export
st_geometry.stars = function(obj,...) {
	if (!has_sfc(obj))
		stop("stars object does not have a simple feature dimension")
	d = st_dimensions(obj)
	d[[ which_sfc(obj) ]]$values
}

#' @name merge
#' @aliases split
#' @param f the name or index of the dimension to split; by default the last dimension
#' @param drop ignored
#' @details split.stars works on the first attribute, and will give an error when more than one attribute is present
#' @export
split.stars = function(x, f = length(dim(x)), drop = TRUE, ...) {
	stopifnot(length(x) == 1)
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

#' merge or split stars object
#' 
#' merge attributes into a dimension, or split a dimension over attributes
#' @param x object of class \code{stars}
#' @param y needs to be missing
#' @param name name for the new dimension
#' @param ... if defined, the first unnamed argument is used for dimension values, if not defined, attribute names are used for dimension values
#' @returns merge merges attributes of a stars object into a new dimension; split splits a dimension over attributes
#' @name merge
#' @export
merge.stars = function(x, y, ..., name = "attributes") {
	dots = list(...)
	if (!missing(y))
		stop("argument y needs to be missing: merging attributes of x")
	old_dim = st_dimensions(x)
	out = do.call(abind, st_redimension(x))
	if (is.factor(x[[1]]) && is.character(out))
		out = structure(factor(as.vector(out), levels = levels(x[[1]])), dim = dim(out))
	new_dim = if (length(dots))
			create_dimension(values = dots[[1]])
		else
			create_dimension(values = names(x))
	dims = setNames(c(old_dim, list(new_dim)), make.unique(c(names(old_dim), name)))
	d = create_dimensions(dims, raster = attr(old_dim, "raster"))
	if (!is.null(names(dots)))
		names(d)[length(d)] = names(dots)
	st_as_stars(out, dimensions = d)
}

sort_out_along = function(ret) { 
	d1 = st_dimensions(ret[[1]])
	d2 = st_dimensions(ret[[2]])
	if ("time" %in% names(d1) && (isTRUE(d1$time$offset != d2$time$offset) || 
			!any(d1$time$values %in% d2$time$values)))
		"time"
	else
		NA_integer_
}


#' @export
is.na.stars = function(x, ...) {
	st_as_stars(lapply(x, is.na), dimensions = st_dimensions(x))
}

#' redimension array, or collapse attributes into a new dimension
#' 
#' redimension array, or collapse attributes into a new dimension
#' @name redimension
#' @export
st_redimension = function(x, new_dims, along, ...) UseMethod("st_redimension")

#' @export
#' @name redimension
#' @param x object of class \code{stars}
#' @param new_dims target dimensions: either a `dimensions` object or an integer vector with the dimensions' sizes
#' @param along named list with new dimension name and values
#' @param ... ignored
st_redimension.stars = function(x, new_dims = st_dimensions(x), 
		along = list(new_dim = names(x)), ...) {

	d = st_dimensions(x)
	if (inherits(new_dims, "dimensions")) {
		di = dim(new_dims)
	} else {
		di = new_dims
		new_dims = create_dimensions(di)
	}
	if (!isTRUE(all.equal(di, dim(x), check.attributes = FALSE))) {
		if (prod(dim(x)) != prod(di))
			stop("product of dim(new_dim) does not match that of x")
		for (i in seq_len(min(length(di), length(dim(x)))))
			if (di[i] == dim(x)[i])
				new_dims[[i]] = d[[i]]
		x = unclass(x)
		for (i in seq_along(x))
			dim(x[[i]]) = di
		st_stars(x, dimensions = new_dims)
	} else { # collapse attributes into dimension
		if (length(x) == 1) # only one attribute: do nothing
			x
		else {
			new_dim = create_dimension(values = along[[1]])
			dims = create_dimensions(c(d, new_dim = list(new_dim)), attr(d, "raster"))
			if (length(names(along)) == 1)
				names(dims)[names(dims) == "new_dim"] = names(along)
			ret = list(attr = do.call(abind, c(unclass(x), along = length(dim(x)) + 1)))
			st_stars(setNames(ret, paste(names(x), collapse = ".")), dimensions = dims)
		}
	}
}

#' @export
"$<-.stars" = function(x, i, value) {
	x[[i]] = value
	x
}

#' @export
"[[<-.stars" = function(x, i, value) {
	if (!is.null(value)) {
		if (prod(dim(x)) %% length(value) != 0) { # error:
			if (is.null(dim(value)))
				stop(paste("replacement has length", length(value), ", data has dim", paste(dim(x), collapse = ", ")))
			else
				stop(paste("replacement has dim", paste(dim(value), collapse = ", "), ", data has dim", paste(dim(x), collapse = ", ")))
		}
		if (inherits(value, "stars")) {
			stopifnot(length(value) == 1)
			value = value[[1]]
		}
		value = if (inherits(value, c("factor", "POSIXct")))
				structure(rep(value, length.out = prod(dim(x))), dim = dim(x), colors = attr(value, "colors"),
					rgba = attr(value, "rgba"))
			else if (!is.array(value) || !isTRUE(all.equal(dim(value), dim(x), check.attributes = FALSE)))
				array(value, dim(x))
			else
				value
	}
	NextMethod()
}

st_upfront = function(x, first = attr(st_dimensions(x), "raster")$dimensions) {
	if (!is.character(first))
		first = names(st_dimensions(x))[first]
	if (!any(is.na(first)))
		aperm(x, c(first, setdiff(names(st_dimensions(x)), first)))
	else
		x
}

#' @export
st_area.stars = function(x, ...) {
	crs = st_crs(x)
	if (is.na(crs))
		message("Missing coordinate reference system: assuming Cartesian coordinates")
	d = st_dimensions(st_upfront(x))[1:2]
	a = if (isTRUE(st_is_longlat(x)) || is_curvilinear(x))
			st_area(st_as_sfc(x, as_points = FALSE)) # has units
		else { 
			a = if (is_regular_grid(x))
					d[[1]]$delta * d[[2]]$delta
				else { # rectilinear:
					x = if (inherits(d[[1]]$values, "intervals"))
							d[[1]]$values
						else
							as_intervals(d[[1]]$values)
					y = if (inherits(d[[2]]$values, "intervals"))
							d[[2]]$values
						else
							as_intervals(d[[2]]$values)
					apply(do.call(cbind, x), 1, diff) %o% apply(do.call(cbind, y), 1, diff)
				}
			if (!is.na(crs))
				units::set_units(abs(a), paste0(crs$units, "^2"), mode = "standard")
			else
				abs(a)
		}
	lst = if (inherits(a, "units"))
			list(area = `units<-`(array(a, dim(d)), units(a)))
		else
			list(area = array(a, dim(d)))
	st_stars(lst, dimensions = d)
}

#' @export
drop_units.stars = function(x) {
	st_stars(lapply(x, drop_units), dimensions = st_dimensions(x))
}

#' Predict values, given a model object, for a stars or stars_proxy object
#' @export
#' @name predict.stars
#' @param object object of class `stars`
#' @param model model object of a class that has a predict method; check with `methods(class = class(object))`
#' @param drop_dimensions logical; if `TRUE`, remove dimensions (coordinates etc) from `data.frame` with predictors
#' @param ... arguments passed on to this predict method
#' @details separate predictors in object need to be separate attributes in object; 
#' in case they are e.g. in a band dimension, use `split(object)`
predict.stars = function(object, model, ..., drop_dimensions = FALSE) {
	obj_df = as.data.frame(st_as_stars(object))
	if (drop_dimensions)
		obj_df = obj_df[-seq_along(dim(object))]
	na_ids = which(is.na(obj_df), arr.ind = TRUE) # identify rows with NA's in the predictors
	obj_df[na_ids] = 0  # fill with something valid (e.g. 0)
	pr = try(predict(model, obj_df, ...), silent = TRUE)
	if (inherits(pr, "try-error")) { # https://github.com/r-spatial/stars/issues/448
		m = paste0("prediction on array(s) `", paste(names(object), collapse = ","), "' failed; will try to split() dimension `", tail(names(dim(object)), 1), "' over attributes")
		message(m)
		predict(split(object), model, ..., drop_dimensions = drop_dimensions) # returns
	} else {
		if (!inherits(pr, "data.frame"))
			pr = if (is.null(colnames(pr)))
					data.frame(prediction = pr)
				else
					as.data.frame(pr)
		pr[unique(data.frame(na_ids)[,1]), ] = NA # Mask with original NA's
		st_stars(lapply(pr, function(y) structure(y, dim = dim(object))), st_dimensions(object))
	}
}


#' create an array with dimension values
#' 
#' create an array with dimension values
#' @param x object of class \code{stars}
#' @param which integer; indices of the dimensions to address (default: all)
#' @return \code{stars} object with dimension values as attributes
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x1 = read_stars(tif)
#' (x = st_dim_to_attr(x1))
#' plot(x)
#' (x = st_dim_to_attr(x1, 2:3))
#' plot(x)
#' (x= st_dim_to_attr(x1, 3))
#' plot(x)
st_dim_to_attr = function(x, which = seq_along(dim(x))) {
	d = dim(x)
	l = vector("list", length = length(which))
	e = expand_dimensions(x)
	for (i in seq_along(which)) {
		l [[i]] = if (is.null(dim(e[[i]]))) {
				dp = c(which[i], setdiff(seq_along(dim(x)), which[i]))
				aperm(array(e[[ which[i] ]], d[dp]), order(dp))
			} else # curvilinear:
				array(e[[i]], d)
	}
	st_stars(setNames(l, names(d)[which]), st_dimensions(x))
}

#' @export
st_interpolate_aw.stars = function(x, to, extensive, ...) {
	ret = sf::st_interpolate_aw(st_as_sf(x), to, extensive, ...)
	geom = attr(ret, "sf_column")
	dx = dim(x)
	if (length(dx) > 2 && length(x) == 1 && length(ret) > 2) {
		ret = merge(st_as_stars(ret))
		nd = names(st_dimensions(x))
		ret = st_set_dimensions(ret, seq_along(dx), 
								names = c(geom, paste0(nd[-(1:2)], collapse = ".")))
		setNames(ret, names(x))
	} else
		ret
}

#' get the raster type (if any) of a stars object
#' @param x object of class \code{stars}
#' @return one of \code{NA} (if the object does not have raster dimensions), 
#' \code{"curvilinear"}, \code{"rectilinear"}, \code{"affine"}, or \code{"regular"}
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' st_raster_type(x)
#' @export
st_raster_type = function(x) {
	if (!has_raster(x))
		NA_character_
	else if (is_curvilinear(x))
		"curvilinear"
	else if (is_rectilinear(x))
		"rectilinear"
	else if (has_rotate_or_shear(x))
		"affine"
	else
		"regular"
}
