#' subset stars objects
#' 
#' subset stars objects
#' @name stars_subset
#' @param x object of class \code{stars}
#' @param i first selector: integer, logical or character vector indicating attributes to select, or object of class \code{sf}, \code{sfc}, \code{bbox}, or \code{stars} used as spatial selector; see details
#' @param ... further (logical or integer vector) selectors, matched by order, to select on individual dimensions
#' @param drop logical; if \code{TRUE}, degenerate dimensions (with only one value) are dropped 
#' @param crop logical; if \code{TRUE} and parameter \code{i} is a spatial geometry (\code{sf} or \code{sfc}) object, the extent (bounding box) of the result is cropped to match the extent of \code{i} using \link{st_crop}. Cropping curvilinear grids is not supported.
#' @details If \code{i} is an object of class \code{sf}, \code{sfc} or \code{bbox}, the spatial subset covering this geometry is selected, possibly followed by cropping the extent. Array values for which the cell centre is not inside the geometry are assigned \code{NA}. If \code{i} is of class \code{stars}, and attributes of \code{i} are \code{logical}, cells in \code{x} corresponding to \code{NA} or \code{FALSE} cells in \code{i} are assigned an \code{NA}. Dimension ranges containing negative values or \code{NA} may be partially supported.
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
#' # with i of class stars:
#' x[x > 75] # generates lots of NA's; pattern for each band
#' x[x[,,,1] > 75] # recycles a single band template for all bands
"[.stars" = function(x, i = TRUE, ..., drop = FALSE, crop = !is_curvilinear(x)) {
	missing.i = missing(i)

	# special case: i is sf/sfc/bbox
	if (! missing.i && inherits(i, c("sf", "sfc", "bbox"))) {
		x = if (has_raster(x))
			st_crop(x, i, crop = crop, ...)
		else {
			x = st_upfront(x, which_sfc(x))
			sel = which(lengths(st_intersects(st_geometry(x), i)) > 0)
			x[, sel]
		}
		return(x)
	}
	# special case: i is stars
	if (! missing.i && inherits(i, "stars")) {
		stopifnot(all(sapply(i, is.logical)))
		fun = function(x, y) { x[is.na(y) | !y] = NA; x }
		ret = mapply(fun, x, i, SIMPLIFY = FALSE)
		return(st_as_stars(ret, dimensions = st_dimensions(x)))
	}

	d = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(dim(x)))

	x = unclass(x)
	if (length(x))
		x = x[i]
	mc = match.call(expand.dots = TRUE)
	# remove [, x, i from mc:
	mc = if (missing.i)
			mc[-(1:2)]
		else
			mc[-(1:3)]

	do_select = FALSE
	for (i in seq_along(mc)) { 
		if ((is.call(mc[[i]]) || is.name(mc[[i]])) && !identical(as.character(mc[[i]]), "")) # try to "get" it:
			mc[[i]] = eval(mc[[i]], parent.frame())
		if (is.numeric(mc[[i]]) || is.call(mc[[i]]) || is.name(mc[[i]]) || is.character(mc[[i]])) { # FIXME: or something else?
			args[[i]] = if (is.character(mc[[i]])) {
						m = match(mc[[i]], d[[i]]$values)
						if (length(m) == 0 || any(is.na(m)))
							stop("selecting using invalid value label(s)?")
						m
					} else
						mc[[i]]
			do_select = TRUE
		}
	}

	if (is_curvilinear(d)) { # fill the row/col selectors for $values matrices:
		args_xy = rep(list(rlang::missing_arg()), 2)
		xy = attr(d, "raster")$dimensions
		for (i in seq_along(mc)) {
			if (is.numeric(mc[[i]]) || is.call(mc[[i]]) || is.name(mc[[i]])) { # FIXME: or something else?
				if (names(d)[i] == xy[1])
					args_xy[[1]] = mc[[i]]
				if (names(d)[i] == xy[2])
					args_xy[[2]] = mc[[i]]
			}
		}
	}

	# subset arrays:
	args[["drop"]] = FALSE
	for (i in names(x))
		x[[i]] = structure(eval(rlang::expr(x[[i]][ !!!args ])), levels = attr(x[[i]], "levels"),
			colors = attr(x[[i]], "colors"), exclude = attr(x[[i]], "exclude"))

	# now do dimensions:
	if (do_select) {
		ed = expand_dimensions(d)
		xy = attr(d, "raster")$dimensions
		if (is_curvilinear(d)) { # subset curvilinear lat/lon matrices/rasters: can't do one-at-a-time!
			d[[ xy[1] ]]$values = eval(rlang::expr(d[[ xy[1] ]]$values[!!!args_xy]))
			d[[ xy[2] ]]$values = eval(rlang::expr(d[[ xy[2] ]]$values[!!!args_xy]))
		}
		
		# dimensions:
		#mc0 = mc[1:3] # "[", x, first dim
		for (i in seq_along(d)) { # one-at-a-time:
			name_i = names(d)[i]
			argi = args[i]
			if (!(is_curvilinear(d) && name_i %in% xy) &&  # as that case was handled above
					!(is.name(argi[[1]]) && all(argi[[1]] == rlang::missing_arg())) &&  # empty arg
					is.numeric(e <- eval(argi[[1]])) && !any(is.na(e)) && !all(diff(e) == 1)) # sequence with gaps
				d[[i]]$values = if (isTRUE(d[[i]]$point) || !is.numeric(unclass(ed[[i]][1])))
						ed[[i]]
					else
						as_intervals(ed[[i]], add_last = TRUE)
			d[[i]] = eval(rlang::expr(d[[i]] [!!!argi]))
		}
	}
	x = st_as_stars(x, dimensions = d)
	if (drop)
		adrop(x)
	else
		x
}

#' @name stars_subset
#' @param downsample downsampling rate used in case \code{i} is a \code{stars_proxy} object
#' @param value array of dimensions equal to those in \code{x}, or a vector or value that will be recycled to such an array
#' @export
#' @details in an assignment (or replacement form, \code{[<-}), argument \code{i} needs to be either (i) a \code{stars} object with logical attribute(s) that has dimensions matching (possibly after recycling) those of \code{x}, in which case the \code{TRUE} cells will be replaced and \code{i} and/or \code{value} will be recycled to the dimensions of the arrays in \code{x}, or (ii) a length-one integer or character vector indicating which array to replace, in which case \code{value} may be stars object or a vector or array (that will be recycled).
#' @examples
#' x = read_stars(tif)
#' # replace, using a logical stars selector: cuts all values above 90 to 90
#' x[x > 90] = 90
#' # replace a single attribute when there are more than one:
#' s = split(x)
#' names(s) = paste0("band", 1:6)
#' # rescale only band 1:
#' s[1] = s[1] * 0.75 
#' # rescale only attribute named "band2":
#' s["band2"] = s["band2"] * 0.85 
#' # create a new attribute from a numeric vector:
#' s["rnorm"] = rnorm(prod(dim(s))) 
#' s
"[<-.stars" = function(x, i, value) {
	if (inherits(i, "stars")) {
		fun = function(x, y, value) { x[y] = value; x }
		st_as_stars(mapply(fun, x, i, value = value, SIMPLIFY = FALSE), dimensions = st_dimensions(x))
	} else if (inherits(i, c("numeric", "character")) && length(i) == 1) {
		if (inherits(value, "stars")) {
			stopifnot(length(value) == 1, first_dimensions_match(x, value))
			value = value[[1]]
		}
		if (is.numeric(i))
			stopifnot(i > 0, i <= length(x))
		y = unclass(x)
		y[[i]] = array(value, dim(x))
		st_as_stars(y, dimensions = st_dimensions(x))
	} else
		stop("selector i should be a stars object or a lenght-one integer or character vector")
}

#' @export
st_intersects.bbox = function(x, y, ...) { # FIXME: segmentize first if geographic coords? see sf::st_sample
	if (!inherits(y, "sfc"))
		y = st_as_sfc(y)
	st_intersects(st_as_sfc(x), y, ...)
}

#' crop a stars object
#' 
#' crop a stars object
#' @name st_crop
#' @export
#' @param x object of class \code{stars}
#' @param y object of class \code{sf}, \code{sfc} or \code{bbox}; see Details below.
#' @param epsilon numeric; factor to shrink the bounding box of \code{y} towards its center before cropping.
#' @param as_points logical; only relevant if \code{y} is of class \code{sf} or \code{sfc}: if \code{FALSE}, treat \code{x} as a set of points, else as a set of small polygons. Default: \code{TRUE} if \code{y} is two-dimensional, else \code{FALSE}; see Details
#' @param ... ignored
#' @param crop logical; if \code{TRUE}, the spatial extent of the returned object is cropped to still cover \code{obj}, if \code{FALSE}, the extent remains the same but cells outside \code{y} are given \code{NA} values.
#' @param normalize logical; if \code{TRUE} then pass the cropped object to \code{\link[sf]{st_normalize}} before returning.
#' @details for raster \code{x}, \code{st_crop} selects cells that intersect with \code{y}. 
#' For intersection, are raster cells interpreted as points or as small polygons? 
#' If \code{y} is of class \code{stars}, \code{x} raster cells are interpreted as points; if \code{y} is of class \code{bbox}, \code{x} cells are interpreted as cells (small polygons). Otherwise, if \code{as_points} is not given, cells are interpreted as points if \code{y} has a two-dimensional geometry.
#' 
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
#' # equivalent:
#' st_crop(l7, bb)
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
st_crop.stars = function(x, y, ..., crop = TRUE, epsilon = sqrt(.Machine$double.eps), 
		as_points = all(st_dimension(y) == 2, na.rm = TRUE), normalize = FALSE) {
	x = st_upfront(x) # put spatial dimensions up front; https://github.com/r-spatial/stars/issues/457
	d = dim(x)
	dm = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(d)+1)
	if (inherits(y, c("stars", "sf", "sfc", "bbox")) && st_crs(x) != st_crs(y))
		stop("for cropping, the CRS of both objects have to be identical")
	if (inherits(y, "stars")) {
		as_points = TRUE
		y = st_as_sfc(st_bbox(y))
	}
	if (!as.matrix(st_intersects(st_bbox(x), st_bbox(y))))
		warning("st_crop: bounding boxes of x and y do not overlap")
	if (crop && (is_regular_grid(x) || has_rotate_or_shear(x))) {
		rastxy = attr(dm, "raster")$dimensions
		xd = rastxy[1]
		yd = rastxy[2]
		bb = st_bbox(y)
		if (any(is.na(as.numeric(bb)))) # as.numeric() can go after sf 0.7-5
			stop("NA values in bounding box of y")
		if (epsilon != 0)
			bb = bb_shrink(bb, epsilon)
		if (!all(is.na(colrow_from_xy(matrix(bb, 2, byrow = TRUE), dm, NA_outside = TRUE)))) { 
			cr = colrow_from_xy(matrix(bb, 2, byrow = TRUE), dm, NA_outside = FALSE) # FALSE: https://github.com/r-spatial/stars/issues/455
			cr[,1] = cr[,1] - dm[[xd]]$from + 1
			cr[,2] = cr[,2] - dm[[yd]]$from + 1
			for (i in seq_along(d)) {
				if (names(d[i]) == xd)
					args[[i+1]] = seq(max(1, cr[1, 1], na.rm = TRUE), min(d[xd], cr[2, 1], na.rm = TRUE))
				if (names(d[i]) == yd) {
					if (dm[[ yd ]]$delta < 0)
						cr[1:2, 2] = cr[2:1, 2]
					args[[i+1]] = seq(max(1, cr[1, 2], na.rm = TRUE), min(d[yd], cr[2, 2], na.rm = TRUE))
				}
			}
			x = eval(rlang::expr(x[!!!args]))
		}
	} else if (crop)
		warning("crop only crops regular grids: maybe use st_warp() first?")

	if (!inherits(y, "bbox")) { # post-process: burn in geometry mask
		dxy = attr(dm, "raster")$dimensions
		xy_grd = if (is_curvilinear(x) || !as_points) # FIXME: for curvilinear as_points should work too!
				st_as_sfc(st_dimensions(x)[dxy], as_points = as_points, geotransform = st_geotransform(x))
			else
				st_as_sf(do.call(expand.grid, expand_dimensions.stars(x)[dxy]), coords = dxy, crs = st_crs(x))
		inside = st_intersects(st_union(y), xy_grd)[[1]]
		d = dim(x) # cropped x
		mask = rep(TRUE, prod(d[dxy]))
		mask[inside] = FALSE
		mask = array(mask, d) # recycles over dims >= 3
		for (i in seq_along(x))
			x[[i]][mask] = NA
	}
	if (normalize[1]) 
		st_normalize(x)
	else 
		x
}

#' @export
st_normalize.stars = function(x, domain = c(0, 0, 1, 1), ...) {
	stopifnot(all(domain == c(0,0,1,1)))
	if (has_raster(x)) {
		x = st_upfront(x)
		d = st_dimensions(x)
		if (d[[1]]$from != 1) {
			d[[1]]$offset = d[[1]]$offset + (d[[1]]$from - 1) * d[[1]]$delta
			d[[1]]$to = d[[1]]$to - d[[1]]$from + 1
			d[[1]]$from = 1
		}
		if (d[[2]]$from != 1) {
			d[[2]]$offset = d[[2]]$offset + (d[[2]]$from - 1) * d[[2]]$delta
			d[[2]]$to = d[[2]]$to - d[[2]]$from + 1
			d[[2]]$from = 1
		}
		st_stars(x, dimensions = d)
	} else
		x
}

#' @name stars_subset
#' @param which character or integer; dimension(s) to be flipped
#' @export
#' @return \code{st_flip} flips (reverts) the array values along the chosen dimension 
#' without(s) changing the dimension properties
#' @examples
#' lc = read_stars(system.file("tif/lc.tif", package = "stars"))
#' x = c(orig = lc, 
#'       flip_x = st_flip(lc, "x"), 
#'       flip_y = st_flip(lc, "y"), 
#'       flip_xy = st_flip(lc, c("x", "y")), 
#'       along = 3)
#' plot(x)
st_flip = function(x, which = 1) {
	if (is.character(which))
		which = match(which, names(dim(x)))
	stopifnot(all(which %in% seq_along(dim(x))))
	dims = lapply(dim(x), seq_len)
	for (i in which)
	  dims[[ i ]] = rev(dims[[ i ]])
	for (i in seq_along(x))
		x[[i]] = do.call(`[`, c(list(x[[i]]), dims))
	x
}
