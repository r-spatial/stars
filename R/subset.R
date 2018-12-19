#' subset stars objects
#' 
#' subset stars objects
#' @name stars_subset
#' @param x object of class \code{stars}
#' @param i first selector: integer, logical or character vector indicating attributes to select, or object of class \code{sf} or \code{sfc} used as spatial selector; see details
#' @param ... further (logical or integer vector) selectors, matched by order, to select on individual dimensions
#' @param drop logical; if \code{TRUE}, degenerate dimensions (with only one value) are dropped 
#' @param crop logical; if \code{TRUE} and parameter \code{i} is a spatial geometry (\code{sf} or \code{sfc}) object, the extent (bounding box) of the result is cropped to match the extent of \code{i} using \link{st_crop}. Cropping curvilinear grids is not supported.
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
"[.stars" = function(x, i = TRUE, ..., drop = FALSE, crop = !is_curvilinear(x)) {
	missing.i = missing(i)
	# special case:
	if (! missing.i && inherits(i, c("sf", "sfc", "bbox"))) {
		x = if (has_raster(x))
			st_crop(x, i, crop = crop, ...)
		else {
			ix = which_sfc(x)
			if (ix != 1) # put first
				x = aperm(x, c(ix, setdiff(seq_len(dim(x)), ix)))
			sfc = st_geometry(x)
			sel = which(lengths(st_intersects(sfc, i)) > 0)
			x[, sel]
		}
		return(x)
	}

	d = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(dim(x)))
	x = unclass(x)[i]
	mc = match.call(expand.dots = TRUE)
	# remove [, x, i from mc:
	mc = if (missing.i)
			mc[-(1:2)]
		else
			mc[-(1:3)]
	do_select = FALSE
	for (i in seq_along(mc)) { 
		if (is.numeric(mc[[i]]) || is.call(mc[[i]])) { # FIXME: or something else?
			args[[i]] = mc[[i]]
			do_select = TRUE
		}
	}

	# subset arrays:
	args[["drop"]] = FALSE
	for (i in names(x))
		x[[i]] = structure(eval(rlang::expr(x[[i]][ !!!args ])), levels = attr(x[[i]], "levels"))

	# now do dimensions:
	if (do_select) {
		ed = expand_dimensions(d)
		xy = attr(d, "raster")$dimensions
		if (is_curvilinear(d)) { # subset curvilinear lat/lon matrices/rasters: can't do one-at-a-time!
			d[[ xy[1] ]]$values = eval(rlang::expr(d[[ xy[1] ]]$values[!!!args]))
			d[[ xy[2] ]]$values = eval(rlang::expr(d[[ xy[2] ]]$values[!!!args]))
		}
		
		# dimensions:
		#mc0 = mc[1:3] # "[", x, first dim
		for (i in seq_along(d)) { # one-at-a-time:
			name_i = names(d)[i]
			argi = args[i]
			if (! (is_curvilinear(d) && name_i %in% xy) &&  # as that was handled above
					length(argi) > 0 && is.numeric(argi[[1]]) && ! all(diff(eval(argi[[1]])) == 1))
				d[[i]]$values = ed[[i]]
			d[[i]] = eval(rlang::expr(d[[i]] [!!!argi]))
		}
	}
	if (drop)
		adrop(st_as_stars(x, dimensions = d))
	else
		st_as_stars(x, dimensions = d)
}

#' @name stars_subset
#' @param value array of dimensions equal to those in \code{x}, or a vector or value that will be recycled to such an array
#' @export
#' @details in an assignment (or replacement form, \code{[<-}), argument \code{i} needs to be a \code{stars} object with dimensions identical to \code{x}, and \code{value} will be recycled to the dimensions of the arrays in \code{x}.
"[<-.stars" = function(x, i, value) {
	if (!inherits(i, "stars"))
		stop("selector should be a stars object")
	fun = function(x, y, value) { x[y] = value; x }
	st_as_stars(mapply(fun, x, i, value = value, SIMPLIFY = FALSE), dimensions = st_dimensions(x))
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
		cr[1,] = cr[1,] - dm[[xd]]$from + 1
		cr[2,] = cr[2,] - dm[[yd]]$from + 1
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
	dxy = attr(dm, "raster")$dimensions
	xy_grd = if (is_curvilinear(x))
			st_as_sfc(st_dimensions(x)[dxy], as_points = TRUE)
		else
			st_as_sf(do.call(expand.grid, expand_dimensions.stars(x)[dxy]), coords = dxy, crs = st_crs(x))
	inside = st_intersects(y, xy_grd)[[1]]
	d = dim(x) # cropped x
	mask = rep(NA_real_, prod(d[dxy]))
	mask[inside] = 1
	x * array(mask, d) # replicates over secondary dims
}
