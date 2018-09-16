# sf conversion things

#' @export
st_as_sfc.stars = function(x, ..., as_points = st_dimensions(x)$x$point, # FIXME: hard-coded x
		which = seq_len(prod(dim(x)[1:2]))) {

	if (is_curvilinear(x)) {
		stopifnot(isTRUE(as_points)) # FIXME:
		d = st_dimensions(x)
		xy = attr(d, "raster")$dimensions
		pts = cbind(as.vector( d[[ xy[1] ]]$values), as.vector( d[[ xy[2] ]]$values))
		st_sfc(lapply(seq_len(nrow(pts)), function(i) st_point(pts[i,])), crs = st_crs(x))[which] # FIXME: more efficient via data.frame?
	} else {
		r = attr(st_dimensions(x), "raster")
		gt = get_geotransform(x)
		st_as_sfc(st_dimensions(x)[r$dimensions], ..., as_points = as_points, which = which, geotransform = gt)
	}
}


#' @export
st_as_stars.sfc = function(.x, ..., FUN = length, as_points = TRUE) {
	st = st_as_stars(st_bbox(.x), ...)
	sfc = st_as_sfc(st, as_points = as_points)
	i = st_intersects(sfc, .x)
	vals = sapply(i, FUN)
	st[[1]] = array(vals, dim(st[[1]]))
	st
}

#' replace x y raster dimensions with simple feature geometry list (points, or polygons = rasterize)
#' @param x object of class \code{stars}
#' @param as_points logical; if \code{TRUE}, generate points at cell centers, else generate polygons
#' @param ... arguments passed on to \code{st_as_sfc}
#' @param na.rm logical; remove cells with all missing values?
#' @return object of class \code{stars} with x and y raster dimensions replaced by a single sfc geometry list column containing either points or square polygons
#' @export
st_xy2sfc = function(x, as_points = st_dimensions(x)$x$point, ..., na.rm = TRUE) { # FIXME: hard-coded x

	d = st_dimensions(x)
	olddim = dim(x)

	if (! has_raster(x))
		stop("x and/or y not among dimensions")

	xy_pos = match(c("x", "y"), names(d)) # FIXME: hard coded raster dims
	if (! all(xy_pos == 1:2))
		stop("x and y need to be first and second dimension")

	stopifnot(identical(which(names(d) %in% c("x", "y")), 1:2)) # FIXME: hard coded raster dims

	# find which records are NA for all attributes:
	a = abind(x, along = length(dim(x)) + 1)
	keep = if (na.rm)
			as.vector(apply(a, c(1,2), function(x) !all(is.na(x))))
		else
			rep(TRUE, prod(dim(x)[1:2]))

	# flatten two dims x,y to one dim sfc (replacing "x")
	sfc = st_as_sfc(x, as_points = as_points, ..., which = which(keep))
	# overwrite x:
	d[["x"]] = create_dimension(from = 1, to = length(sfc), values = sfc)
	# rename x to sfc:
	names(d)[names(d) == "x"] = "sfc"
	# remove y:
	d[["y"]] = NULL
	# flatten arrays:
	for (i in seq_along(x))
		dim(x[[i]]) = c(sfc = length(keep), olddim[-xy_pos]) 
	# reduce arrays to non-NA cells:
	if (na.rm) {
		for (i in seq_along(x))
			x[[i]] = switch(as.character(length(dim(x[[i]]))), 
				"1" = x[[i]][which(keep),drop=FALSE],
				"2" = x[[i]][which(keep),,drop=FALSE],
				"3" = x[[i]][which(keep),,,drop=FALSE],
				"4" = x[[i]][which(keep),,,,drop=FALSE], # etc -- FIXME: use tidy eval here
				)
	}

	structure(x, dimensions = d)
}

#' @export
st_as_sf.stars = function(x, ..., as_points = st_dimensions(x)$x$point, na.rm = TRUE) { # FIXME: hard-coded x

	if (has_raster(x))
		x = st_xy2sfc(x, as_points = as_points, ..., na.rm = na.rm)

	if (! has_sfc(x))
		stop("no feature geometry column found")

	# FIXME: this probably only works for 2D arrays, now
	sfc = st_dimensions(x)$sfc$values
	# may choose units method -> is broken; drop units TODO: if fixed:
	dfs = lapply(x, function(y) as.data.frame(y))
	nc = sapply(dfs, ncol)
	df = do.call(cbind, dfs)
	if (length(dim(x)) > 1) {
		if (length(unique(names(df))) == 1) {
			labels = format(expand_dimensions(st_dimensions(x))[[2]])
			names(df) = if (length(labels) == ncol(df))
					labels
				else
					apply(expand.grid(labels, names(x))[,2:1], 1, paste0, collapse = " ")
		}
	} else
		names(df) = names(x)
	st_sf(df, geometry = sfc)
}

#' @name st_as_stars
#' @export
st_as_stars.sf = function(.x, ...) {
	geom = st_geometry(.x)
	if (length(list(...)))
		stop("secondary arguments ignored")
	dimensions = create_dimensions(list(sfc = 
			create_dimension(1, length(geom), refsys = st_crs(geom)$proj4string, values = geom)))
	lst = lapply(st_set_geometry(.x, NULL), function(x) { dim(x) = length(geom); x })
	st_as_stars(lst, dimensions = dimensions)
}
