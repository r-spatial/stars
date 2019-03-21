#' @export
st_intersects.stars = function(x, y, sparse = TRUE, ..., as_points = NA, transpose = FALSE) { 

	y = st_geometry(y)
	d = st_dimensions(x)
	xy = attr(d, "raster")$dimensions
	if (is.na(as_points)) {
		p = d[[ xy[1] ]]$point || d[[ xy[2] ]]$point
		if (is.na(p)) {
			warning("as_points is NA: assuming here that raster cells are small polygons, not points")
			as_points = FALSE
		} else
			as_points = p
	}
	if (has_raster(x) && inherits(y, "sfc_POINT") && !as_points) {
		stopifnot(isTRUE(sparse))
		cr = colrow_from_xy(st_coordinates(y), x, NA_outside = TRUE)
		ncells = prod(dim(d[xy]))
		ncols = dim(x)[ xy[1] ]
		ix = (cr[,2] - 1) * ncols + cr[,1] # for all points, the 1-based index in (as.vector) x
		# create the transpose:
    	ret = structure(lapply(as.list(ix), function(x) if (is.na(x)) integer(0) else x),
        	predicate = "intersects",
        	region.id = seq_along(y),
        	ncol = ncells,
        	class = "sgbp")
		if (! sparse)
			ret = as.matrix(ret)
		# then transpose if needed:
		if (!transpose) 
			t(ret)
		else
			ret
	} else
		st_intersects(st_as_sf(x, as_points = as_points), y, sparse = sparse, ...)
}
