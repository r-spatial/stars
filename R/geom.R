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
	if (has_raster(x) && !is_curvilinear(x) &&
			inherits(y, "sfc_POINT") && 
			!as_points) {

		cr = colrow_from_xy(st_coordinates(y), x, NA_outside = TRUE)
		ncells = prod(dim(d[xy]))
		ncols = dim(x)[ xy[1] ]
		ix = (cr[,2] - 1) * ncols + cr[,1] # for all points, the 1-based index in (as.vector) x
		# now create the TRANSPOSE of the sgbp:
   		ret = structure(lapply(as.list(ix), function(x) if (is.na(x)) integer(0) else x),
       		predicate = "intersects",
       		region.id = seq_along(y),
       		ncol = ncells,
       		class = "sgbp")
		if (! sparse)
			ret = as.matrix(ret)

		if (! transpose) 
			t(ret)
		else
			ret
		
	} else { # rectilinear, curvilinear, or polygons:
		ret = st_intersects(st_as_sf(x, as_points = as_points), y, sparse = sparse, ...)
		if (! sparse)
			ret = as.matrix(ret)

		if (transpose) 
			t(ret)
		else
			ret
	}
}

# FIXME: of_y should be left/right/full?

#' @export
st_join.stars = function(x, y, ..., what = "left") {
	if (!requireNamespace("dplyr", quietly = TRUE)) 
		stop("dplyr needed: install first?")
	if (inherits(y, "sfc"))
		y = st_sf(y)
	if (!inherits(y, "sf"))
		stop("argument y should be of class sf, or sfc")
	x = st_upfront(x)

	if (what == "left") {
		ix = st_intersects(x, y, sparse = TRUE, ..., transpose = FALSE)
		li = lengths(ix)
		if (any(li > 1)) {
			warning("st_join has n-to-m matches, taking first match for each n")
			ix[li > 1] = lapply(ix[li > 1], head, 1)
		}
		sel = li > 0
		ix = unlist(ix)
		# white-out existing cells not matched:
		for (i in seq_along(x))
			x[[i]][!sel] = NA
		# add matching fields from y:
		for (i in setdiff(names(y), attr(y, "sf_column"))) {
			a = array(NA_real_, dim(x)) # FIXME: take care of other types here 
			a[sel] = y[[i]][ix]
			x[[ i ]] = a
		}
		x
	} else if (what == "right") {
		ix = st_intersects(x, y, sparse = TRUE, ..., transpose = TRUE)
		li = lengths(ix)
		i = unlist(ix) # sequence of x indexes
		j = rep(seq_along(ix), li) # possibly repeated y indexes
		stopifnot(length(i) == length(j))
		recycle = prod(dim(x)[-(1:2)]) # 1 in case of no dimensions beyond x and y
		i = rep(i, recycle)
		j = rep(j, recycle)
		st_sf(dplyr::bind_cols(as.data.frame(x)[i,], y[j,]))
	} else
		stop('value for "what" not supported')
}
