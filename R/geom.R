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
		
	} else { # curvilinear:
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

#' Spatially join a stars and an `sf` object
#'
#' Spatially join a stars and an `sf` object
#' @param x object of class stars
#' @param y object of class sf, or one that can be coerced into that by \link{st_as_sf}
#' @param join join function, which should return an sgbp object
#' @param ... arguments that will be passed on to the join function
#' @param as_points logical; controls whether grid cells in \code{x} will be treated as points, or as cell areas; \link{st_intersects.stars} by default will derive this from \code{x}'s metadata, or else assume areas.
#' @param what either "left1" or "right"
#' @param warn logical; if TRUE, warn on 1-to-many matches when \code{what} is \code{"left1"}
#' @returns If what is "left1", an object of class stars with the (first) value of y at spatial instances of x: when there is more than one match to a single x value, the first matching record from y is taken (and if \code{warn} is TRUE a warning is raised). If what is "inner", an object of class \code{sf} with all matching records of x and y.
#' @export
st_join.stars = function(x, y, ..., join = st_intersects, what = "left", as_points = NA, warn = TRUE) {
	if (!requireNamespace("dplyr", quietly = TRUE)) 
		stop("dplyr needed: install first?")

	if (!inherits(y, "sf")) {
		try(y <- st_as_sf(y))
		if (inherits(y, "try-error"))
			stop("argument y should be of class sf, or sfc")
	}

	x = st_upfront(x)

	if (what == "left1") {
		ix = join(x, y, ..., as_points = as_points)
		li = lengths(ix)
		if (warn && any(li > 1)) {
			warning(paste("st_join found", sum(li > 1),"1-to-n matches, taking the first match for each of those"))
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
	} else if (what == "inner") {
		ix = if (missing(join))
				st_intersects(x, y, sparse = TRUE, ..., transpose = TRUE, as_points = as_points)
			else
				join(y, st_as_sf(x, as_points = as_points), ...)
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
