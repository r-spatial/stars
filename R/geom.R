#' spatial intersect predicate for stars and sfc object
#'
#' spatial intersect predicate for stars and sfc object
#' @param x object of class stars
#' @param y object that has an `st_geometry` method: of class `sf` or `sfc`, or `stars` object with an `sfc` dimension
#' @param sparse logical; if TRUE, return the a sparse logical matrix (object of class `sgbp`), if FALSE, return a logical matrix
#' @param as_points logical, should grid cells be considered as points (TRUE) or polygons (FALSE)? Default: FALSE and warning emitted
#' @param ... ignored, or passed on to `st_intersects.sf` for curvilinear grids
#' @param transpose logical; should the transpose of the `sgbp` object be returned?
#' @return `sgbp` object if sparse = TRUE, logical matrix otherwise
#' @details curvilinear grids are always converted to polygons, so points on grid boundaries may intersect with two cells touched; for other grids each cell boundary or corner belongs only to one cell.
#' 
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
	ret = if (has_raster(x) && !is_curvilinear(x) &&
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
	} else { # curvilinear or !as_points:
		ret = st_intersects(st_as_sf(x, as_points = as_points, na.rm = FALSE), y, 
			sparse = sparse, ...)
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
#' @param join the join function, which should return an sgbp object; see details
#' @param ... arguments that will be passed on to the join function
#' @param as_points logical; controls whether grid cells in \code{x} will be treated as points, or as cell areas; the \link{st_intersects.stars} method by default will derive this from \code{x}'s metadata, or else assume areas.
#' @param what "left1", "right" or "inner"; see details
#' @param warn logical; if TRUE, warn on 1-to-many matches when \code{what} is \code{"left1"}
#' @return If what is "left1", an object of class stars with the (first) value of y at spatial instances of x
#' @details When there is more than one match to a single x value, the first matching record from y is taken (and if \code{warn} is TRUE a warning is raised). If what is "inner", an object of class \code{sf} with all matching records of x and y.
#' @export
st_join.stars = function(x, y, join = st_intersects, ..., what = "left1", as_points = NA, warn = TRUE) {
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
			na_mode = NA
			storage.mode(na_mode) = storage.mode(y[[i]])
			a = array(na_mode, dim(x))
			attr(a, "units") = attr(y[[i]], "units")
			attr(a, "levels") = attr(y[[i]], "levels")
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
		if (length(dim(x)) > 2) {
			recycle = prod(dim(x)[-(1:2)]) # 1 in case of no dimensions beyond x and y
			i = rep(i, recycle)
			j = rep(j, recycle)
			# FIXME: create a stars object with an sfc dimension
			warning("a stars object should probably have been created here; please file an issue")
		}
		st_sf(dplyr::bind_cols(as.data.frame(x)[i,], y[j,]))
	} else
		stop('value for "what" not supported')
}
