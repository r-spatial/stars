#' spatially or temporally aggregate stars object
#' 
#' spatially or temporally aggregate stars object, returning a data cube with lower spatial or temporal resolution 
#' @param x object of class \code{stars} with information to be aggregated
#' @param by object of class \code{sf}, \code{sfc} or \code{POSIXct} with aggregation geometry
#' @param FUN aggregation function, such as \code{mean}
#' @param ... arguments passed on to \code{FUN}, such as \code{na.rm=TRUE}
#' @param drop logical; ignored
#' @param join join function to find matches of x to by
#' @export
aggregate.stars = function(x, by, FUN, ..., drop = FALSE, join = st_intersects) {

	dots = list(...)
	classes = c("sf", "sfc", "POSIXct", "PCICt")
	if (!(inherits(by, classes)))
		stop(paste("currently, only `by' arguments of class", paste(classes, collapse= ", "), "supported"))

	# aggregate is done over one or more dimensions
	# say we have dimensions 1,...,k and we want to aggregate over i,...,j
	# with 1 <= i <= j <= k; 
	# let |n| = j-1+1 be the number of dimensions to aggregate over, n
	# let |m| = k - n be the number of remaining dimensions, m
	# permute the cube such that the n dimensions are followed by the m
	# rearrange the cube to a 2D matrix with |i| x ... x |j| rows, and remaining cols
	# find the grouping of the rows
	# (rearrange such that groups are together)
	# for each sub matrix, belonging to a group, do
	#   apply FUN to every column
	#   assing the resulting row to the group
	# now we have |g| rows, with |g| the number of groups
	# assign each group to the target group of "by"
	# redimension the matrix such that unaffected dimensions match again

	if (inherits(by, c("sf", "sfc"))) {
		if (has_raster(x)) {
			ndims = 2
			x_had_raster = TRUE
			x = st_upfront(x)
		} else if (has_sfc(x)) {
			ndims = 1
			x_had_raster = FALSE
			x = st_upfront(x, which_sfc(x))
		}
		if (inherits(by, "sf"))
			by = st_geometry(by)

	} else {
		stop("temporal aggregation not implemented yet")
	}

	d = st_dimensions(x)
	dims = dim(d)

	# find groups:
	x_geoms = if (has_raster(x))
			st_as_sfc(x, as_points = TRUE)
		else
			st_dimensions(x)[[ which_sfc(x) ]]$values
	
	grps = unlist(join(x_geoms, by))

	agr_grps = function(x, grps, uq, FUN, ...) { 
		do.call(rbind, lapply(uq, function(i) {
				sel <- grps == i
				if (!any(sel))
					rep(NA_real_, ncol(x))
				else
					apply(x[sel, , drop = FALSE], 2, FUN, ...)
			}
		))
	}

	# rearrange:
	x = structure(x, dimensions = NULL, class = NULL) # unclass
	newdims = c(prod(dims[1:ndims]), prod(dims[-(1:ndims)]))
	for (i in seq_along(x))
		x[[i]] = agr_grps(array(x[[i]], newdims), grps, seq_along(by), FUN, ...)

	# recreate dimensions:
	if (x_had_raster)
		d = d[-2] # y
	d[[1]] = create_dimension(values = by)
	names(d)[1] = "geometry" # FIXME: anything better?

	newdim = c(sfc = length(by), dims[-(1:ndims)])
	st_stars(lapply(x, array, dim = newdim), dimensions = d)
}
