#' spatially aggregate stars object to feature geometry
#' 
#' spatially aggregate stars object to feature geometry
#' @param x object of class \code{stars} with information to be aggregated
#' @param by object of class \code{stars} with aggregation geometry
#' @param FUN aggregation function
#' @param ... arguments passed on to \code{FUN}
#' @param drop logical; ignored
#' @param dim integer; geometry dimension that will be aggregated
#' @param join join function to find matches of x to by
#' @export
aggregate.stars = function(x, by, FUN, ..., drop = FALSE, 
		dim = which_sfc(x)[1], join = st_intersects) {

	dots = list(...)
	if (!(inherits(by, "sf") || inherits(by, "sfc"))) 
		stop("currently, only `by' arguments of class sf or sfc supported")

	if (length(dim) != 1 || !is.numeric(dim))
		stop("argument dim should have length 1")

	sfc = st_dimensions(x)[[dim]]$values

	by = st_geometry(by)
	i = join(sfc, by)
	if (any(lengths(i) > 1))
		warning("assigning multi-matching geometries to first geometry matched in by")
	i = sapply(i, function(x) x[1])

	# dispatch to stats::aggregate:
	ui = unlist(i)

	x = st_upfront(x, dim)

	aggr = function(.x, .by, .FUN, ...) {
		ret = rep(NA_real_, length(by))
		a = aggregate(.x, .by, .FUN, ...)
		ret[a$Group.1] = a[[2]]
		ret
	}

	ret = st_apply(x, 2:length(dim(x)), aggr, .by = list(i), .FUN = FUN, ...)
	d = st_dimensions(ret)
	d[[1]]$values = by
	d[[1]]$refsys = st_crs(by)$proj4string
	structure(ret, dimensions = d)
}

aggregate.stars = function(x, by, FUN, ..., drop = FALSE, 
		dim = which_sfc(x)[1], join = st_intersects) {

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

	dots = list(...)
	if (!(inherits(by, "sf") || inherits(by, "sfc"))) 
		stop("currently, only `by' arguments of class sf or sfc supported")

	if (length(dim) != 1 || !is.numeric(dim))
		stop("argument dim should have length 1")

	sfc = st_dimensions(x)[[dim]]$values

	by = st_geometry(by)
	i = join(sfc, by)
	if (any(lengths(i) > 1))
		warning("assigning multi-matching geometries to first geometry matched in by")
	i = sapply(i, function(x) x[1])

	# dispatch to stats::aggregate:
	ui = unlist(i)
	x = st_upfront(x, dim)

	aggr = function(.x, .by, .FUN, ...) {
		ret = rep(NA_real_, length(by))
		a = aggregate(.x, .by, .FUN, ...)
		ret[a$Group.1] = a[[2]]
		ret
	}

	ret = st_apply(x, 2:length(dim(x)), aggr, .by = list(i), .FUN = FUN, ...)
	d = st_dimensions(ret)
	d[[1]]$values = by
	d[[1]]$refsys = st_crs(by)$proj4string
	structure(ret, dimensions = d)
}
