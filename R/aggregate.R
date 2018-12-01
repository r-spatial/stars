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
aggregate.stars = function(x, by, FUN, ..., drop = FALSE, dim = which_sfc(x)[1], join = st_intersects) {

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

	if (dim != 1)
		x = aperm(x, c(dim, setdiff(seq_len(dim(x)), dim)))

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
