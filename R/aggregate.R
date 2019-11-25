#' spatially or temporally aggregate stars object
#' 
#' spatially or temporally aggregate stars object, returning a data cube with lower spatial or temporal resolution 
#' @param x object of class \code{stars} with information to be aggregated
#' @param by object of class \code{sf} or \code{sfc} for spatial aggregation, for temporal aggregation a vector with time values (\code{Date}, \code{POSIXct}, or \code{PCICt}) that is interpreted as a sequence of left-closed, right-open time intervals or a string like "months", "5 days" or the like (see \link{cut.POSIXt}); if by is an object of class \code{stars}, it is converted to sfc by \code{st_as_sfc(by, as_points = FALSE)} thus ignoring its time component.
#' @param FUN aggregation function, such as \code{mean}
#' @param ... arguments passed on to \code{FUN}, such as \code{na.rm=TRUE}
#' @param drop logical; ignored
#' @param join join function to find matches of x to by
#' @param rightmost.closed see \link{findInterval}
#' @param left.open logical; used for time intervals, see \link{findInterval} and \link{cut.POSIXt}
#' @param as_points see \link[stars]{st_as_sf}: shall raster pixels be taken as points, or small square polygons?
#' @export
#' @examples
#' # aggregate time dimension in format Date
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' t1 = as.Date("2018-07-31")
#' x = read_stars(c(tif, tif, tif, tif), along = list(time = c(t1, t1+1, t1+2, t1+3)))
#' st_get_dimension_values(x, "time")
#'
#' # aggregate time dimension in format Date - interval (from stars 0.4-1)
#' by_t = "2 days"
#' x_agg_time2 = aggregate(x, by = by_t, FUN = max) 
#' st_get_dimension_values(x_agg_time2, "time")
#' x_agg_time - x_agg_time2
#'
#' # aggregate time dimension in format POSIXct
#' x = st_set_dimensions(x, 4, values = as.POSIXct(c("2018-07-31", 
#'                                                   "2018-08-01", 
#'                                                   "2018-08-02", 
#'                                                   "2018-08-03")), 
#'                       names = "time")
#' by_t = as.POSIXct(c("2018-07-31", "2018-08-02"))
#' x_agg_posix = aggregate(x, by = by_t, FUN = max)
#' st_get_dimension_values(x_agg_posix, "time")
#' x_agg_time - x_agg_posix
aggregate.stars = function(x, by, FUN, ..., drop = FALSE, join = st_intersects, 
		as_points = any(st_dimension(by) == 2, na.rm = TRUE), rightmost.closed = FALSE,
		left.open = FALSE) {

	if (inherits(by, "stars"))
		by = st_as_sfc(by, as_points = FALSE)

	classes = c("sf", "sfc", "POSIXct", "Date", "PCICt", "character")
	if (!inherits(by, classes))
		stop(paste("currently, only `by' arguments of class", 
			paste(classes, collapse= ", "), "supported"))

	geom = "geometry"
	drop_y = FALSE
	grps = if (inherits(by, c("sf", "sfc"))) {
			x = if (has_raster(x)) {
					ndims = 2
					drop_y = TRUE
					st_upfront(x)
				} else if (has_sfc(x)) {
					ndims = 1
					st_upfront(x, which_sfc(x))
				}

			if (inherits(by, "sf")) {
				geom = attr(by, "sf_column")
				by = st_geometry(by)
			}
	
			# find groups:
			x_geoms = if (has_raster(x)) {
					if (identical(join, st_intersection))
						x_geoms
					else
						st_as_sfc(x, as_points = as_points)
				} else
					st_dimensions(x)[[ which_sfc(x) ]]$values
			
			# unlist(join(x_geoms, by)) -> this would miss the empty groups, 
			#      and may have multiple if geometries in by overlap, hence:
			sapply(join(x_geoms, by), function(x) if (length(x)) x[1] else NA)
		} else { # time: by is POSIXct/Date or character
			ndims = 1
			x = st_upfront(x, which_time(x))
			values = expand_dimensions(x)[[1]]
			if (inherits(by, "character")) {
				i = cut(values, by, right = left.open)
				by = if (inherits(values, "Date"))
						as.Date(levels(i))
					else
						as.POSIXct(levels(i))
				i = as.integer(i)
			} else {
				if (!inherits(values, class(by)))
					warning(paste0('argument "by" is of a different class (', class(by)[1], 
						') than the time values (', class(values)[1], ')'))
				i = findInterval(values, by, left.open = left.open, rightmost.closed = rightmost.closed)
				i[ i == 0 | i == length(by) ] = NA
			}
			i
		}

	d = st_dimensions(x)
	dims = dim(d)

	agr_grps = function(x, grps, uq, FUN, ...) { 
		do.call(rbind, lapply(uq, function(i) {
				sel <- which(grps == i)
				if (!isTRUE(any(sel)))
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

	# reconstruct dimensions table:
	d[[1]] = create_dimension(values = by)
	names(d)[1] = if (inherits(by, c("POSIXct", "Date", "PCICt")))
			"time"
		else
			geom
	if (drop_y)
		d = d[-2] # y

	newdim = c(sfc = length(by), dims[-(1:ndims)])
	st_stars(lapply(x, array, dim = newdim), dimensions = d)
}

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

#' @export
aggregate.stars_proxy = function(x, by, FUN, ...) {
	if (inherits(by, "stars"))
		by = st_as_sfc(by, as_points = FALSE)
	if (!inherits(by, c("sf", "sfc", "sfg")))
		stop("aggregate.stars_proxy only implemented for spatial `by' arguments")
	by = st_geometry(by)

	# this assumes the result is small, no need to proxy
	l = lapply(seq_along(by), function(i) aggregate(st_as_stars(x[by[i]]), by[i], FUN, ...))
	do.call(c, c(l, along = list(which_sfc(l[[1]]))))
}
