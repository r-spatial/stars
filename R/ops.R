#' S3 Ops Group Generic Functions for stars objects
#'
#' Ops functions for stars objects, including comparison, product and divide, add, subtract
#'
#' @param e1 object of class \code{stars}
#' @param e2 object of class \code{stars}
#'
#' @return object of class \code{stars}
#' @name ops_stars
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' x * x
#' x / x
#' x + x
#' x + 10
#' all.equal(x * 10, 10 * x)
#' @export
Ops.stars <- function(e1, e2) {
	ret = if (missing(e2))
			lapply(e1, .Generic)
		else if (!inherits(e2, "stars"))
			lapply(e1, .Generic, e2 = e2)
		else {
			if (!all(dim(e1) == dim(e2))) {
				stopifnot(length(e2) == 1)
				lapply(e1, .Generic, e2 = as.vector(e2[[1]]))
			} else
				mapply(.Generic, e1, e2, SIMPLIFY = FALSE)
		}
	if (any(sapply(ret, function(x) is.null(dim(x))))) # happens if e1[[1]] is a factor; #304
		ret = lapply(ret, function(x) { dim(x) = dim(e1); x })
	if (! inherits(e1, "stars"))
		setNames(st_as_stars(ret, dimensions = st_dimensions(e2)), names(e2))
	else
		st_as_stars(ret, dimensions = st_dimensions(e1))
}

#' Mathematical operations for stars objects
#'
#' @param x object of class stars
#' @param ... parameters passed on to the Math functions
#' 
#' @export
#' @name ops_stars
#' 
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' a = sqrt(x)
#' b = log(x, base = 10)
#' @export
Math.stars = function(x, ...) {
	ret = lapply(x, .Generic, ...)
	st_as_stars(ret, dimensions = st_dimensions(x))
}

#' @name ops_stars
#' @export
Ops.stars_proxy <- function(e1, e2) {
	if (!inherits(e1, "stars_proxy"))
		stop("first argument in expression needs to be the stars_proxy object") # FIXME: needed?? #nocov
	if (missing(e2))
		collect(e1, match.call(), .Generic, "e1", env = environment())
	else
		collect(e1, match.call(), .Generic, c("e1", "e2"), env = environment())
}

#' @name ops_stars
#' @export
Math.stars_proxy = function(x, ...) {
	collect(x, match.call(), .Generic, env = environment())
}


#' @export
st_apply = function(X, MARGIN, FUN, ...) UseMethod("st_apply")

#' st_apply apply a function to one or more array dimensions
#' 
#' st_apply apply a function to array dimensions: aggregate over space, time, or something else
#' @name st_apply
#' @param X object of class \code{stars}
#' @param MARGIN see \link[base]{apply}; index number(s) or name(s) of the dimensions over which \code{FUN} will be applied 
#' @param FUN see \link[base]{apply} and do see below at details.
#' @param ... arguments passed on to \code{FUN}
#' @param CLUSTER cluster to use for parallel apply; see \link[parallel]{makeCluster}
#' @param PROGRESS logical; if \code{TRUE}, use \code{pbapply::pbapply} to show progress bar
#' @param FUTURE logical;if \code{TRUE}, use \code{future.apply::future_apply} 
#' @param rename logical; if \code{TRUE} and \code{X} has only one attribute and \code{FUN} is a simple function name, rename the attribute of the returned object to the function name
#' @param .fname function name for the new attribute name (if one or more dimensions are reduced) or the new dimension (if a new dimension is created); if missing, the name of \code{FUN} is used
#' @return object of class \code{stars} with accordingly reduced number of dimensions; in case \code{FUN} returns more than one value, a new dimension is created carrying the name of the function used; see the examples.
#' @details FUN is a function which either operates on a single object, which will 
#' be the data of each iteration step over dimensions MARGIN, or a function that 
#' has as many arguments as there are elements in such an object. See the NDVI 
#' examples below. Note that the second form can be very much faster e.g. when a trivial 
#' function is not being called for every pixel, but only once (example).
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' st_apply(x, 1:2, mean) # mean band value for each pixel
#' st_apply(x, c("x", "y"), mean) # equivalent to the above
#' st_apply(x, 3, mean)   # mean of all pixels for each band
#' st_apply(x, "band", mean) # equivalent to the above
#' st_apply(x, 1:2, range) # min and max band value for each pixel
#' fn_ndvi1 = function(x) (x[4]-x[3])/(x[4]+x[3]) # ONE argument: will be called for each pixel
#' fn_ndvi2 = function(red,nir) (nir-red)/(nir+red) # n arguments: will be called only once
#' ndvi1 = st_apply(x, 1:2, fn_ndvi1)
#' ndvi2 = st_apply(x[,,,3:4], 1:2, fn_ndvi2) # note that we select bands 3 and 4 in the first argument
#' all.equal(ndvi1, ndvi2)
#' # to get a progress bar also in non-interactive mode, specify:
#' if (require(pbapply)) { # install it, if FALSE
#'   pboptions(type = "timer")
#' }
#' @export
st_apply.stars = function(X, MARGIN, FUN, ..., CLUSTER = NULL, PROGRESS = FALSE, FUTURE = FALSE, 
		rename = TRUE, .fname) {
	if (missing(.fname))
		.fname <- paste(deparse(substitute(FUN), 50), collapse = "\n")
	if (is.character(MARGIN))
		MARGIN = match(MARGIN, names(dim(X)))
	dX = dim(X)[MARGIN]

	if (PROGRESS && !requireNamespace("pbapply", quietly = TRUE))
		stop("package pbapply required, please install it first")
	
	if (FUTURE && !requireNamespace("future.apply", quietly = TRUE))
	  stop("package future.apply required, please install it first")

	fn = function(y, ...) {
		ret = if (PROGRESS)
				pbapply::pbapply(X = y, MARGIN = MARGIN, FUN = FUN, ..., cl = CLUSTER)
			else {
				if (is.null(CLUSTER) && !FUTURE)
					apply(X = y, MARGIN = MARGIN, FUN = FUN, ...)
				else if (FUTURE) {
					oopts = options(future.globals.maxSize = +Inf)
					on.exit(options(oopts))
					future.apply::future_apply(y, MARGIN = MARGIN, FUN = FUN, ...)
				} else
					parallel::parApply(CLUSTER, X = y, MARGIN = MARGIN, FUN = FUN, ...)
			}
		if (is.array(ret))
			ret
		else
			array(ret, dX)
	}
	no_margin = setdiff(seq_along(dim(X)), MARGIN)
	n_args_cleaned = function(f, n) sum(!(names(as.list(args(f))) %in% c("", "...", n)))
	ret = if (n_args_cleaned(FUN, names(list(...))) == 1) # single arg, can't chunk ...
			lapply(X, fn, ...) 
		else # call FUN on full chunks:
			lapply(X, function(a) do.call(FUN, setNames(append(asplit(a, no_margin), list(...)), NULL)))
	# fix dimensions:
	dim_ret = dim(ret[[1]])
	ret = if (length(dim_ret) == length(MARGIN)) { # FUN returned a single value
			if (length(ret) == 1 && rename && make.names(.fname) == .fname)
				ret = setNames(ret, .fname)
			st_stars(ret, st_dimensions(X)[MARGIN])
		} else { # FUN returned multiple values: need to set dimension name & values
			dim_no_margin = dim(X)[-MARGIN]
			if (length(no_margin) > 1 && dim(ret[[1]])[1] == prod(dim_no_margin)) {
				r = attr(st_dimensions(X), "raster")
				new_dim = c(dim_no_margin, dim(ret[[1]])[-1])
				for (i in seq_along(ret))
					dim(ret[[i]]) = new_dim
				# set dims:
				dims = st_dimensions(X)[c(no_margin, MARGIN)]
			} else {
				orig = st_dimensions(X)[MARGIN]
				r = attr(orig, "raster")
				dims = c(structure(list(list()), names = .fname), orig)
				dims[[1]] = if (!is.null(dimnames(ret[[1]])[[1]])) # FUN returned named vector:
						create_dimension(values = dimnames(ret[[1]])[[1]])
					else
						create_dimension(to = dim_ret[1])
			}
			st_stars(ret, dimensions = create_dimensions(dims, r))
		}
	for (i in seq_along(ret))
		names(dim(ret[[i]])) = names(st_dimensions(ret))
	ret
}
