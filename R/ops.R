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
	#ret = if (is.array(e2))
	ret = if (!inherits(e2, "stars"))
			lapply(e1, .Generic, e2 = e2)
		else
			mapply(.Generic, e1, e2, SIMPLIFY = FALSE)
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
	collect(e1, match.call(), .Generic, "e1")
}

#' @name ops_stars
#' @export
Math.stars_proxy = function(x, ...) {
	collect(x, match.call(), .Generic)
}


#' @export
st_apply = function(X, MARGIN, FUN, ...) UseMethod("st_apply")

#' st_apply apply a function to one or more array dimensions
#' 
#' st_apply apply a function to array dimensions: aggregate over space, time, or something else
#' @name st_apply
#' @param X object of class \code{stars}
#' @param MARGIN see \link[base]{apply}; index number(s) or name(s) of the dimensions over which \code{FUN} will be applied 
#' @param FUN see \link[base]{apply}
#' @param ... arguments passed on to \code{FUN}
#' @param CLUSTER cluster to use for parallel apply; see \link[parallel]{makeCluster}
#' @param PROGRESS logical; if \code{TRUE}, use \code{pbapply::pbapply} to show progress bar
#' @param FUTURE logical;if \code{TRUE}, use \code{future.apply::future_apply} 
#' @return object of class \code{stars} with accordingly reduced number of dimensions; in case \code{FUN} returns more than one value, a new dimension is created carrying the name of the function used; see the examples.
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' st_apply(x, 1:2, mean) # mean band value for each pixel
#' st_apply(x, c("x", "y"), mean) # equivalent to the above
#' st_apply(x, 3, mean)   # mean of all pixels for each band
#' st_apply(x, "band", mean) # equivalent to the above
#' st_apply(x, 1:2, range) # min and max band value for each pixel
#' @export
st_apply.stars = function(X, MARGIN, FUN, ..., CLUSTER = NULL, PROGRESS = FALSE, FUTURE = FALSE) {
  
  fname <- paste(deparse(substitute(FUN), 50), collapse = "\n")
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
      if (is.null(CLUSTER) & !FUTURE)
        apply(X = y, MARGIN = MARGIN, FUN = FUN, ...)
      else if (FUTURE){
        oopts = options(future.globals.maxSize = +Inf)
        on.exit(options(oopts))  
        future_apply(y, MARGIN, FUN, ...)
      }
      else
        parallel::parApply(CLUSTER, X = y, MARGIN = MARGIN, FUN = FUN, ...)
    }
    if (is.array(ret))
      ret
    else
      array(ret, dX)
  }
	ret = lapply(X, fn, ...) 
	dim_ret = dim(ret[[1]])
	if (length(dim_ret) == length(MARGIN)) # FUN returned a single value
		st_stars(ret, st_dimensions(X)[MARGIN])
	else { # FUN returned multiple values:
		orig = st_dimensions(X)[MARGIN]
		r = attr(orig, "raster")
		dims = c(structure(list(list()), names = fname), orig)
		dims[[1]] = if (!is.null(dimnames(ret[[1]])[[1]])) # FUN returned named vector:
				create_dimension(values = dimnames(ret[[1]])[[1]])
			else
				create_dimension(to = dim_ret[1])
		st_stars(ret, dimensions = create_dimensions(dims, r))
	}
}
