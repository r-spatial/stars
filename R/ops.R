#' S3 Ops Group Generic Functions for stars objects
#'
#' Ops functions for stars objects, including comparison, product and divide, add, subtract
#'
#' @param e1 object of class \code{stars}
#' @param e2 object of class \code{stars}
#'
#' @return object of class \code{stars}
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' x * x
#' x / x
#' x + x
#' x + 10
#' @export
Ops.stars <- function(e1, e2) {
	ret = if (inherits(e2, "stars"))
		mapply(.Generic, e1, e2, SIMPLIFY = FALSE)
  	else
  		lapply(e1, .Generic, e2 = e2)
	st_as_stars(ret, dimensions = st_dimensions(e1))
}

#' Mathematical operations for stars objects
#'
#' @param x object of class stars
#' @param ... parameters passed on to the Math functions
#' 
#' @export
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

#' st_apply apply a function to one or more array dimensions
#' 
#' st_apply apply a function to array dimensions: aggregate over space, time, or something else
#' @param X object of class \code{stars}
#' @param MARGIN see \link[base]{apply}; if \code{MARGIN} is a character vector, 
#' @param FUN see \link[base]{apply}
#' @param ... arguments passed on to \code{FUN}
#' @return object of class \code{stars} with accordingly reduced number of dimensions; in case \code{FUN} returns more than one value, a new dimension is created carrying the name of the function used; see the examples.
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' st_apply(x, 1:2, mean) # mean band value for each pixel
#' st_apply(x, 3, mean)   # mean of all pixels for each band
#' st_apply(x, 1:2, range) # min and max band value for each pixel
#' @export
st_apply = function(X, MARGIN, FUN, ...) {
	fname <- paste(deparse(substitute(FUN), 50), collapse = "\n")
	if (is.character(MARGIN))
		MARGIN = match(MARGIN, names(dim(X)))
	dX = dim(X)[MARGIN]
	fn = function(y, ...) {
		ret = apply(y, MARGIN, FUN, ...)
		if (is.array(ret))
			ret
		else
			array(ret, dX)
	}
	ret = lapply(X, fn, ...) 
	dim_ret = dim(ret[[1]])
	if (length(dim_ret) == length(MARGIN)) # FUN returned a single value
		st_as_stars(ret, dimensions = st_dimensions(X)[MARGIN])
	else { # FUN returned multiple values:
		orig = st_dimensions(X)[MARGIN]
		dims = c(structure(list(list()), names = fname), orig)
		dims[[1]] = if (!is.null(dimnames(ret[[1]])[[1]])) # FUN returned named vector:
				create_dimension(values = dimnames(ret[[1]])[[1]])
			else
				create_dimension(to = dim_ret[1])
		ret = st_as_stars(ret, dimensions = structure(dims, class = "dimensions")) # FIXME: better to use constructor?
		if (all(c("x", "y") %in% names(dims)))
			ret = aperm(ret, c("x", "y", setdiff(names(dims), c("x", "y"))))
		ret
	}
}
