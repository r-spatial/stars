# convert arrays to data.frame, in long form
to_df = function(x) {
	as.data.frame(lapply(x, c))
}

set_dim = function(x, d) {
	f = function(y, d) { dim(y) = d; y }
	lapply(x, f, d = d)
}

get_dims = function(d_cube, d_stars) {
	d_stars = d_stars[names(d_cube)]
	for (i in seq_along(d_cube))
		if (is.list(d_stars[[i]]$values))
			d_stars[[i]]$values = d_stars[[i]]$values[ d_cube[[i]] ]
	d_stars
}

#' dplyr verbs for stars objects
#' 
#' dplyr verbs for stars objects
#' @param .data see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @name dplyr
#' @export
filter.stars <- function(.data, ...) {
	cb = dplyr::as.tbl_cube(.data)
	cb = dplyr::filter(cb, ...)
	st_as_stars(cb$mets, dimensions = get_dims(cb$dims, st_dimensions(.data)))
}

#' @name dplyr
#' @export
mutate.stars <- function(.data, ...) {
	d = st_dimensions(.data)
	dim_orig = dim(.data)
	ret = dplyr::mutate(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim_orig), dimensions = d)
}

#' @name dplyr
#' @export
select.stars <- function(.data, ...) {
	d = st_dimensions(.data)
	dim_orig = dim(.data)
    ret <- dplyr::select(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim_orig), dimensions = d)
}

#' @param var see \link[dplyr]{pull}
#' @name dplyr
#' @export
pull.stars = function (.data, var = -1) 
{
	var = rlang::enquo(var)
	dplyr::pull(to_df(.data), !!var)
}

#' @name dplyr
#' @param x object of class \code{stars}
#' @export
as.tbl_cube.stars = function(x, ...) {
	cleanup = function(y) {
		if (is.list(y))
			seq_along(y)
		else
			y
	}
	dims = lapply(expand_dimensions(x), cleanup)
	dplyr::tbl_cube(dims, c(unclass(x)))
}

# example from HW's advanced R:
#slice <- function(x, along, index) {
#  #stopifnot(length(index) == 1)
#    
#  nd <- length(dim(x))
#  indices <- rep(list(missing_arg()), nd)
#  indices[[along]] <- index
#  
#  expr(x[!!!indices])
#}
#
#eval(slice(x, 1, 1:3))
