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
	for (i in seq_along(d_cube)) {
		d_stars[[i]]$values = if (is.list(d_stars[[i]]$values))
				d_stars[[i]]$values[ d_cube[[i]] ]
			else
				d_cube[[i]]
		d_stars[[i]] = create_dimension(values = d_stars[[i]]$values, point = d_stars[[i]]$point, 
			refsys = d_stars[[i]]$refsys)
	}
	d_stars
}

#' dplyr verbs for stars objects
#' 
#' dplyr verbs for stars objects
#' @param .data see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @name dplyr
filter.stars <- function(.data, ...) {
	cb = dplyr::as.tbl_cube(.data)
	cb = dplyr::filter(cb, ...)
	st_as_stars(cb$mets, dimensions = get_dims(cb$dims, st_dimensions(.data)))
}

#' @name dplyr
mutate.stars <- function(.data, ...) {
	d = st_dimensions(.data)
	dim_orig = dim(.data)
	ret = dplyr::mutate(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim_orig), dimensions = d)
}

#' @name dplyr
select.stars <- function(.data, ...) {
	d = st_dimensions(.data)
	dim_orig = dim(.data)
    ret <- dplyr::select(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim_orig), dimensions = d)
}

#' @param var see \link[dplyr]{pull}
#' @name dplyr
pull.stars = function (.data, var = -1) {
	var = rlang::enquo(var)
	dplyr::pull(to_df(.data), !!var)
}

#' @name dplyr
#' @param x object of class \code{stars}
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

register_all_s3_methods = function() {
	register_s3_method("dplyr", "filter", "stars")
	register_s3_method("dplyr", "select", "stars")
	register_s3_method("dplyr", "mutate", "stars")
	register_s3_method("dplyr", "pull", "stars")
	register_s3_method("dplyr", "as.tbl_cube", "stars")
}

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
#nocov start
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
