# convert arrays to data.frame, in long form
to_df = function(x) {
	as.data.frame(lapply(x, function(y) structure(y, dim = NULL)))
}

set_dim = function(x, d) {
	lapply(x, function(y, dims) structure(y, dim = dims), dims = d)
}

get_dims = function(d_cube, d_stars) {
	xy = attr(d_stars, "raster")$dimensions
	d_stars = d_stars[names(d_cube)]
	for (i in seq_along(d_cube)) {
		d_stars[[i]]$values = if (is.list(d_stars[[i]]$values))
				d_stars[[i]]$values[ d_cube[[i]] ]
			else
				d_cube[[i]]
		d_stars[[i]] = create_dimension(values = d_stars[[i]]$values, point = d_stars[[i]]$point, 
			refsys = d_stars[[i]]$refsys, is_raster = names(d_stars)[i] %in% xy)
	}
	d_stars
}

#' dplyr verbs for stars objects
#' 
#' dplyr verbs for stars objects
#' @param .data object of class \code{stars}
#' @param ... see \link[dplyr]{filter}
#' @name dplyr
filter.stars <- function(.data, ...) {
	cb = dplyr::as.tbl_cube(.data)
	cb = dplyr::filter(cb, ...)
	st_as_stars(cb$mets, dimensions = get_dims(cb$dims, st_dimensions(.data)))
}

#' @name dplyr
mutate.stars <- function(.data, ...) {
	ret = dplyr::mutate(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim(.data)), dimensions = st_dimensions(.data))
}

#' @name dplyr
select.stars <- function(.data, ...) {
    ret <- dplyr::select(to_df(.data), ...)
	st_as_stars(set_dim(ret, dim(.data)), dimensions = st_dimensions(.data))
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

#' @name dplyr
#' @param along name or index of dimension to which the slice should be applied
#' @param index integer value(s) for this index
#' @param drop logical; drop dimensions that only have a single index?
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x1 = read_stars(tif)
#' library(dplyr)
#' x1 %>% slice("band", 2:3)
#' x1 %>% slice("x", 50:100)
slice.stars <- function(.data, along, index, ..., drop = length(index) == 1) {
  #stopifnot(length(index) == 1)
    
  nd <- length(dim(.data))
  indices <- rep(list(rlang::missing_arg()), nd + 1)
  if (is.character(along))
  	along = which(along == names(st_dimensions(.data)))
  indices[[along + 1]] <- index
  indices[["drop"]] <- drop
  
  eval(rlang::expr(.data[!!!indices]))
}

#' ggplot geom for stars objects
#' 
#' ggplot geom for stars objects
#' @param mapping see \link[ggplot2]{geom_raster}
#' @param data see \link[ggplot2]{geom_raster}
#' @param ... see \link[ggplot2]{geom_raster}
#' @name geom_stars
#' @export
geom_stars = function(mapping = NULL, data = NULL, ...) {

    if (!requireNamespace("ggplot2", quietly = TRUE))
        stop("package ggplot2 required, please install it first") # nocov

	d = st_dimensions(data)
	if (has_raster(d)) {
		xy = attr(d, "raster")$dimensions
		if (is.null(mapping))
			mapping = ggplot2::aes(x = !!rlang::sym(xy[1]), y = !!rlang::sym(xy[2]),
				fill = !!rlang::sym(names(data)[1]))
		if (is_regular(d))
			ggplot2::geom_raster(mapping = mapping, data = as.data.frame(data), ...)
		else # but will this also work for rotate/shear/curvilinear?
			ggplot2::geom_tile(mapping = mapping, data = as.data.frame(data), ...)
	} else if (has_sfc(d))
		ggplot2::geom_sf(data = st_as_sf(d), ...)
	else
		stop("geom_stars only works for objects with raster or vector geometries")
}

register_all_s3_methods = function() {
	register_s3_method("dplyr", "filter", "stars") # nocov start
	register_s3_method("dplyr", "select", "stars")
	register_s3_method("dplyr", "mutate", "stars")
	register_s3_method("dplyr", "pull", "stars")
	register_s3_method("dplyr", "as.tbl_cube", "stars")
	register_s3_method("dplyr", "slice", "stars")
	register_s3_method("xts", "as.xts", "stars") # nocov end
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
#nocov end
