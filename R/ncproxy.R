# nc_proxy is a subclass of stars_proxy.

#' @noRd
#' @description gets the nc_request object(s) from a nc_proxy object. 
get_nc_request <- function(x) {
	out <- lapply(x, function(v) {
		attr(v, "nc_request")[[1]]
	})
	attr(out, "class") <- "nc_request"
	out
}

#' @noRd
#' @param x an object of class nc_proxy
#' @param nc_request an existing nc_request object to put into x.
#' If nc_request is not included, the nc_request object found in x is used
#' and the dimensions 
#' @description splits an nc_request object apart and adds it back to an 
#' nc_proxy object. If the nc_proxy object no longer has the variable in 
#' question that request is not added back.
put_nc_request <- function(x, nc_request = get_nc_request(x)) {
	out_class <- class(x)
	x <- unclass(x)
	
	for(s in seq_along(x)) attr(x[[s]], "nc_request") <- nc_request[s]
	
	class(x) <- out_class
	x
}

# nc_request is a class for "nc_request" attributes of an nc_proxy object.
#' @export
print.nc_request <- function(x, ...) {
	r <- x[1]
	selecter <- rep(1, length(x))
	if(length(selecter) > 1) {
		g <- 1
		for(s in 1:length(selecter)) {
			matched <- sapply(x, function(nc1, nc2) {
				identical(nc1, nc2)
			}, nc2 = x[[s]])

			if(any(!matched[1:s])) g <- g + 1
			selecter[matched] <- g
		}
	}
	for(s in unique(selecter)) {
		vars <- names(x)[selecter == s]
		ind <- which(selecter == s)[1]
		cat(paste("\nVariables:", paste(vars, collapse = ", ")))
		cat(paste("\n Size:", x[[ind]]$size, "cells"))
		cat(paste("\n Units:", as.character(attr(x[[ind]]$units, "units"))))
		cat(paste("\n Start:", paste(x[[ind]]$start, collapse = ", ")))
		cat(paste("\n Count:", paste(x[[ind]]$count, collapse = ", ")))
		cat(paste("\n Dimension Match:", paste(x[[ind]]$dimid_match, collapse = ", ")))
	}
}

#' @export
print.nc_proxy = function(x, ..., n = 1e5, nfiles = 10, simplify = TRUE) {
	cat("netcdf source stars proxy object from:\n")

	if(simplify) print(shorten_names(x[[1]], nfiles))
	nc_request <- get_nc_request(x)

	cat("\nAvailable nc variables:\n")
	cat(paste(names(nc_request),
			  sapply(nc_request,
			  	   function(x) add_units(x$units)),
			  "\n", collapse = ""))
	cat("\ndimension(s):\n")
	print(st_dimensions(x), ...)
	cat("\n")
	cat("\nnc_request:\n")
	print(nc_request)
}

# as.data.frame.stars_proxy works for nc_proxy.

#' @export
st_as_stars.nc_proxy <- function(.x, ..., downsample = 0) {
	read_ncdf(.x, downsample = downsample, ...)
}

#' @name plot
#' @param max_times integer; maximum number of time steps to attempt to plot. 
#' @export
#' @details when plotting a subsetted \code{stars_proxy} object, the default value for argument \code{downsample} will not be computed correctly, and has to be set manually.
plot.nc_proxy = function(x, y, ..., downsample = get_downsample(dim(x)), max_times = 10) {
	
	nc_request <- get_nc_request(x)
	
	nc_request <- nc_request[1]
			
	nc_request[[1]]$count[nc_request[[1]]$axis == "T"] <- 
		min(max_times, nc_request[[1]]$count[nc_request[[1]]$axis == "T"])
	
	x <- x[1]
	
	x <- put_nc_request(x, nc_request)
	
	plot(st_as_stars(x, downsample = downsample, ...), ..., downsample = 0)
}

#' @export
"[.nc_proxy" <- function(x, i = TRUE, ...) {
	
	structure(NextMethod(), class = class(x))
}

# "[[<-.nc_proxy" not needed.
	
#' @export
c.nc_proxy = function(..., along = NA_integer_, along_crs = FALSE, try_hard = FALSE, 
						 nms = names(list(...)), tolerance = sqrt(.Machine$double.eps)) {
	
	dots = list(...)
	if (!all(sapply(dots, function(x) inherits(x, "nc_proxy"))))
		stop("all arguments to c() should be nc_proxy objects")
	
	x <- dots[[1]]
	
	structure(NextMethod(), class = class(x))
}


#' @export
st_redimension.nc_proxy <- function(x, new_dims, along, ...) stop("st_redimension not support for nc_proxy")

#' @export
st_mosaic.nc_proxy = function(.x, ...) stop("st_mosaic not supported for nc_proxy")
