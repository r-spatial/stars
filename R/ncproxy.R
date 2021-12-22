# nc_proxy is a subclass of stars_proxy.

# nc_request is a class for "nc_request" attributes of an nc_proxy object.
#' @export
print.nc_request <- function(x) {
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
	nc_request <- attr(nc, "nc_request")

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

#' #' @export
#' st_as_stars.nc_proxy = function(x, ...) {
#'
#' }
