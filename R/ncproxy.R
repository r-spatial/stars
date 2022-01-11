.update_dims <- function(dims, proxy_dimensions, coords, tdim) {
	uc <- coords
	for(coord in names(coords)) {
		
		pv <- st_get_dimension_values(proxy_dimensions, coord)
		
		between <- function(x, ymin, ymax, z = x) {
			z[x >= ymin & x <= ymax, drop = FALSE]
		}
		
		if(inherits(pv, "POSIXt")) {
			uc[[coord]] <- between(tdim$values, min(pv), max(pv), uc[[coord]])
		} else {
			uc[[coord]] <- between(uc[[coord]], min(pv), max(pv))
		}
		
		dims$start[dims$name == coord] <- which(coords[[coord]] == uc[[coord]][1])
		dims$count[dims$name == coord] <- dims$length[dims$name == coord] <- length(uc[[coord]])
		
	}
	dims
}

#' @export
print.nc_proxy = function(x, ..., n = 1e5, nfiles = 10, simplify = TRUE) {
	cat("netcdf source stars proxy object from:\n")

	if(simplify) print(shorten_names(x[[1]], nfiles))

	cat("\nAvailable nc variables:\n")
	cat(paste(names(x), collapse = "\n"))
	cat("\n\ndimension(s):\n")
	print(st_dimensions(x), ...)
	cat("\n")
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
	
	x <- x[1]
			
	tdim <- which(sapply(st_dimensions(x), function(x) any(grepl("^POSIX|^PCIC", x$refsys))))
	
	if(length(tdim)) {
		keep <- 1:min(max_times, length(tvals <- st_get_dimension_values(x, tdim)))
		if(tdim == 1) {
			x <- x[,keep] # T
		} else if(tdim == 2) {
			x <- x[,,keep] # XT
		} else if(tdim == 3) {
			x <- x[,,,keep] # XYT
		} else {
			x <- x[,,,,keep] # XYZT
		}
	}
	
	plot(st_as_stars(x, downsample = downsample, ...), ..., downsample = 0)
}

#' @export
"[.nc_proxy" <- function(x, i = TRUE, ...) {
	# stars_proxy works but need to match class.
	structure(NextMethod(), class = class(x))
}

#' @export
st_crop.nc_proxy <- function(x, y, ..., 
							 crop = TRUE, 
							 epsilon = sqrt(.Machine$double.eps), 
							 collect = TRUE) {
	# will this work? Couild be slick if it does.
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
