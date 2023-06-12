.update_dims <- function(dims, proxy_dimensions, coords, tdim) {
	uc <- coords
	for(coord in names(coords)) {
		
		pv <- st_get_dimension_values(proxy_dimensions, coord)
		
		between <- function(x, ymin, ymax, z = x) {
			z[x >= ymin & x <= ymax, drop = FALSE]
		}
		
		if(inherits(pv, "POSIXt")) {
			# Need to deal with the case where tdim isn't values but is from to.
			uc[[coord]] <- between(tdim$values, min(pv), max(pv), uc[[coord]])
		} else {
			if(attr(proxy_dimensions, "raster")$curvilinear) {
				uc[[coord]] <- seq(proxy_dimensions[[coord]]$from, 
								   proxy_dimensions[[coord]]$to)
			} else {
				uc[[coord]] <- between(uc[[coord]], min(pv), max(pv))
			}
		}
		
		dims$start[dims$name == coord] <- which(coords[[coord]] == uc[[coord]][1])[1]
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
	if (!is.null(attr(x, "call_list"))) {
		cat("call_list:\n")
		print(unlist(attr(x, "call_list")))
		cat("This object has pending lazy operations: dimensions as printed may not reflect this.\n")
	}
}

# as.data.frame.stars_proxy works for nc_proxy.

#' @export
st_as_stars.nc_proxy <- function(.x, ..., downsample = 0, envir = parent.frame()) {
	process_call_list(read_ncdf(.x, downsample = downsample, ...), 
					  attr(.x, "call_list"), envir = envir, downsample = downsample)
}

#' @name plot
#' @param max_times integer; maximum number of time steps to attempt to plot. 
#' @export
#' @details when plotting a subsetted \code{stars_proxy} object, the default value for argument \code{downsample} will not be computed correctly, and has to be set manually.
plot.nc_proxy = function(x, y, ..., downsample = get_downsample(dim(x)), max_times = 16) {
	
	if(length(x) > 1) {
		message("Plotting first variable only.")
		x <- x[1]
	}
			
	tdim <- which(sapply(st_dimensions(x), function(x) any(grepl("^POSIX|^PCIC", x$refsys))))
	
	if(length(tdim)) {
		if(length(st_get_dimension_values(x, tdim)) > max_times) {
			stop("Time dimension of nc_proxy is longer than max_times in plot.nc_proxy.")
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

#' @param .x stars object to add curvilinear coordinates too.
#' @param curvilinear only for creating curvilinear grids: named length 
#' 2 list holding longitude and latitude matrices; the names of this 
#' list should correspond to raster dimensions referred to
#' @param crs object of class \code{crs} with the coordinate reference 
#' system of the values in \code{curvilinear}; see details
#' @details if \code{curvilinear} is a \code{stars} object with longitude 
#' and latitude values, its coordinate reference system is typically not 
#' that of the latitude and longitude values.
#' @noRd
add_curvilinear <- function(.x, 
							curvilinear = NULL, 
							crs = st_crs('OGC:CRS84')) {
	# so we can just call add_curvilinear regardless
	if (is.null(curvilinear))
		.x
	else {
		stopifnot(is.list(curvilinear), 
				  names(curvilinear) %in% names(dim(.x)))
		
		if (inherits(curvilinear[[1]], "stars"))
			curvilinear[[1]] = curvilinear[[1]][[1]]
		
		if (inherits(curvilinear[[2]], "stars"))
			curvilinear[[2]] = curvilinear[[2]][[1]]
		
		dimensions = st_dimensions(.x)
		xy = names(curvilinear)
		
		dimensions[[ xy[1] ]]$values = structure(curvilinear[[1]], dim = setNames(dim(curvilinear[[1]]), xy))
		dimensions[[ xy[2] ]]$values = structure(curvilinear[[2]], dim = setNames(dim(curvilinear[[1]]), xy))
		
		# erase regular grid coefficients $offset and $delta:
		dimensions[[ xy[1] ]]$offset = dimensions[[ xy[1] ]]$delta = NA_real_
		dimensions[[ xy[2] ]]$offset = dimensions[[ xy[2] ]]$delta = NA_real_
		
		raster = get_raster(dimensions = names(curvilinear), 
							curvilinear = TRUE)
		
		st_set_crs(st_stars(.x, 
							create_dimensions(dimensions, raster),
							class = class(.x)), 
				   crs)
	}
}
