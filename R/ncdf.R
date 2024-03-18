
#' Read NetCDF into stars object
#'
#' Read data from a file (or source) using the NetCDF library directly.
#'
#' The following logic is applied to coordinates. If any coordinate axes have
#' regularly spaced coordinate variables they are reduced to the
#' offset/delta form with 'affine = c(0, 0)', otherwise the values of the coordinates
#' are stored and used to define a rectilinear grid.
#'
#' If the data has two or more dimensions and the first two are regular
#' they are nominated as the 'raster' for plotting.
#'
#' If the \code{curvilinear} argument is used it specifies the 2D arrays
#' containing coordinate values for the first two dimensions of the data read. It is currently
#' assumed that the coordinates are 2D and that they relate to the first two dimensions in
#' that order.
#' @examples
#' f <- system.file("nc/reduced.nc", package = "stars")
#' if (require(ncmeta, quietly = TRUE)) {
#'  read_ncdf(f)
#'  read_ncdf(f, var = c("anom"))
#'  read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), count = c(10, 12, 1, 1)))
#' }
#'
#' @param .x NetCDF file or source as a character vector or an nc_proxy object.
#' @param ... ignored
#' @param var variable name or names (they must be on matching grids)
#' @param ncsub matrix of start, count columns (see Details)
#' @param curvilinear length two character named vector with names of variables holding
#' longitude and latitude values for all raster cells. `stars` attempts to figure out appropriate
#' curvilinear coordinates if they are not supplied.
#' @param eps numeric; dimension value increases are considered identical when they differ less than \code{eps}
#' @param ignore_bounds logical; should bounds values for dimensions, if present, be ignored?
#' @param make_time if \code{TRUE} (the default), an attempt is made to provide a date-time class from the "time" variable
#' @param make_units if \code{TRUE} (the default), an attempt is made to set the units property of each variable
#' @param proxy logical; if \code{TRUE}, an object of class \code{stars_proxy} is read which contains array
#' metadata only; if \code{FALSE} the full array data is read in memory. If not set, defaults to \code{TRUE}
#' when the number of cells to be read is larger than \code{options(stars.n_proxy)}, or to 1e8 if that option was not set.
#' @param downsample integer; number of cells to omit between samples along each dimension. 
#' e.g. \code{c(1,1,2)} would return every other cell in x and y and every third cell
#' in the third dimension (z or t). If 0, no downsampling is applied. Note that this transformation
#' is applied AFTER NetCDF data are read using st_downsample. As such, if proxy=TRUE, this 
#' option is ignored.
#' @details
#' If \code{var} is not set the first set of variables on a shared grid is used.
#'
#' \code{start} and \code{count} columns of ncsub must correspond to the variable dimension (nrows)
#' and be valid index using \code{\link[RNetCDF]{var.get.nc}} convention (start is 1-based). If the count value
#' is \code{NA} then all steps are included. Axis order must match that of the variable/s being read.
#' @export
#' @examples
#' if (require(ncmeta, quietly = TRUE)) {
#'  #' precipitation data in a curvilinear NetCDF
#'  prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#'  prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE)
#' }
#'
#' ##plot(prec) ## gives error about unique breaks
#' ## remove NAs, zeros, and give a large number
#' ## of breaks (used for validating in detail)
#' qu_0_omit = function(x, ..., n = 22) {
#'   x = units::drop_units(na.omit(x))
#'   c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
#' }
#' if (require(dplyr, quietly = TRUE)) {
#'   prec_slice = slice(prec, index = 17, along = "time")
#'   plot(prec_slice, border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = FALSE)
#'   nc = sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")
#'   plot(st_geometry(nc), add = TRUE, reset = FALSE, col = NA)
#' }
read_ncdf = function(.x, ..., var = NULL, ncsub = NULL, curvilinear = character(0),
    eps = sqrt(.Machine$double.eps), ignore_bounds = FALSE, make_time = TRUE, make_units = TRUE,
    proxy = NULL, downsample = 0) {

  if (!requireNamespace("ncmeta", quietly = TRUE))
    stop("package ncmeta required, please install it first") # nocov
  if (!requireNamespace("RNetCDF", quietly = TRUE))
    stop("package RNetCDF required, please install it first") # nocov

  if(inherits(.x, "nc_proxy")) {
  	# grab the source file and all variables needed.
    x <- as.character(.x[[1]]) # FIXME: only supports one data source.
    var <- names(.x)
    proxy_dimensions <- st_dimensions(.x)
    nc_prj <- sf::st_crs(.x)
    
    if(!is.null(ncsub)) {
    	warning("ncsub ignored when .x is class nc_proxy")
    	ncsub <- NULL
    }
    
  } else {
    x <- .x
    proxy_dimensions <- NULL
  }
	
  # Get all the nc metadata
  meta <- .fix_meta(ncmeta::nc_meta(x))

  if(.is_netcdf_cf_dsg(meta)) {
    if (!requireNamespace("ncdfgeom", quietly = TRUE))
      stop("package ncdfgeom required, please install it first") # nocov

    if(!is.null(proxy) && proxy) warning("proxy behavior not supported for timeseries netcdf")

    geom <- .get_geom_name(meta)

    return(st_as_stars(ncdfgeom::read_timeseries_dsg(x), sf_geometry = geom))
  }

  # Get relevant variables
  var <- .get_vars(var, meta)
  rep_var <- var[1L]

  # Get coordinate variable info
  all_coord_var <- ncmeta::nc_coord_var(x)

  if(ncol(all_coord_var) == 0) all_coord_var <- data.frame(variable = NA, X = NA, Y = NA,
                                                           Z = NA, T = NA, bounds = NA)

  coord_var <- .clean_coord_var(all_coord_var, rep_var, meta, curvilinear)

  canon_order <- c("X", "Y", "Z", "T")[!is.na(coord_var[c("X", "Y", "Z", "T")])]
  
  curvilinear <- if(coord_var$curvilinear) {
    c(X = coord_var$X, Y = coord_var$Y) } else character(0)

  # Get dimensions for representative var in correct axis order.
  dims <- .get_dims(meta, rep_var, coord_var, meta$axis, canon_order)

  # Validate that ncsub matches dims
  dims <- .add_ncsub(dims, ncsub)

  nc <- RNetCDF::open.nc(x)
  on.exit(RNetCDF::close.nc(nc), add = TRUE)

  # Get coordinates from netcdf or create them
  coords <- .get_coords(nc, dims)
  coords <- .clean_coords(coords, coord_var, meta$attribute, eps)

  # Figure out if we have a raster or not
  raster <- .get_nc_raster(coords)

  # Get matcher for netcdf requests
  dimid_matcher <- .get_dimid_matcher(nc, coord_var, var)
  
  if(!is.null(proxy_dimensions)) {
  	tdim <- if("T" %in% dims$axis) {
  		
  		tmeta <- .get_time_meta(coord_var, rep_var, meta)
  		
  		tvals <- coords[[which(dims$axis == "T")]]
  		
  		tdim <- create_dimension(from = 1, to = length(tvals))
  		
  		tdim$values <- tvals
  		
  		make_cal_time2(tdim,
  					   time_unit = tmeta$tunit, 
  					   cal = tmeta$calendar)
  		
  	} else NULL
  	
  	dims <- .update_dims(dims, proxy_dimensions, coords, tdim)
  	
  	coords <- .get_coords(nc, dims)
  	coords <- .clean_coords(coords, coord_var, meta$attributes, eps)
  }
  
  pull <- .should_pull(proxy, 
  					   array_size = prod(dims[, "count", drop = TRUE]),
  					   num_vars = length(var))
  
  out_data <- if(pull) {
  	# Get all the data from the nc file
  	.set_nc_units(.get_data(nc, var, dims, dimid_matcher, pull = pull),
  				  meta$attribute, make_units)
  } else {
  	# Just return the source file for each variable.
  	setNames(as.list(rep(x, length(var))), var)
  }
  
  # Create stars dimensions object

  if(is.null(nc_dim <- dim(out_data[[1]]))) nc_dim <- dims$length
  
  dimensions <- create_dimensions(setNames(nc_dim, dims$name),
  								  raster)
  	
  dimensions <- .get_nc_dimensions(dimensions,
                                   coord_var = all_coord_var,
                                   coords = coords,
                                   nc = nc,
                                   dims = dims,
                                   var_names = meta$variable$name,
                                   curvilinear,
                                   eps = eps,
                                   ignore_bounds = ignore_bounds,
                                   atts = meta$attribute)
  dimensions <- .get_nc_time(dimensions, make_time,
                            coord_var, rep_var, meta)

  if (is.character(out_data[[1]])) {
    
    # this is a proxy
    out_data <- st_stars_proxy(out_data, dimensions, NA_value = NA_real_, resolutions = NULL)

    class(out_data) <- c("nc_proxy", "stars_proxy", "stars")

  } else {
    # Make initial response data
    out_data <- st_stars(out_data, dimensions)
  }

  if(is.null(proxy_dimensions)) {
  	if(length(curvilinear) == 2) {
  		cv <- curvilinear[1]
  	} else if (length(attr(coords, "cv")) > 0) {
  		cv <- attr(coords, "cv")[1]
  	} else {
  		cv <- all_coord_var[all_coord_var$variable == rep_var, ]$X
  	}
  	
  	nc_prj <- .get_nc_projection(meta$attribute, rep_var, cv)
  }
  
  st_crs(out_data) <- nc_prj

  # Add curvilinear and return
  if (length(curvilinear) == 2) {
    curvi_coords = .get_curvilinear_coords(curvilinear, dimensions, nc, dims)
    out_data <- add_curvilinear(out_data, curvilinear = curvi_coords, nc_prj)
  }
  
  if(!all(downsample == 0)) {
    out_data <- st_downsample(out_data, downsample)
  }
  
  out_data
  
}

.fix_meta <- function(meta) {
  nas <- is.na(meta$axis$dimension)
  if (any(nas)) meta$axis$dimension[nas] <- -1 # nocov

  if(!is.null(names(meta$attribute))) {
    names(meta$attribute)[names(meta$attribute) == "attribute"] <- "name" # future proofing.
  }

  meta
}

.is_regular <- function(coords_list, eps) {
  sapply(coords_list, function(x) regular_intervals(x, epsilon = eps))
}

.unique_fuzz <- function(x, eps) {
  u = unique(x)
  if (all(diff(sort(u)) < eps))
    mean(x) # rather than mean(u)
  else
    u
}

.as_rectilinear = function(d) {
    ed = expand_dimensions(d, center = FALSE)
    for (i in attr(d, "raster")$dimensions) {
        if (!is.na(d[[ i ]]$offset)) {
            d[[i]]$values = as_intervals(ed[[i]], add_last = TRUE)
            d[[i]]$offset = d[[i]]$delta = NA
        }
    }
    d
}

.should_pull <- function(proxy, array_size, num_vars,
                         n_proxy = options("stars.n_proxy")[[1]] %||% 1.e8) {
  if(is.null(proxy)) {
    if(array_size > n_proxy) {
      pull <- FALSE
      message("Large netcdf source found, returning proxy object.")
    } else {
      pull <- TRUE
      message(paste("Will return stars object with", array_size, "cells."))
    }
  } else {
    pull <- !proxy
  }

  if(pull & array_size > n_proxy)
    warning("Large netcdf source will be requested. Consider using stars proxy.")

  pull
}

.get_vars <- function(var, meta) {

  if (is.null(var)) {
    ix <- 1
    if (meta$grid$grid[ix] == "S") {
      ix <- which(!meta$grid$grid == "S")[1L] # nocov

      if (length(ix) < 1)  stop("only scalar variables found, not yet supported") # nocov
    }
    grd = meta$grid$grid[which.max(nchar(meta$grid$grid))]
    var = meta$grid$variables[[match(grd, meta$grid$grid)]]$variable

    message(sprintf("no 'var' specified, using %s", paste(var, collapse = ", ")))
    other_vars <- setdiff(meta$variable$name, var)
    if (length(other_vars) > 0)
      message(sprintf("other available variables:\n %s", paste(other_vars, collapse = ", ")))
  }
  return(var)
}

.get_grid_mapping <- function(atts, rep_var) {
	if(!is.null(atts)) {
		suppressWarnings(ncmeta::nc_grid_mapping_atts(atts, rep_var))
	} else {
		list()
	}
}

.get_nc_projection <- function(atts, rep_var, rep_coord_var) {

  nc_grid_mapping <- .get_grid_mapping(atts, rep_var)
  
  if(length(nc_grid_mapping) == 0) {
    
    if(.is_degrees(atts, rep_coord_var)) {
      message(paste("No projection information found in nc file. \n",
                    "Coordinate variable units found to be degrees, \n",
                    "assuming WGS84 Lat/Lon."))
      st_crs('OGC:CRS84')
    } else {
      warning("No projection information found in nc file.")
      st_crs(NULL)
    }
  } else {
  	tryCatch({
  		cv_units <- .get_gm_units(atts, rep_coord_var)
  	
  		base_gm <- ncmeta::nc_gm_to_prj(nc_grid_mapping)
  	
  		base_gm <- paste0(gsub("\\+units=m ", "", base_gm), 
  						  " +units=", cv_units)
  	
    	st_crs(base_gm)
  	}, error = function(e) {
  		warning(paste0("failed to create crs based on grid mapping\n",
  					  "and coordinate variable units. Will return NULL crs.\n",
  					  "Original error: \n", e))
  		st_crs(NULL)
  	})
  }
}

.is_degrees <- function(atts, var) {
  units <- .get_attributes(atts, "units", var)$value[[1]]
  !is.null(units) && grepl("degrees", units, ignore.case = TRUE)
}

.get_gm_units <- function(atts, var) {
	if(.is_degrees(atts, var)) {
		"degrees"
	} else {
		.get_attributes(atts, "units", var)$value[[1]]	
	}
}

.clean_coord_var <- function(c_v, var, meta, curvilinear) {
  c_v <- c_v[c_v$variable == var, ]

  if(nrow(c_v) == 0) c_v[1, ] <- NA

  check_curvi <- .check_curvilinear(c_v, var, meta$variable, curvilinear)

  if(length(curvilinear)) {
  	c_v$X <- rep(check_curvi[1], nrow(c_v))
  	c_v$Y <- rep(check_curvi[2], nrow(c_v))
  }

  not_na <- apply(c_v, 1, function(x) sum(!is.na(x)))
  
  c_v <- c_v[which(not_na == max(not_na)), ][1, ]
  
  has_prj <- length(.get_grid_mapping(meta$attribute, var)) != 0
  
  if(length(curvilinear) > 0) {
    if(length(check_curvi) == 0) stop("Curvilinear coordinate variables provided by not found in file.")
    c_v <- c_v[c_v$variable == var & c_v$X == check_curvi[1], ]
    c_v$curvilinear <- TRUE
  } else if(nrow(c_v) > 1 | length(check_curvi) == 2) {
    # Should use convenience function?
    sn_atts <- meta$attribute[meta$attribute$name == "standard_name", ]
    auxiliary_x <- sn_atts[grepl("projection_x_coordinate", sn_atts$value), ]$variable
    auxiliary_y <- sn_atts[grepl("projection_y_coordinate", sn_atts$value), ]$variable

    if(length(auxiliary_x) > 0 && length(auxiliary_y) > 0 &&
       auxiliary_x %in% c_v$X && auxiliary_y %in% c_v$Y &
       has_prj) {
      c_v <- c_v[c_v$variable == var & c_v$X == auxiliary_x, ]
      c_v$curvilinear <- FALSE
    } else if (length(check_curvi) == 2) {
      c_v <- c_v[c_v$variable == var & c_v$X == check_curvi[1], ]
      c_v$curvilinear <- TRUE
    } else{
      c_v <- c_v[1, ]
      warning(paste("Found two coordinate variable pairs. Chosing:",
                    paste(as.character(c_v)[2:5], collapse = " "),
                    "for", as.character(c_v)[1])) #nocov
    }
  } else {
    c_v$curvilinear <- FALSE
  }
  return(c_v)
}

.check_curvilinear <- function(coord_var, var, variables, curvilinear) {
  if(length(curvilinear) > 0) {
    if(!all(curvilinear %in% variables$name)) stop("Curvilinear variables not found in file.")
    if(!length(curvilinear) == 2 | all(!is.character(curvilinear))) stop("Curvilinear input must be a length two character vector.")
    if(all(variables$ndims[match(curvilinear, variables$name)] == 2)) {
      xy_coords <- coord_var[c("X", "Y")]
      if(!curvilinear[1] %in% c(xy_coords$X, xy_coords$Y) | !curvilinear[2] %in% c(xy_coords$X, xy_coords$Y)) {
        stop("Specified curvilinear coordinate variables not found as X/Y coordinate variables.")
      } else {
        if(!curvilinear[1] %in% xy_coords$X) {
          curvilinear <- curvilinear[2:1]
        }
        return(stats::setNames(curvilinear, c("X", "Y")))
      }
    } else {
      stop("Specified curvilinear coordinates are not 2-dimensional.")
    }
  }
  # If we have X and Y coordvars to look at.
  if (all(c("X", "Y") %in% names(coord_var))) {

    # Actually look at this one.
    XY_curvi = unlist(coord_var[coord_var$variable == var, ][c("X", "Y")])

    if (all(!is.na(XY_curvi)) && # If all coordinate variables have 2 dims.
        all(variables$ndims[match(XY_curvi, variables$name)] == 2)) {
      return(XY_curvi)
    } else {
      return(character(0))
    }
  } else {
    return(character(0))
  }
}

#' Gets dimension info ensuring they are in an order suited to coordinate
#' variables that need to be subset appropriately.
#' @noRd
.get_dims <- function(meta, var, c_v, axis, canon_order) {
  # This matches the axes of a given variable to the correct dimension
  dims <-meta$dimension[match(meta$axis$dimension[meta$axis$variable == var],
                              meta$dimension$id), ]

  dims$coord_var <- ""
  dims$axis <- ""

  rep_var_coordinates <- .get_attributes(meta$attribute, "coordinates", var)
  if(!is.null(rep_var_coordinates)) {
    rep_var_coordinates <- strsplit(rep_var_coordinates$value[[1]], " ")[[1]]
  }

  x_cv <- axis[axis$variable == c_v$X, ]
  y_cv <- axis[axis$variable == c_v$Y, ]

  if(nrow(x_cv) == 1 & nrow(y_cv) == 1) {
    dims[dims$id == x_cv$dimension, ][c("coord_var", "axis")] <- list(x_cv$variable, "X")
    dims[dims$id == y_cv$dimension, ][c("coord_var", "axis")] <- list(y_cv$variable, "Y")
  }

  dim_matcher <- axis[axis$variable == c_v$X, ]$dimension

  if(length(dim_matcher) < 2) {
    dim_matcher <- c(dim_matcher,
                     axis[axis$variable == c_v$Y, ]$dimension)
  }

  z_axis <- integer(0)
  if(!is.na(c_v$Z)) {
    z_cv <- axis[axis$variable == c_v$Z, ]
    
    z_axis <- z_cv$dimension
    
    dims[dims$id == z_cv$dimension, ][c("coord_var", "axis")] <- list(z_cv$variable, "Z")
    
    dim_matcher <- c(dim_matcher, z_axis)
    
  } else if(nrow(dims) == 4 & !c_v$curvilinear) {
    z_axis <- unique(axis[!axis$variable %in% c(c_v$X, c_v$Y, c_v$T) &
                            !axis$dimension %in% dim_matcher, ]$dimension)
  }

  if(!is.na(c_v$T)) {
    t_cv <- axis[axis$variable == c_v$T, ]
    
    t_axis <- t_cv$dimension
    
    dims[dims$id == t_cv$dimension, ][c("coord_var", "axis")] <- list(t_cv$variable, "T")
    
    if(length(z_axis) > 1) {
      z_axis <- z_axis[z_axis != t_axis]
    }
    dim_matcher <- c(dim_matcher, z_axis, t_axis)
  }

  if(all(!is.na(dim_matcher))) {
    if(length(dim_matcher) != length(dims$id)) {
      dim_matcher <- c(dim_matcher,
                       dims$id[!dims$id %in% dim_matcher])
      dim_matcher <- unique(dim_matcher)
    }
    dims <- dims[match(dims$id, dim_matcher), ]
  }
  return(dims)
}

#' Gets a list of dimension id matching indexes for use in
#' requesting data from a given NetCDF file.
#' @noRd
.get_dimid_matcher <- function(nc, coord_var, var) {
  setNames(lapply(var, FUN = function(.v, c_v, nc) {

    matcher <- NULL

    if(all(!is.na(c_v[2:3]))) {
      coordvar_dimids <- RNetCDF::var.inq.nc(nc, c_v$X)$dimids

      if(length(coordvar_dimids) == 1) {
        coordvar_dimids <- c(coordvar_dimids,
                             RNetCDF::var.inq.nc(nc, c_v$Y)$dimids)
      }

      if(!is.na(c_v$Z)) {
        coordvar_dimids <- c(coordvar_dimids,
                             RNetCDF::var.inq.nc(nc, c_v$Z)$dimids)
      }

      if(!is.na(c_v$T)) {
        coordvar_dimids <- c(coordvar_dimids,
                             RNetCDF::var.inq.nc(nc, c_v$T)$dimids)
      }

      var_dimids <- RNetCDF::var.inq.nc(nc, .v)$dimids

      matcher <- match(coordvar_dimids, var_dimids)
      
      if(!all(diff(matcher[1:2]) == 1)) {
        warning("Non-canonical axis order found, attempting to correct.")
      }
    }
    matcher
  }, c_v = coord_var, nc = nc),
  var)
}

.get_nc_var <- function(nc, .v, start, count) {
  RNetCDF::var.get.nc(nc,
                      variable = .v,
                      start = start,
                      count = count,
                      collapse = FALSE, ## keep 1-dims
                      unpack = TRUE,     ## offset and scale applied internally
                      rawchar = TRUE)  ## needed for NC_CHAR, as per
}

.get_data <- function(nc, var, dims, dimid_matcher, pull = pull) {
  out_data <- lapply(var, pull = pull, FUN = function(.v, pull) {

    dm <- match(RNetCDF::var.inq.nc(nc, .v)$dimids,
                dims[, "id", drop = TRUE])
    if(is.null(dm)) dm <- c(1:nrow(dims))

    request <- list(start = dims[, "start", drop = TRUE][dm],
                    count = dims[, "count", drop = TRUE][dm])

    request$size <- prod(request$count)

    request$dimid_match <- dm
    
    request$axis <- dims$axis[dm]

    if(pull) {

      ret <- .get_nc_var(nc, .v, request$start, request$count)

      if(length(dm) > 1 && !all(diff(dm[1:2]) == 1)) {
        ret <- aperm(ret, dm)
      }

    }
    
    return(ret)

  })
  
  ## "../rasterwise/extdata/R13352.nc"
  ## https://github.com/hypertidy/tidync/issues/75
  ## check for NC_CHAR case
  setNames(lapply(out_data, function(.v) {
  	if (mode(.v) == "raw") {
  		array(unlist(lapply(.v, rawToChar)), dims$length)
  	} else .v
  }), var)
}

.get_attributes <- function(attribute, att, var = NULL) {
  if(!is.null(var)) attribute <- attribute[attribute$variable == var, ]
  if(att %in% attribute$name)
    attribute[attribute$name == att, ]
  else {
    NULL
  }
}

.add_ncsub <- function(dims, ncsub, proxy_dimensions) {

  if (is.null(ncsub)) {
    dims$start <- 1
    dims$count <- dims$length
  } else {
    if (nrow(dims) != nrow(ncsub))
      stop("input ncsub doesn't match available dims")
    ix <- is.na(ncsub[, "count"])
    if (any(ix))  ncsub[ix, "count"] <- dims$length[ix] - ncsub[ix, "start"] + 1
    if (any(ncsub[, "start"] < 1) ||
        any((ncsub[, "count"] - ncsub[, "start"] + 1) > dims$length))
      stop("start or count out of bounds")
    dims$start <- ncsub[, "start"]
    dims$count <- ncsub[, "count"]
  }
  return(dims)
}

.set_nc_units <- function(data_list, nc_atts, make_units) {
  # Get all the units
  nc_units <- .get_attributes(nc_atts, "units")

  if (!is.null(nc_units) && nrow(nc_units) > 0 && make_units) {
    for (i in names(data_list)) {
      if (i %in% nc_units$variable) {
        uval <- unlist(nc_units$value[nc_units$variable == i])
        if(is.numeric(data_list[[i]])) {
          units(data_list[[i]]) <- try_as_units(uval[1L])
        } else {
          data_list[[i]]$units <- try_as_units(uval[1L])
        }
      }
    }
  }
  return(data_list)
}

.get_coords <- function(nc, dims) {
  ## cannot assume we have COORDS style coordinate varibales
  ## - so create them as 1:length if needed
  coords = setNames(vector("list", length(dims$name)), dims$name)
  
  attr(coords, "cv") <- c()
  
  for (ic in seq_along(coords)) {
    subidx <- seq(dims$start[ic], length = dims$count[ic])

    ## create_dimvar means we can var_get it
    ## test checks if there's actuall a variable of the dim name
    if (dims$name[ic] %in% ncmeta::nc_vars(nc)$name) {
      coords[[ic]] <- RNetCDF::var.get.nc(nc, variable = dims$name[ic])[subidx]
		
      attr(coords, "cv") <- c(attr(coords, "cv"), dims$name[ic])
      
    } else {
      coords[[ic]] <- subidx
    }
  }
  return(coords)
}

.get_nc_raster <- function(coords) {
  ## can we create a raster?
  raster = NULL
  ## which coords are regular
  if (length(coords) > 1) {
    raster = get_raster(affine = c(0, 0),
                        dimensions = names(coords)[1:2], curvilinear = FALSE)
  }
  return(raster)
}

.clean_coords <- function(coords, coord_var, atts, eps) {
	## hack longitudes for #277
	if (any(lon_coord <- grepl("lon", coord_var$X, ignore.case = TRUE))) {
		lon_coord <- coord_var$X[lon_coord][1]
		lons <- coords[[lon_coord]]
		if (!is.null(lons) && !regular_intervals(lons, epsilon = eps) &&
			.is_degrees(atts, lon_coord) &&
			lons[1L] > 180 && min(lons) > 0) {
			coords[[lon_coord]] <- ((coords[[lon_coord]] + 180) %% 360) - 180
		} else if(!is.null(lons) && .is_degrees(atts, lon_coord) &&
				  max(lons) > 180 && min(lons) > 180) {
			coords[[lon_coord]] <- ((coords[[lon_coord]] + 180) %% 360) - 180
		} else if(!is.null(lons) && .is_degrees(atts, lon_coord) &&
				  max(lons) > 180) {
			message(paste0("0-360 longitude crossing the international date", 
						   "line encountered.\nLongitude coordinates will be",
						   "0-360 in output."))
			
		}
	}
	coords
}

.get_nc_dimensions <- function(dimensions, coord_var, coords, nc, dims,
                               var_names, curvilinear, eps, ignore_bounds,
                               atts) {

  to_rectilinear = FALSE

  regular <- .is_regular(coords, eps)

  for (i in seq_along(coords)) {
  	
  	from_to <- dimensions[[i]]$from:dimensions[[i]]$to
  	
  	try_bounds <- names(coords)[i] %in% var_names && 
  		!ignore_bounds &&
  		length(bounds <- coord_var[coord_var$variable == names(coords)[i], ]$bounds) > 0 &&
  		bounds %in% var_names
  	
  	if (try_bounds) {
  		
  		bounds = RNetCDF::var.get.nc(nc, bounds)
  		if (!is.matrix(bounds)) # single instance, returns a vector
  			bounds = matrix(bounds, nrow = 2)
  		
  		if(!bounds[1, 1] < coords[[i]][1] | !bounds[2, 1] > coords[[i]][1]) {
  			warning(paste("bounds not enveloping", names(coords)[i], "coordinates. Ignoring."))
  			try_bounds <- FALSE
  		} else {
  			
  			is_reg = ncol(bounds) > 1 && length(u <- .unique_fuzz(apply(bounds, 2, diff), eps)) == 1 &&
  				length(v <- .unique_fuzz(diff(bounds[1,]), eps)) == 1
  			
  			if (is_reg && abs(u + v) < eps) {
  				warning(paste("bounds for", names(coords)[i], "seem to be reversed; reverting them"))
  				bounds = apply(bounds, 2, sort) # should not be needed according to CF, but see #133
  				u = v
  			}
  			
  			if (is_reg && abs(v - u) < eps) {
  				dimensions[[i]]$offset = bounds[1,1]
  				dimensions[[i]]$delta = v
  			} else {
  				dimensions[[i]]$values = make_intervals(bounds[1, from_to], bounds[2,from_to])
  				dimensions[[i]]$point = FALSE
  				if (i %in% 1:2 && length(curvilinear) < 1) # FIXME: ? hard-coding here that lon lat are in the first two dimensions:
  					to_rectilinear = TRUE
  			}
  		}
  	}
  	
  	if (regular[i] & !try_bounds) {
      mdc = mean(diff(coords[[i]]))
      coord_name = names(coords)[i]
      t_dim = coord_var[coord_var$variable == coord_name,]$T
      if (is.null(t_dim) || length(t_dim) == 0)
        t_dim = NA_character_
      # https://github.com/r-spatial/stars/issues/378
      if (!is.na(t_dim) && names(coords)[i] == t_dim)
        dimensions[[i]]$offset[1L] = coords[[i]][1]
      else
        dimensions[[i]]$offset[1L] = coords[[i]][1] - mdc/2
      ## NaN for singleton dims, but that seems ok unless we have explicit interval?
      dimensions[[i]]$delta[1L]  = mdc
    } else {
      dimensions[[i]]$values = coords[[i]][from_to]
      ## offset/delta for fall-back index (and for NA test )
      ## https://github.com/r-spatial/stars/blob/master/R/dimensions.R#L294-L303
      dimensions[[i]]$offset[1L] = NA_real_
      dimensions[[i]]$delta[1L] = NA_real_
    }
  }
  if (to_rectilinear)
    dimensions = .as_rectilinear(dimensions)
  return(dimensions)
}

.get_time_meta <- function(coord_var, var, meta) {
	
	TIME_name = as.character(na.omit(unlist(
		coord_var[coord_var$variable == var, ][c("T")])))
	
	if(!length(TIME_name)) {
		
		list(tname = NULL, calendar = NULL, tunit = NULL)	
		
	} else {
		
		atts = meta$attribute[meta$attribute$variable == TIME_name, ]
		
		## might not exist, so default to NULL
		calendar = unlist(atts$value[atts$name == "calendar"])[1L]
		tunit = unlist(atts$value[atts$name == "units"])[1L]
		
		list(tname = TIME_name, calendar = calendar, tunit = tunit)	
	}
}

.get_nc_time <- function(dimensions, make_time, coord_var, var, meta) {
  # sort out time -> POSIXct:
  if (make_time) {
    if (all("T" %in% names(coord_var))) {
      
      tmeta <- .get_time_meta(coord_var, var, meta)

      if (!is.na(tmeta$tname) && length(tmeta$tname) == 1L &&
          meta$variable$ndims[match(tmeta$tname, meta$variable$name)] == 1) {
        
        if (tmeta$tname %in% names(dimensions)) {
          tdim <- NULL
          if (is.null(tmeta$tunit) || inherits(tmeta$tunit, "try-error")) {
            warning("ignoring units of time dimension, not able to interpret")
          } else {
            tdim = make_cal_time2(dimensions,
            					  time_name = tmeta$tname,
            					  time_unit = tmeta$tunit, 
            					  cal = tmeta$calendar)

          }

          if (!is.null(tdim)) dimensions[[tmeta$tname]] <- tdim

        }
      }
    }
  }
  return(dimensions)
}

.get_curvilinear_coords <- function(curvilinear, dimensions, nc, dims) {
  curvi_coords <- lapply(curvilinear, function(.v) {

    cv_matcher <- RNetCDF::var.inq.nc(nc, .v)$dimids
    cv_matcher <- match(cv_matcher, dims$id[1:2])

    RNetCDF::var.get.nc(nc,
                        variable = .v,
                        start = dims[1:2, "start", drop = TRUE][cv_matcher],
                        count = dims[1:2, "count", drop = TRUE][cv_matcher],
                        collapse = FALSE,
                        unpack = TRUE)
  })
  names(curvi_coords)[1:2] <- names(dimensions)[1:2]

  expected_shape <- c(dimensions[[1]]$to, dimensions[[2]]$to)

  if(!all(dim(curvi_coords[[1]]) == expected_shape)) {
    curvi_coords[1] <- t(curvi_coords[1])
  }

  if(!all(dim(curvi_coords[2]) == expected_shape)) {
    curvi_coords[2] <- t(curvi_coords[2])
  }

  return(curvi_coords)
}

make_cal_time2 <- function(x, time_name = NULL, time_unit = NULL, cal = NULL) {
   if(inherits(x, "dimensions")) {
    	tm = st_get_dimension_values(x, time_name)
    	dimension <- x[[time_name]]
    } else {
    	tm <- x$values
    	dimension <- x
    }

    if(! is.null(cal)  && cal %in% c("360_day", "365_day", "noleap")) {
      if (!requireNamespace("PCICt", quietly = TRUE)) {
        stop("package PCICt required, please install it first") # nocov
      }
      t01 = set_units(0:1, time_unit, mode = "standard")
      delta = if (grepl("months", time_unit)) {
        if (cal == "360_day")
          set_units(30 * 24 * 3600, "s", mode = "standard")
        else
          set_units((365/12) * 24 * 3600, "s", mode = "standard")
      } else
        set_units(as_units(diff(as.POSIXct(t01))), "s", mode = "standard")


      origin = as.character(as.POSIXct(t01[1]))
      v.pcict = PCICt::as.PCICt(tm * as.numeric(delta), cal, origin)
      if (!is.null(dimension$values)) {
        v = dimension$values
        if (inherits(v, "intervals")) {
          start = PCICt::as.PCICt(v$start * as.numeric(delta), cal, origin)
          end =   PCICt::as.PCICt(v$end   * as.numeric(delta), cal, origin)
          dimension$values = make_intervals(start, end)
        } else
          dimension$values = v.pcict
      } else {
        dimension$offset = v.pcict[1]
        dimension$delta = diff(v.pcict[1:2])
      }

      dimension$refsys = "PCICt"
    } else { # Gregorian/Julian, POSIXct:
      if (!is.null(dimension$values)) {
        v = dimension$values
        if (inherits(v, "intervals")) {
          start = as.POSIXct(units::set_units(v$start, time_unit, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
          end =   as.POSIXct(units::set_units(v$end,   time_unit, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
          dimension$values = make_intervals(start, end)
        } else
          dimension$values = as.POSIXct(units::set_units(tm, time_unit, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
      } else {
        t0 = dimension$offset
        t1 = dimension$offset + dimension$delta
        t.posix = as.POSIXct(units::set_units(c(t0, t1), time_unit, mode = "standard")) # or: utcal.nc(u, c(t0,t1), "c")
        dimension$offset = t.posix[1]
        dimension$delta = diff(t.posix)
      }
      dimension$refsys = "POSIXct"
    }
    dimension


}

.is_netcdf_cf_dsg <- function(meta) {
  featuretype <- .get_attributes(meta$attribute, "featureType", "NC_GLOBAL")

  if(!is.null(featuretype)) {
    if(grepl( "timeseries", featuretype$value, ignore.case = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

.get_geom_name <- function(meta) {
  geometry <- unlist(unique(.get_attributes(meta$attribute, "geometry")$value))
  if(length(geometry) > 1) {
    warning(paste("Only a single geometry is supported. Using", geometry[1])) #nocov
    geometry <- geometry[1]
  }

  if(!is.null(geometry)) {
    ncdfgeom::read_geometry(meta$source$source)
  } else {
    NA
  }
}

#' @param sf_geometry sf data.frame with geometry and attributes to be added to stars object.
#' Must have same number of rows as timeseries instances.
#' @details For the \code{ncdfgeom} method: objects are point-timeseries with optional line or polygon geometry for each timeseries specified with the \code{sf_geometry} parameter. See \pkg{ncdfgeom} for more about this NetCDF-based format for geometry and timeseries.
#' @name st_as_stars
#' @export
#'
st_as_stars.ncdfgeom <- function(.x, ..., sf_geometry = NA) {

  crs <- sf::st_crs('OGC:CRS84')

  if(length(.x$alts) == 0) {
    ts_points <- data.frame(X = .x$lons, Y = .x$lats)
    ts_points <- sf::st_as_sf(ts_points, coords = c("X", "Y"), crs = crs)

  } else {
    ts_points <- data.frame(X = .x$lons, Y = .x$lats, Z = .x$alts)
    ts_points <- sf::st_as_sf(ts_points, coords = c("X", "Y", "Z"), crs = crs)
  }


  data <- .x$data_frames[[1]]

  gdim <- create_dimension(from = 1, to = length(.x$lats),
                           refsys = crs, point = TRUE,
                           values = ts_points$geometry)
  tdim <- create_dimension(from = 1, to = length(.x$time),
                           refsys = "POSIXct", point = FALSE,
                           values = as.POSIXct(.x$time))
  dim <- list(time = tdim, points = gdim)

  if(inherits(sf_geometry, "sf")) {
    if(length(gdim$values) != length(st_geometry(sf_geometry)))
      stop("geometry must be same length as instance dimension of timeseries")

    is_point <- any(grepl("point", class(st_geometry(sf_geometry)), ignore.case = TRUE))

    sf_dim <- create_dimension(from = 1, to = length(gdim$values),
                               refsys = st_crs(sf_geometry),
                               point = is_point, is_raster = FALSE,
                               values = st_geometry(sf_geometry))

    dim <- c(dim, list(geometry = sf_dim))
  }

  st_stars(x = setNames(list(as.matrix(.x$data_frames[[1]])),
                        .x$varmeta[[1]]$name),
           dimensions =  create_dimensions(dim))
}


