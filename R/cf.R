# things related to the CFtime and ncdfCF packages

CF_calendar_regular = c("standard", "gregorian", "proleptic_gregorian", "utc", "tai")
CF_calendar_model = c("360_day", "365_day", "noleap", "366_day", "all_leap")

is_CFTime = function(x) {
	# catch things like "CFTime_360_day" as a CFTime refsys
	length(x) == 1 && !is.na(x) && is.character(x) && substr(x, 1, 6) == "CFTime"
}

"[.CFTime" = function(x, i = TRUE, ...) {
	x$indexOf(i)
}

#' @param sf_geometry sf data.frame with geometry and attributes to be added to stars object.
#' Must have same number of rows as timeseries instances.
#' @details For the \code{ncdfgeom} method: objects are point-timeseries with optional line or polygon geometry for each timeseries specified with the \code{sf_geometry} parameter. See \pkg{ncdfgeom} for more about this NetCDF-based format for geometry and timeseries.
#' @name st_as_stars
#' @export
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

# Take a CFVariable `var` and convert its axes into a dimensions structure
.dimensions_from_CF <- function(var) {
	axes = var$axes
	axis_order = match(sapply(axes, function(ax) ax$orientation), c("X", "Y", "Z", "T"))
	if (any(is.na(axis_order[1:2])))
		stop("Data variable does not have X and/or Y axes")
	if (axes[[ axis_order[1] ]]$length == 1 || axes[[ axis_order[2] ]]$length == 1)
		stop("Raster dimensions are degenerate")
	
	crs = if (is.null(var$crs))
		"OGC:CRS84" # If no grid_mapping, then data is lat-long, assuming WGS84 datum here,
	else {      # otherwise, tease the AUTHORITY out of the WKT2 string of the grid_mapping
				# This will often fail to identify an AUTHORITY because CF grid_mappings
				# are incomplete and a WKT2 thus has many "unknown"s.
		wkt = var$crs_wkt2
		auth = regexec('([a-zA-Z0-9]*)",["]?([a-zA-Z0-9]*)["]?\\]\\]$', wkt)
		auth = regmatches(wkt, auth)[[1]]
		if (!length(auth))
			NA_character_
		else
			paste(auth[-1], collapse = ":")
	}
	
	raster = get_raster(dimensions = c(axes[[ axis_order[1] ]]$name, axes[[ axis_order[2] ]]$name))
	dimensions = lapply(axes, function(ax) {
		if (ax$length == 1) 
			return (NULL)
			
		switch(ax$orientation,
			   X = create_dimension(values = ax$coordinates, refsys = crs, is_raster = TRUE), 
			   Y = create_dimension(values = ax$coordinates, refsys = crs, is_raster = TRUE),
			   T = create_dimension(to = length(time), refsys = "CFtime", values = ax$time),
		   		{
					v = ax$coordinates
		   			u = ax$attribute("units")
					if (!is.na(u))
						units(v) = try_as_units(u)
		   			create_dimension(values = v)
		   		}
			)
		}
	)
	dimensions = dimensions[lengths(dimensions) > 0]
	create_dimensions(dimensions, raster)
}

#' @param .var Character vector with names of netCDF data variables to convert
#'   into stars attributes. All data variables have to use the same set of axes.
#'   If omitted, the first data variable will be used.
#' @param all_compatible Logical flag that indicates if all data variables with
#'   the same axes as argument `var` uses should be converted into attributes.
#'   Default is `TRUE` when only one data variable is specified, `FALSE`
#'   otherwise.
#' @name st_as_stars
#' @export
st_as_stars.CFDataset <- function(.x, ..., .var = .x$var_names[1], all_compatible = length(var) == 1) {
	if (!requireNamespace("ncdfCF", quietly = TRUE))
		stop("Please install package 'ncdfCF' before using this functionality")
	
	all_var_names = .x$var_names
	if (!is.character(.var) || !length(.var) || !all(.var %in% all_var_names))
		stop("Argument `.var` must be a character vector of one of more data variable names in the data set.")
	all_compatible = is.logical(all_compatible) && all_compatible[1]
	
	# Grab all CFVariables from the passed data variable names - check that all are compatible on dimensions
	axis_names = sapply(.x[[ .var[1] ]]$axes, function(ax) ax$fullname)
	num_axes = length(axis_names)
	if (length(.var) > 1) {
		vars = lapply(.var, function(v_name) {
			v = .x[[v_name]]
			v_ax_names = sapply(v$axes, function(ax) ax$fullname)
			if (length(v_ax_names) == num_axes && all(v_ax_names == axis_names)) 
				v
			else
				NULL
		})
		if (any(is.null(vars)))
			stop("Data variables in argument `.var` have different axes.")
		names(vars) <- sapply(vars, function(v) v$name)
	} else if (all_compatible) {
		vars = lapply(all_var_names, function(nm) {
			v = .x[[nm]]
			if (all(sapply(v$axes, function(ax) ax$fullname) == axis_names))
				v
			else
				NULL
		})
		names(vars) <- sapply(vars, function(v) v$name)
		vars = vars[lengths(vars) > 0]
		if (!all(.var %in% names(vars)))
			stop("Data variables in argument `var` have different axes.")
	} else
		vars = setNames(list(.x[[ .var[1] ]]), .var[1])
	
	# Turn the list of CFVariables into stars attributes
	attributes = lapply(vars, function(v) {
		att = v$raw()
		dimnames(att) <- NULL
		un = v$attribute("units")
		if (!is.na(un))
			units(att) = try_as_units(un)
		att
	})
	names(attributes) = names(vars)
	
	dimensions = .dimensions_from_CF(vars[[1]])	
	st_stars(attributes, dimensions = dimensions)
}

#' @name st_as_stars
#' @export
st_as_stars.CFVariable <- function(.x, ...) {
	if (!requireNamespace("ncdfCF", quietly = TRUE))
		stop("Please install package 'ncdfCF' before using this functionality")
	
	dimensions = .dimensions_from_CF(.x)
	att <- .x$raw()
	dimnames(att) <- NULL
	attributes = setNames(list(att), .x$name)
	un = .x$attribute("units")
	if (!is.na(un))
		units(attributes[[1]]) = try_as_units(un)
	
	st_stars(attributes, dimensions = dimensions)
}
