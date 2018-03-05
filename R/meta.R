#' read raster/array metadata from file or connection
#'
#' read raster/array dataset from file or connection
#' @param .x if character, name of file(s) to read; if list: list with arrays
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param sub integer or logical; sub-datasets to be read
#' @param quiet logical; print progress output?
#' @param NA_value numeric value to be used for conversion into NA values; by default this is read from the input file
#' @param ... ignored
#' @return object of class \code{stars}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' # x1 = read_stars(nv, options = "OVERVIEW_LEVEL=1")
read_stars_meta = function(.x, ..., options = character(0), driver = character(0), 
		sub = TRUE, quiet = FALSE, NA_value = NA_real_) {

	x = .x
	if (length(x) > 1) { # recurse:
		lapply(x, read_stars_meta, options = options, driver = driver, sub = sub, quiet = quiet)
		# do.call(c, c(ret, along = 3))
	}

	properties = gdal_read(x, options = options, driver = driver, read_data = FALSE, NA_value = NA_value)

	if (properties$bands[2] == 0) { # read sub-datasets: different attributes
		sub_names = split_strings(properties$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		# sub_datasets = gdal_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)
		names(sub_datasets) = nms
		sub_datasets = sub_datasets[sub]
		nms = names(sub_datasets)

		.read_stars = function(x, options, driver, quiet) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			read_stars_meta(x, options = options, driver = driver)
		}
		ret = lapply(sub_datasets, .read_stars, options = options, 
			driver = properties$driver[1], quiet = quiet)
		if (! quiet)
			cat("\n")
		ret
	} else  { # we have one single array:
		if (properties$driver[1] == "netCDF")
			properties = parse_netcdf_meta(properties, x)
		properties = parse_meta(properties)
		units = if (!is.null(properties$units) && !is.na(properties$units))
				try_as_units(properties$units)
			else
				NULL

		dims = c(
			x = properties$cols[2], 
			y = properties$rows[2], 
			bands = properties$bands[2],
			lengths(properties$dim_extra))

		structure(properties, dims = dims, file_names = x, units = units,
			dimensions = create_dimensions(dims, properties), class = "stars_meta")
	}
}
