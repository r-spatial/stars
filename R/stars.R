## read raster/array dataset from file or connection

#' get dimensions from stars object
#' @export
#' @param x object to retrieve dimensions information from 
#' @param ... further arguments
#' @return the \code{dimensions} attribute of \code{x}, of class \code{dimensions}
st_dimensions = function(x, ...) UseMethod("st_dimensions")

#' @export
#' @name st_dimensions
st_dimensions.stars = function(x, ...) attr(x, "dimensions")

create_dimension = function(from = 1, to, offset = NA_real_, delta = NA_real_, 
		geotransform = rep(NA_real_, 6), refsys = NA_character_, values = NULL) {
	list(from = from, to = to, offset = offset, delta = delta, 
		geotransform = geotransform, refsys = refsys, values = values)
}

create_dimensions = function(dims, pr = NULL) {
	if (!is.null(pr) && !is.null(pr$properties)) # messy!
		pr = pr$properties
	lst = vector("list", length(dims))
	names(lst) = names(dims)
	if (!is.null(pr)) {
		for (i in names(lst)) {
			lst[[i]] = switch(i,
				x = create_dimension(from = pr$cols[1], to = pr$cols[2], 
					offset = pr$geotransform[1], 
					delta = pr$geotransform[2], geotransform = pr$geotransform, 
					refsys = if (is.null(pr$proj4string)) NA_character_ 
						else pr$proj4string),
				y = create_dimension(from = pr$rows[1], to = pr$rows[2], 
					offset = pr$geotransform[4],
					delta = pr$geotransform[6], geotransform = pr$geotransform, 
					refsys = if (is.null(pr$proj4string)) NA_character_ 
						else pr$proj4string),
				create_dimension(from = 1, to = dims[i]) # time? depth+units?
			)
		}
	} else {
		for (i in names(lst))
			lst[[i]] = create_dimension(from = 1, to = dims[i])
	}
	if (! is.null(pr$dim_extra)) {
		for (d in names(pr$dim_extra)) {
			refsys = if (inherits(pr$dim_extra[[d]], "POSIXct")) "POSIXct" else NA_character_
			lst[[d]] = create_dimension(from = 1, to = 1, offset = pr$dim_extra[[d]], refsys = refsys)
		}
	}
	structure(lst, class = "dimensions")
}

parse_netcdf_meta = function(pr, name) {
	meta = pr$meta
	name = tail(strsplit(name, ":")[[1]], 1)
	get_val = function(pattern, meta) {
		i = grep(pattern, meta)
		if (length(i))
			strsplit(meta[i], "=")[[1]][2]
		else
			NA_character_
	}
	# unit:
	pr$units = get_val(paste0(name, "#units"), meta)
	# extra dims: NETCDF_DIM_EXTRA={time,zlev}
	val = get_val("NETCDF_DIM_EXTRA", meta)
	if (! is.na(val)) {
		val = substr(val, 2, nchar(val)-1) # e.g. "{time,depth}" removes { }
		val = strsplit(val, ",")[[1]]
		if (length(val)) {
			pr$dim_extra = vector("list", length(val))
			names(pr$dim_extra) = val
			for (v in val) {
				rhs = get_val(paste0("NETCDF_DIM_", v, "_VALUES"), meta)
				if (!is.na(rhs))
					pr$dim_extra[[v]] = as.numeric(rhs)
				else {
					rhs = get_val(paste0("NETCDF_DIM_", v), meta)
					pr$dim_extra[[v]] = as.numeric(rhs)
				}
				pr$dim_extra[[v]] = set_units(pr$dim_extra[[v]], 
					get_val(paste0(v, "#units"), meta))
				if (v == "time")
					pr$dim_extra[[v]] = as.POSIXct(pr$dim_extra[[v]])
			}
		}
	}
	pr
}

#' read raster/array dataset from file or connection
#' @param x if character, file name to read; if list: list with arrays
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param sub integer or logical; sub-datasets to be read
#' @param quiet logical; print progress output?
#' @return object of class \code{stars}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' x1 = st_stars(tif, options = "OVERVIEW_LEVEL=1")
st_stars = function(x, ...) UseMethod("st_stars")

#' @name st_stars
#' @export
st_stars.character = function(x, ..., options = character(0), driver = character(0), sub = TRUE, quiet = FALSE) {

	if (length(x) > 1) { # recurse:
		ret = lapply(x, st_stars, options = options, driver = driver, sub = sub, quiet = quiet)
		return(do.call(c, c(ret, along = 3)))
	}

	properties = CPL_read_gdal(x, options, driver, TRUE)

	if (properties$bands[2] == 0) { # read sub-datasets:
		sub_names = split_strings(properties$sub) # get named list
		sub_datasets = sub_names[seq(1, length(sub_names), by = 2)]
		sub_datasets = sub_datasets[sub]
		# sub_datasets = st_get_subdatasets(x, options)[sub] # -> would open x twice

		# FIXME: only for NetCDF:
		nms = sapply(strsplit(unlist(sub_datasets), ":"), tail, 1)

		read_stars = function(x, options, driver, keep_meta, quiet) {
			if (! quiet)
				cat(paste0(tail(strsplit(x, ":")[[1]], 1), ", "))
			st_stars(x, options = options, driver = driver)
		}
		ret = lapply(sub_datasets, read_stars, options = options, 
			driver = properties$driver[1], quiet = quiet)
		if (! quiet)
			cat("\n")
		structure(do.call(c, ret), names = nms)
	} else  {
		data = attr(properties, "data")
		properties = structure(properties, data = NULL)
		if (properties$driver[1] == "netCDF")
			properties = parse_netcdf_meta(properties, x)
		if (! is.null(properties$units) && ! is.na(properties$units))
			data = set_units(data, make_unit(properties$units))
		newdims = structure(rep(1, length(properties$dim_extra)), 
			names = names(properties$dim_extra))
		structure(list(structure(data, dim = c(dim(data), newdims))),
			names = x,
			dimensions = create_dimensions(dim(data), properties),
			class = "stars")
	}
}

#' @name st_stars
#' @param dimensions object of class dimensions
#' @export
st_stars.list = function(x, ..., dimensions = NULL) {
	if (length(x) > 1) {
		for (i in 2:length(x))
			if (!identical(dim(x[[1]]), dim(x[[i]])))
				stop("dim attributes not identical")
	}
	if (is.null(dimensions))
		dimensions = create_dimensions(x)
	structure(x, dimensions = dimensions, class = "stars")
}


#' @name st_stars
#' @param band integer; which band (dimension) to plot
#' @param attr integer; which attribute to plot
#' @param asp numeric; aspect ratio of image
#' @param rgb integer; specify three bands to form an rgb composite
#' @param maxColorValue numeric; passed on to \link{rgb}
#' @param xlab character; x axis label
#' @param ylab character; y axis label
#' @param ... passed on to \code{image.default}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' image(x)
image.stars = function(x, ..., band = 1, attr = 1, asp = 1, rgb = NULL, maxColorValue = 1,
		xlab = names(dims)[1], ylab = names(dims)[2]) {

	if (any(dim(x) == 1))
		x = adrop(x)

	dims = expand_dimensions(x)
	x = unclass(x[[ attr ]])
	x = if (length(dim(x)) == 3) {
			if (is.null(rgb))
				x[ , rev(seq_len(dim(x)[2])), band]
			else {
				stop("not yet supported")
				xy = dim(x)[1:2]
				x = structure(x[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
				x = rgb(x, maxColorValue = maxColorValue) # FIXME: deal with NAs
				dim(x) = xy
				#return(rasterImage(x[ , rev(seq_len(dim(x)[2]))], 0, 0, 1, 1, interpolate = FALSE))
			}
		} else
			x[ , rev(seq_len(dim(x)[2]))]
	image.default(dims[[1]], rev(dims[[2]]), unclass(x), asp = asp, xlab = xlab, ylab = ylab, ...)
}

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
xy_from_colrow = function(x, geotransform) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

expand_dimensions = function(x) {
	dimensions = st_dimensions(x)
	lst = vector("list", length(dimensions))
	names(lst) = names(dimensions)
	if ("x" %in% names(lst)) {
		x = dimensions[["x"]]
		gt = x$geotransform
		if (! all(is.na(gt)))
			lst[["x"]] = xy_from_colrow(cbind(seq(x$from, x$to) - .5, 0), gt)[,1]
		else
			stop("cannot determine x and y coordinates without geotransform")
	}
	if ("y" %in% names(lst)) {
		y = dimensions[["y"]]
		gt = y$geotransform
		if (! all(is.na(gt)))
			lst[["y"]] = xy_from_colrow(cbind(0, seq(y$from, y$to) - .5), gt)[,2]
		else
			stop("cannot determine x and y coordinates without geotransform")
	}
	for (nm in setdiff(names(lst), c("x", "y"))) {
		dm = dimensions[[nm]]
		lst[[nm]] = if (!is.null(dm$values))
				dm$values 
			else if (is.na(dm$offset) || is.na(dm$delta))
				seq(dm$from, dm$to)
			else
				seq(from = dm$offset, by = dm$delta, length.out = dm$to - dm$from + 1)
	}
	lst
}

#' @export
as.data.frame.stars = function(x, ...) {
	## FIXME: now ignores possible affine parameters:
	dims = attr(x, "dimensions")
	lapply(dims, 
		function(x) { 
			if (!is.null(x$geotransform)) {
				aff = x$geotransform[c(3,5)]
				if (!all(is.na(aff)) && any(aff != 0)) 
					stop("affine transformation needed") 
			}
		}
	)
	coords = do.call(expand.grid, expand_dimensions(x))
	data.frame(coords, lapply(x, c))
}

#' @export
print.dimensions = function(x, ..., digits = 6) {
	lst = lapply(x, function(y) { 
			aff = y$geotransform[c(3,5)]
			y$geotransform = if (any(!is.na(aff)) && any(aff != 0))
				paste(signif(y$geotransform, digits = digits), collapse = ", ")
			else
				NULL
			y
		}
	)
	lst = lapply(lst, function(x) lapply(x, format, digits = digits))
	ret = data.frame(do.call(rbind, lst), stringsAsFactors = FALSE)
	names(ret) = names(lst[[1]])
	print(ret)
}

#' @export
print.stars = function(x, ...) {
	add_units = function(x) {
		f = function(obj) if (inherits(obj, "units")) paste0("[", as.character(units(obj)), "]") else ""
		paste(names(x), sapply(x, f))
	}
	cat("stars object with", length(dim(x)), "dimensions and", 
		length(x), if (length(x) > 1) "attributes\n" else "attribute\n")
	cat("attribute(s):\n")
	df = as.data.frame(lapply(x, as.vector), optional = TRUE)
	names(df) = add_units(x)
	print(summary(df))
	cat("dimension(s):\n")
	lst = attr(x, "dimensions")
	print(lst, ...)
}

#' @export
aperm.stars = function(a, perm = NULL, ...) {
	if (is.null(perm))
		perm = rev(seq_along(dim(a)))
	if (is.character(perm) && is.null(dimnames(a[[1]]))) {
		ns = names(attr(a, "dimensions"))
		dn = lapply(as.list(dim(a)), seq_len)
		names(dn) = ns
		print(dn)
		for (i in seq_along(a))
			dimnames(a[[i]]) = dn
	}
	dimensions = structure(attr(a, "dimensions")[perm], class = "dimensions")
	structure(lapply(a, aperm, perm = perm, ...), 
		dimensions = dimensions, class = "stars")
}

#' @export
dim.stars = function(x, ...) {
	if (length(x) == 0)
		integer(0)
	else
		dim(x[[1]])
}

check_equal_dimensions = function(lst) {
	if (length(lst) > 1) {
		for (i in 2:length(lst))
			if (!identical(attr(lst[[1]], "dimensions"), attr(lst[[i]], "dimensions")))
				stop(paste("object 1 and", i, "have different dimensions"))
	}
	TRUE
}

handle_dimensions = function(dots, along) {
	dims = attr(dots[[1]], "dimensions")
	offset = lapply(dots, function(x) attr(x, "dimensions")[[along]]$offset)
	offset = structure(do.call(c, offset), tzone = attr(offset[[1]], "tzone")) # preserve TZ
	if (length(unique(diff(offset))) == 1) { # regular & sorted
		dims[[along]]$offset = min(offset)
		dims[[along]]$delta = diff(offset)[1]
	} else {
		dims[[along]]$values = offset
		dims[[along]]$delta = NA_real_
	}
	dims[[along]]$from = 1
	dims[[along]]$to = length(offset)
	dims
}

propagate_units = function(new, old) {
	for (i in seq_along(new))
		if (inherits(old[[i]], "units"))
			units(new[[i]]) <- units(old[[i]])
	new
}

#' @export
c.stars = function(..., along = NA_integer_) {
	dots = list(...)
	if (is.na(along)) { # merge attributes
		check_equal_dimensions(dots)
		st_stars(do.call(c, lapply(dots, unclass)), dimensions = attr(dots[[1]], "dimensions"))
	} else {
		if (length(dots) == 1 && is.na(along)) # attributes to array dimension:
			along = length(dim(dots[[1]]) + 1)
		ret = if (length(dots) == 1 && along == length(dim(dots[[1]])) + 1) { # collapse:
			dn = names(dots[[1]])
			do.call(abind, c(dots, along = along))
		} else { # loop over attributes:
			propagate_units(mapply(abind, ..., along = along, SIMPLIFY = FALSE), dots[[1]])
		}
		dims = handle_dimensions(dots, along)
		structure(ret, dimensions = dims, class = "stars")
	}
}

#' @export
adrop.stars = function(x, drop = which(dim(x) == 1), ...) {
	dims = structure(attr(x, "dimensions")[-drop], class = "dimensions")
	structure(lapply(x, adrop, drop = drop, ...), dimensions = dims, class = "stars")
	# deal with dimensions table
}
