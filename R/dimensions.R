#' get dimensions from stars object
#' @export
#' @param .x object to retrieve dimensions information from 
#' @param ... further arguments
#' @return the \code{dimensions} attribute of \code{x}, of class \code{dimensions}
st_dimensions = function(.x, ...) UseMethod("st_dimensions")

#' @export
#' @name st_dimensions
st_dimensions.stars = function(.x, ...) attr(.x, "dimensions")

#' @export
st_dimensions.dimensions = function(.x, ...) .x


#' @export
#' @name st_dimensions
st_dimensions.array = function(.x, ...) {
	if (length(list(...)) > 0)
		stop("only one argument expected")
	dn = dimnames(.x)

	# raster?
	r = if (all(c("x", "y") %in% names(dn)))
			get_raster(affine = c(0.0, 0.0), dimensions = c("x", "y"), curvilinear = FALSE)
		else
			get_raster(affine = c(0.0, 0.0), dimensions = rep(NA_character_, 2), curvilinear = FALSE)

	ret = if (is.null(dn))
			st_dimensions(list(.x)) # default
		else # try to get dimension names and default values from dimnames(.x):
			create_dimensions(lapply(dn, function(y) create_dimension(values = y)), r)

	if (is.null(names(ret)) || any(names(ret) == ""))
		names(ret) = make.names(seq_along(ret))

	ret
}

#' @export
st_dimensions.matrix = st_dimensions.array


#' @export
#' @name st_dimensions
#' @param .raster length 2 character array with names (if any) of the raster dimensions
#' @param affine numeric; specify parameters of the affine transformation
#' @param cell_midpoints logical; if \code{TRUE} AND the dimension values are strictly regular, the values are interpreted as the cell midpoint values rather than the cell offset values when calculating offset (i.e., the half-cell-size correction is applied); can have a value for each dimension, or else is recycled
#' @param point logical; does the pixel value (measure) refer to a point (location) value or to an pixel (area) summary value?
#' @details dimensions can be specified in two ways. The simplest is to pass a vector with numeric values for a numeric dimension, or character values for a categorical dimension. Parameter \code{cell_midpoints} is used to specify whether numeric values refer to the offset (start) of a dimension interval (default), or to the center; the center case is only available for regular dimensions. For rectilinear numeric dimensions, one can specify either a vector with cell borders (start values), or a data.frame with two columns named "start" and "end", with the respective interval start and end values. In the first case, the end values are computed from the start values by assuming the last two intervals have equal width.
#' 
st_dimensions.default = function(.x, ..., .raster, affine = c(0, 0), 
		cell_midpoints = FALSE, point = FALSE) {
	d = list(...)
	if (! missing(.x))
		d = append(list(.x), d)
	if (missing(.raster)) { 
		.raster = if (all(c("x", "y") %in% names(d)) && is.numeric(d$x) && is.numeric(d$y))
				c("x", "y") 
			else 
				rep(NA_character_, 2)
	}
	create_dimensions(mapply(create_dimension, values = d, is_raster = cell_midpoints, point = point, SIMPLIFY = FALSE),
		raster = get_raster(dimensions = .raster, affine = affine))
}

#' @name st_dimensions
#' @param which integer or character; index or name of the dimension to be changed
#' @param values values for this dimension (e.g. \code{sfc} list-column)
#' @param names character; new names vector for (all) dimensions, ignoring \code{which}
#' @export
#' @examples
#' x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
#' # Landsat 7 ETM+ band semantics: https://landsat.gsfc.nasa.gov/the-enhanced-thematic-mapper-plus/
#' # set bands to values 1,2,3,4,5,7:
#' (x1 = st_set_dimensions(x, "band", values = c(1,2,3,4,5,7), names = "band_number", point = TRUE))
#' # set band values as bandwidth
#' bw = units::set_units(rbind(c(0.45,0.515), c(0.525,0.605), c(0.63,0.69), c(0.775,0.90), c(1.55,1.75), c(2.08,2.35)), µm)
#' # set bandwidth midpoint:
#' (x2 = st_set_dimensions(x, "band", values = 0.5 * (bw[,1]+bw[,2]), names = "bandwidth_midpoint", point = TRUE))
#' # set bandwidth intervals:
#' (x3 = st_set_dimensions(x, "band", values = make_intervals(bw), names = "bandwidth"))
st_set_dimensions = function(.x, which, values, names, ...) {
	d = st_dimensions(.x)
	if (! missing(values)) {
		if (is.character(which))
			which = match(which, names(d))
		if (is.na(which))
			stop("which should be a name or index of an existing dimensions")
		if (dim(.x)[which] != length(values)) {
			if (dim(.x)[which] == length(values) - 1)
				values = as_intervals(values)
			else
				stop(paste("length of values does not match dimension", which))
		}
		d[[which]] = create_dimension(values = values, ...)
		if (! missing(names) && length(names) == 1)
			base::names(d)[which] = names
		else if (inherits(values, "sfc"))
			base::names(d)[which] = "sfc"
	} else { # set all names
		if (! missing(names)) {
			# handle names in raster attribute, #46
			r = attr(d, "raster")
			if (any(!is.na(r$dimensions))) {
				r$dimensions = names[match(r$dimensions, names(d))]
				attr(d, "raster") = r
			}
			if (length(d) != length(names))
				stop("length of names should match number of dimension")
			base::names(d) = names
		}
	}
	st_as_stars(unclass(.x), dimensions = d)
}

#' @name st_dimensions
#' @param max logical; if \code{TRUE} return the end, rather than the beginning of an interval
#' @param center logical; if \code{TRUE} return the center of an interval; if \code{NA} return the center for raster dimensions, and the start of intervals in other cases
#' @export
st_get_dimension_values = function(.x, which, ..., max = FALSE, center = NA) {
	expand_dimensions(.x, ..., max = max, center = center)[[which]]
}


#' @export
"[.dimensions" = function(x, i, j,..., drop = FALSE) {
	raster = attr(x, "raster")
	ret = unclass(x)[i]
	if (isTRUE(all(raster$dimensions %in% names(ret))))
		create_dimensions(ret, raster)
	else
		create_dimensions(ret) # drop raster
}

regular_intervals = function(x, epsilon = 1e-10) {
	if (length(x) <= 1)
		FALSE
	else {
		ud = if (is.atomic(x))
				unique(diff(x))
			else {
				if (identical(tail(x$end, -1), head(x$start, -1)))
					x$end - x$start
				else
					return(FALSE)
			}
		diff(range(ud)) / mean(ud) < epsilon
	}
}

create_dimension = function(from = 1, to, offset = NA_real_, delta = NA_real_, 
		refsys = NA_character_, point = NA, values = NULL, is_raster = FALSE)  {

#	if (inherits(refsys, "crs"))
#		refsys = refsys$proj4string

	if (! is.null(values)) { # figure out from values whether we have sth regular:
		from = 1
		to = length(values)
		if (is.character(values) || is.factor(values))
			values = as.character(values)
		else if (is.atomic(values)) { 
			if (! all(is.finite(values)))
				warning("dimension value(s) non-finite")
			else {
				if (regular_intervals(values)) {
					offset = values[1]
					if (length(values) > 1) {
						delta = values[2] - values[1]
					# shift half grid cell size if x or y raster dim cell midpoints:
						if (is_raster)
							offset = offset - 0.5 * delta
					}
					values = NULL
				}
				if (inherits(offset, "POSIXct"))
					refsys = "POSIXct"
				if (inherits(offset, "Date"))
					refsys = "Date"
				if (inherits(offset, "PCICt"))
					refsys = "PCICt"
			}
		}
		if (inherits(values, "sfc")) {
			point = inherits(values, "sfc_POINT")
			if (!is.na(st_crs(values)) && is.na(refsys)) # inherit:
				refsys = st_crs(values)$proj4string
		}
		if (is.numeric(values) && (is.na(point) || !point)) {
			values = if (is_raster)
					set_dimension_values(centers = values)
				else
					set_dimension_values(start = values)
		}
		if (inherits(values, "units") || (inherits(values, "intervals") && inherits(values$start, "units")))
			refsys = "udunits"
	}
	structure(list(from = from, to = to, offset = offset, delta = delta, 
		refsys = refsys, point = point, values = values), class = "dimension")
}

create_dimensions = function(lst, raster = NULL) {
	if (is.numeric(lst)) # when called with a dim(array) argument:
		lst = setNames(lapply(seq_along(lst), function(i) create_dimension(from = 1, to = lst[i])), 
		               names(lst))
	if (is.null(names(lst)))
		names(lst) = make.names(seq_along(lst))
	if (any(names(lst) == "")) {
		sel = which(names(lst) == "")
		names(lst)[sel] = make.names(seq_along(sel))
	}
	if (is.null(raster))
		raster = get_raster(dimensions = c(NA_character_, NA_character_))
	structure(lst, raster = raster, class = "dimensions")
}

create_dimensions_from_gdal_meta = function(dims, pr) {
	#if (all(is.na(pr$geotransform)))
	#	pr$geotransform = c(0.0,  1.0,  0.0,  0.0,  0.0, -1.0) # some GTiffs...
	lst = vector("list", length(dims))
	names(lst) = names(dims)
	for (i in names(lst)) {
		lst[[i]] = switch(i,
			x = create_dimension(from = pr$cols[1], to = pr$cols[2], 
				offset = pr$geotransform[1], 
				delta = pr$geotransform[2],
				point = pr$point,
				refsys = if (is.null(pr$proj4string)) NA_character_ 
					else pr$proj4string),
			y = create_dimension(from = pr$rows[1], to = pr$rows[2], 
				offset = pr$geotransform[4],
				delta = pr$geotransform[6],
				point = pr$point,
				refsys = if (is.null(pr$proj4string)) NA_character_ 
					else pr$proj4string),
			create_dimension(from = 1, to = dims[i]) # time? depth+units? To be filled in later...
		)
	}
	if (! is.null(pr$dim_extra)) { # netcdf...
		for (d in names(pr$dim_extra)) {
			refsys = if (inherits(pr$dim_extra[[d]], "POSIXct")) 
					"POSIXct" 
				else if (inherits(pr$dim_extra[[d]], "PCICt"))
					"PCICt"
				else 
					NA_character_
			de = pr$dim_extra[[d]]
			diff.de = diff(de)
			lst[[d]] = if (length(unique(diff.de)) <= 1) {
					delta = if (length(diff.de)) diff.de[1] else NA_real_
					create_dimension(from = 1, to = length(de), offset = de[1], delta = delta, refsys = refsys)
				} else
					create_dimension(from = 1, to = length(de), values = de, refsys = refsys)
		}
		lst[["band"]] = NULL
	}
	# set up raster:
	raster = get_raster(affine = pr$geotransform[c(3,5)], dimensions = c("x", "y"), curvilinear = FALSE)
	create_dimensions(lst, raster)
}

get_raster = function(affine = rep(0, 2), dimensions = c("x", "y"), curvilinear = FALSE) {
	if (any(is.na(affine))) {
		# warning("setting NA affine values to zero")
		affine = c(0, 0)
	}
	structure(list(affine = affine, dimensions = dimensions, curvilinear = curvilinear), class = "stars_raster")
}

get_geotransform = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	stopifnot(inherits(x, "dimensions"))
	r = attr(x, "raster")
	if (is.null(r))
		rep(NA_real_, 6)
	else {
		xd = x[[ r$dimensions[1] ]]
		yd = x[[ r$dimensions[2] ]]
		c(xd$offset, xd$delta, r$affine[1], yd$offset, r$affine[2], yd$delta)
	}
}


#' @export
print.stars_raster = function(x, ...) {
	if (any(is.na(x$affine)))
		cat(paste("affine parameters:", x$affine[1], x$affine[2], "\n"))
	else if (any(x$affine != 0.0))
		cat(paste("sheared raster with parameters:", x$affine[1], x$affine[2], "\n"))
	if (x$curvilinear)
		cat("curvilinear grid\n")
}

get_val = function(pattern, meta) {
	i = grep(pattern, meta)
	if (length(i))
		strsplit(meta[i], "=")[[1]][2]
	else
		NA_character_
}

parse_netcdf_meta = function(pr, name) {
	meta = pr$meta
	spl = strsplit(name, ":")[[1]] # "C:\..." results in length 2:
	name = if (length(spl) < 3) # name is not the variable, but the file name; FIXME: how to make it the variable?
			"zzzzz40163a99980" # bogus string, to avoid match
		else
			tail(spl, 1) # last one contains 
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
				if (!is.na(rhs)) {
					rhs = strsplit(gsub("[\\{\\}]" , "", rhs), ",")
					pr$dim_extra[[v]] = as.numeric(rhs[[1]])
				} else {
					rhs = get_val(paste0("NETCDF_DIM_", v), meta) # nocov # FIXME: find example?
					pr$dim_extra[[v]] = as.numeric(rhs)           # nocov
				}

				cal = get_val(paste0(v, "#calendar"), meta)
				u =   get_val(paste0(v, "#units"), meta)
				if (! is.na(u)) {
					if (v == "time" && !is.na(cal) && cal %in% c("360_day", "365_day")) {
						origin = 0:1
						units(origin) = try_as_units(u)
						delta = as.numeric(set_units(as_units(diff(as.POSIXct(origin))), "s", mode = "standard"))
						origin_txt = as.character(as.POSIXct(origin[1]))
    					if (!requireNamespace("PCICt", quietly = TRUE))
        					stop("package PCICt required, please install it first") # nocov
						pr$dim_extra[[v]] = PCICt::as.PCICt(pr$dim_extra[[v]] * delta, cal, origin_txt)
					} else {
						units(pr$dim_extra[[v]]) = try_as_units(u)
						if (v == "time" && !inherits(try(as.POSIXct(pr$dim_extra[[v]]), silent = TRUE),
								"try-error")) {
							pr$dim_extra[[v]] = as.POSIXct(pr$dim_extra[[v]])
						}
					}
				}
			}
			pr$dim_extra = rev(pr$dim_extra)
		}
	}
	pr
}

try_as_units = function(u) {
	un = try(as_units(u), silent = TRUE)
	if (inherits(un, "try-error")) {
		warning(paste("ignoring unrecognized unit:", u), call. = FALSE)
		NULL
	} else
		un
}

parse_gdal_meta = function(properties) {
	point = get_val("AREA_OR_POINT", properties$meta)
	properties$point = switch(point,
	  Area=FALSE,
	  Point=TRUE,
	  NA)
	properties
}

expand_dimensions = function(x, ...) UseMethod("expand_dimensions")

expand_dimensions.stars = function(x, ...) {
	expand_dimensions(st_dimensions(x), ...)
}

# returns, in case of numeric dimensions:
# 	center = TRUE: return center values for x and y coordinates, interval start values otherwise
# 	center = FALSE: return start values
#   center = NA: return centers for x/y raster, otherwise start values
#   add_max = TRUE: add in addition to x and y start values an x_max and y_max end values
expand_dimensions.dimensions = function(x, ..., max = FALSE, center = NA) {

	if (isTRUE(max) && isTRUE(center))
		stop("only one of max and center can be TRUE, not both")

	where = if (max) 
			1.0
		else if (isTRUE(center))
			0.5
		else
			0.0

	dimensions = x
	xy = attr(x, "raster")$dimensions
	gt = get_geotransform(x)
	lst = vector("list", length(dimensions))
	names(lst) = names(dimensions)
	if (! is.null(xy) && all(!is.na(xy))) { # we have raster:
		if (!max && (is.na(center) || center))
			where = 0.5
		if (xy[1] %in% names(lst)) # x
			lst[[ xy[1] ]] = get_dimension_values(dimensions[[ xy[1] ]], where, gt, "x")
		if (xy[2] %in% names(lst))  # y
			lst[[ xy[2] ]] = get_dimension_values(dimensions[[ xy[2] ]], where, gt, "y")
	}
	for (nm in setdiff(names(lst), xy)) # non-xy dimensions
		lst[[ nm ]] = get_dimension_values(dimensions[[ nm ]], where, NA, NA)

	lst
}


#' @export
dim.dimensions = function(x) {
	if (is_curvilinear(x))
		setNames(sapply(x, function(x) { x$to - x$from + 1 } ), names(x))
	else
		lengths(expand_dimensions(x)) # FIXME: optimise?
}


#' @export
print.dimensions = function(x, ..., digits = 6) {
	lst = lapply(x, function(y) {
			if (length(y$values) > 2) {
				y$values = if (is.array(y$values))
						paste0("[", paste(dim(y$values), collapse = "x"), "] ", 
							format(min(y$values), digits = digits), ",...,", 
							format(max(y$values), digits = digits))
					else
						paste0(format(head(y$values, 1)), ",...,", 
							format(tail(y$values, 1)))
			}
			nc = if (inherits(y$refsys, "crs"))
					nchar(y$refsys$proj4string)
				else
					nchar(y$refsys)
			if (!is.na(y$refsys) && nc > 28)
				y$refsys = paste0(substr(y$refsys, 1L, 25),"...")
			y
		}
	)
	mformat = function(x, ..., digits) {
		if (inherits(x, "PCICt")) 
			format(x, ...)
		else
			format(x, digits = digits, ...) 
	}
	lst = lapply(lst, function(x) lapply(x, mformat, digits = digits))
	ret = data.frame(do.call(rbind, lst), stringsAsFactors = FALSE)
	r = attr(x, "raster")
	if (!any(is.na(r$dimensions))) {
		ret$raster = rep("", nrow(ret))
		ret[r$dimensions[1], "raster"] = "[x]"
		ret[r$dimensions[2], "raster"] = "[y]"
		names(ret) = c(names(lst[[1]]), "")
	}
	print(ret)
	print(attr(x, "raster"))
}

identical_dimensions = function(lst) {
	if (length(lst) > 1) {
		for (i in 2:length(lst))
			if (!identical(attr(lst[[1]], "dimensions"), attr(lst[[i]], "dimensions")))
				return(FALSE)
	}
	TRUE
}

combine_dimensions = function(dots, along) {
	dims = attr(dots[[1]], "dimensions")
	if (along > length(dims)) {
		dims[[along]] = create_dimension(from = 1, to = length(dots), values = names(dots))
	} else {
		offset = lapply(dots, function(x) attr(x, "dimensions")[[along]]$offset)
		if (any(is.na(offset))) {
			dims[[along]]$from = 1
			dims[[along]]$to = sum(sapply(dots, function(x) { d = st_dimensions(x)[[along]]; d$to - d$from + 1} ))
		} else {
			offset = structure(do.call(c, offset), tzone = attr(offset[[1]], "tzone")) # preserve TZ
			if (length(unique(diff(offset))) == 1) { # regular & sorted
				dims[[along]]$offset = min(offset)
				dims[[along]]$delta = diff(offset)[1]
			} else {
				dims[[along]]$values = offset  # nocov # FIXME: find example?
				dims[[along]]$delta = NA_real_ # nocov
				# FIXME: irregular: should set $values here and NA offset!
			}
			dims[[along]]$from = 1
			dims[[along]]$to = length(offset)
		}
	}
	dims
}

#' @export
seq.dimension = function(from, ..., center = FALSE) { # does what expand_dimensions also does, for single dimension
	get_dimension_values(from, where = ifelse(center, 0.5, 0.0), NA, what = NA_character_)
}


#' @export
`[.dimension` = function(x, i, ...) {
	if (!missing(i)) {
		if (!is.null(x$values) && !is.matrix(x$values))
			x$values = x$values[i]
		if (!is.na(x$from)) {
			rang = x$from:x$to # valid range
			if (all(diff(i) == 1)) {
				if (min(i) < 1 || max(i) > length(rang))
					stop("invalid range selected")
				sel = rang[i]
				x$from = min(sel)
				x$to = max(sel)
			} else { # invalidate offset & delta
				x$delta = x$offset = NA
				x$from = 1
				x$to = length(i)
			}
		}
	}
	x
}

#' @export
dimnames.stars = function(x) {
	names(st_dimensions(x))
}

#' @export
`dimnames<-.stars` = function(x, value) {
	for (i in seq_along(x))
		names(dim(x[[i]])) = value
	names(attr(x, "dimensions")) = value
	x
}
