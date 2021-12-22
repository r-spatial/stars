#' get dimensions from stars object
#' @name st_dimensions
#' @export
#' @param .x object to retrieve dimensions information from 
#' @param ... further arguments
#' @param value new object of class \code{dimensions}, with matching dimensions
#' @return the \code{dimensions} attribute of \code{x}, of class \code{dimensions}
st_dimensions = function(.x, ...) UseMethod("st_dimensions")

#' @export
#' @name st_dimensions
st_dimensions.stars = function(.x, ...) attr(.x, "dimensions")

#' @export
st_dimensions.dimensions = function(.x, ...) .x

#' @export
#' @name st_dimensions
`st_dimensions<-` = function(x, value) UseMethod("st_dimensions<-")

#' @export
#' @name st_dimensions
#' @param x object of class \code{dimensions}
`st_dimensions<-.stars` = function(x, value) {
	stopifnot(inherits(value, "dimensions"), length(x) && all(dim(x[[1]]) == dim(value)))
	st_stars(x, value)
}

#' @export
#' @name st_dimensions
`st_dimensions<-.stars_proxy` = function(x, value) {
	if (!is.null(attr(x, "call_list")))
		stop("st_dimensions<- on a stars_proxy object only works if there is no call list")
	NextMethod()
}


#' @export
#' @name st_dimensions
`st_dimensions<-.list` = function(x, value) {
	st_as_stars(x, dimensions = value)
}


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
#' @param values values for this dimension (e.g. \code{sfc} list-column), or length-1 \code{dimensions} object
#' @param names character; vector with new names for all dimensions, or with the single new name for the dimension indicated by \code{which}
#' @param xy length-2 character vector; (new) names for the \code{x} and \code{y} raster dimensions
#' @export
#' @examples
#' x = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
#' # Landsat 7 ETM+ band semantics: https://landsat.gsfc.nasa.gov/the-enhanced-thematic-mapper-plus/
#' # set bands to values 1,2,3,4,5,7:
#' (x1 = st_set_dimensions(x, "band", values = c(1,2,3,4,5,7), names = "band_number", point = TRUE))
#' # set band values as bandwidth
#' rbind(c(0.45,0.515), c(0.525,0.605), c(0.63,0.69), c(0.775,0.90), c(1.55,1.75), c(2.08,2.35)) %>%
#'   units::set_units("um") -> bw # or: units::set_units(µm) -> bw
#' # set bandwidth midpoint:
#' (x2 = st_set_dimensions(x, "band", values = 0.5 * (bw[,1]+bw[,2]), 
#'    names = "bandwidth_midpoint", point = TRUE))
#' # set bandwidth intervals:
#' (x3 = st_set_dimensions(x, "band", values = make_intervals(bw), names = "bandwidth"))
st_set_dimensions = function(.x, which, values = NULL, point = NULL, names = NULL, xy, ...) {
	d = st_dimensions(.x)
	if (!missing(which) && is.character(which))
		which = match(which, base::names(d))
	if (! is.null(values)) {
		if (is.na(which))
			stop("which should be a name or index of an existing dimensions")
		if (inherits(values, "dimensions")) {
			if (is.null(names))
				names <- names(values)
			values <- values[[1]]$values
		}
		if (!inherits(values, "intervals") && dim(.x)[which] != length(values)) {
			if (dim(.x)[which] == length(values) - 1) # create intervals:
				values = as_intervals(values)
			else
				stop(paste("length of values (", length(values), 
					") does not match length of dimension", which, "(", dim(.x)[which], ")"))
		}
		d[[which]] = create_dimension(values = values, point = point %||% d[[which]]$point, ...)
		if (! is.null(names) && length(names) == 1)
			base::names(d)[which] = names
		r = attr(d, "raster")
		if (isTRUE(r$curvilinear)) {
			# FIXME: there's much more that should be checked for curvilinear grids...
			# https://github.com/r-spatial/stars/issues/460
			if (which %in% r$dimensions && !is.matrix(values))
				attr(d, "raster")$curvilinear = FALSE 
		}
#		else if (inherits(values, "sfc"))
#			base::names(d)[which] = "sfc"
	} else if (!is.null(point) && is.logical(point)) {
		d[[which]]$point = point
	} else if (!missing(names)) {
		if (length(d) != length(names) && length(names) != 1)
			stop("length of names should match number of dimensions")
		r = attr(d, "raster")
		if (length(d) == length(names)) {
			# handle names in raster attribute, #46
			# problematic again in #379
			if (any(!is.na(r$dimensions)))
				r$dimensions = names[match(r$dimensions, names(d))]
			new_names = names
		} else { # replace the name of dimension `which`, #354
			if (names(d)[which] %in% r$dimensions)
				r$dimensions[match(names(d)[which], r$dimensions)] = names
			new_names = names(d)
			new_names[which] = names
		}
		attr(d, "raster") = r
		base::names(d) = new_names
	} else if (! missing(xy)) {
		stopifnot(length(xy) == 2)
		r = attr(d, "raster")
		r$dimensions = as.character(xy)
		attr(d, "raster") = r
	} else
		d[[which]] = create_dimension(from = 1, to = dim(.x)[which], ...)
	if (inherits(.x, "stars_proxy"))
		structure(.x, dimensions = d)
	else
		st_as_stars(unclass(.x), dimensions = d)
}


#' @name st_dimensions
#' @param where character, one of 'start', 'center' or 'end'. Set to NA (default) to ignore and use \code{max} and \code{center} explictly.  This argument provides a convenient alternative to setting \code{max} and \code{center}.
#' @param max logical; if \code{TRUE} return the end, rather than the beginning of an interval
#' @param center logical; if \code{TRUE} return the center of an interval; if \code{NA} return the center for raster dimensions, and the start of intervals in other cases
#' @export
#' @examples
#' m = matrix(1:20, nrow = 5, ncol = 4)
#' dim(m) = c(x = 5, y = 4) # named dim
#' (s = st_as_stars(m))
#' st_get_dimension_values(s, 'x', where = "start")
#' st_get_dimension_values(s, 'x', center = FALSE)
#' st_get_dimension_values(s, 'x', where = "center")
#' st_get_dimension_values(s, 'x', center = TRUE)
#' st_get_dimension_values(s, 'x', where = "end")
#' st_get_dimension_values(s, 'x', max = TRUE)
st_get_dimension_values = function(.x, which, ..., where = NA, max = FALSE, center = NA) {
	if ((!is.numeric(which) && !is.character(which)) || length(which) != 1)
		stop("argument which should be a length 1 dimension index or name") # nocov
  if (!is.na(where)){
    w = tolower(where[1])
    if (w == 'center'){
      max = FALSE
      center = TRUE
    } else if (w == 'start'){
      max = FALSE
      center = FALSE
    } else if (w == 'end'){
      max = TRUE
      center = NA
    } else {
      stop("where, if not NA, must be 'start', 'center' or 'end': ", where)
    }
  }
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
		ud = if (is.atomic(x) && (is.numeric(x) || inherits(x, c("POSIXt", "Date"))))
				unique(diff(x))
			else {
				if (inherits(x, "intervals") && identical(tail(x$end, -1), head(x$start, -1)))
					x$end - x$start
				else
					return(FALSE)
			}
		isTRUE(as.numeric(abs(diff(range(ud)) / mean(ud))) < epsilon)
	}
}

create_dimension = function(from = 1, to, offset = NA_real_, delta = NA_real_, 
		refsys = NA_character_, point = NA, values = NULL, is_raster = FALSE)  {

	example = NA

	if (! is.null(values)) { # figure out from values whether we have sth regular:
		from = 1
		to = length(values)
		if (!(is.character(values) || is.factor(values)) && is.atomic(values)) { 
			if (! all(is.finite(values)))
				warning("dimension value(s) non-finite")
			else {
				if (regular_intervals(values)) {
					offset = values[1]
					if (length(values) > 1) {
						delta = diff(values[1:2])
					# shift half grid cell size if x or y raster dim cell midpoints:
						if (is_raster)
							offset = offset - 0.5 * delta
					}
					values = NULL
					example = offset
				} else
					example = values
			}
		}
		if (inherits(values, "intervals"))
			example = values$start

		# refsys:
		if (inherits(example, "POSIXct"))
			refsys = "POSIXct"
		else if (inherits(example, "Date"))
			refsys = "Date"
		else if (inherits(example, "PCICt"))
			refsys = "PCICt"
		else if (inherits(example, "units"))
			refsys = "udunits"

		if (inherits(values, "sfc")) {
			point = inherits(values, "sfc_POINT")
			if (!is.na(st_crs(values)) && is.na(refsys)) # inherit:
				refsys = st_crs(values)
		}
		if (is.numeric(values) && (is.na(point) || !point)) {
			values = if (is_raster)
					set_dimension_values(centers = values)
				else
					set_dimension_values(start = values)
		}
	}
	structure(list(from = unname(from), to = unname(to), offset = unname(offset), 
		delta = unname(delta), refsys = refsys, point = unname(point), values = values), 
		class = "dimension")
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

get_crs = function(pr) {
	if (!is.null(pr$crs))
		pr$crs
	else if (!is.null(pr$wkt)) # newer sf, but GDAL < 3.0.0
		st_crs(pr$wkt)
	else if (!is.null(pr$proj_wkt))
		st_crs(pr$proj_wkt)
	else 
		st_crs(NA)
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
				refsys = get_crs(pr)),
			y = create_dimension(from = pr$rows[1], to = pr$rows[2], 
				offset = pr$geotransform[4],
				delta = pr$geotransform[6],
				point = pr$point,
				refsys = get_crs(pr)),
			# default:
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
	# handle band descriptions, if present:
	if (!is.null(lst$band) && !is.null(pr$descriptions) && all(pr$descriptions != ""))
		lst$band$values = pr$descriptions
	# set up raster:
	raster = get_raster(affine = pr$geotransform[c(3,5)],
						dimensions = c("x", "y"),
						curvilinear = FALSE, 
						blocksizes = pr$blocksizes)
	create_dimensions(lst, raster)
}

get_raster = function(affine = rep(0, 2), dimensions = c("x", "y"),
					  curvilinear = FALSE, blocksizes = NULL) {
	if (any(is.na(affine)))
		affine = c(0, 0)
	structure(list(affine = affine,
				   dimensions = dimensions,
				   curvilinear = curvilinear,
				   blocksizes = blocksizes), 
		  class = "stars_raster")
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
	invisible(x)
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
	# longname:
	pr$long_name = get_val(paste0(get_val("NC_GLOBAL#variable_id", meta), "#long_name"), meta)
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
					if (v == "time" && !is.na(cal) && cal %in% c("360_day", "365_day", "noleap")) {
						origin = 0:1
						units(origin) = try_as_units(u)
						delta = if (grepl("months", u)) {
								if (cal == "360_day")
									set_units(30 * 24 * 3600, "s", mode = "standard")
								else
									set_units((365/12) * 24 * 3600, "s", mode = "standard")
							} else
								set_units(as_units(diff(as.POSIXct(origin))), "s", mode = "standard")
						origin_txt = as.character(as.POSIXct(origin[1]))
						if (!requireNamespace("PCICt", quietly = TRUE))
							stop("package PCICt required, please install it first") # nocov
						pr$dim_extra[[v]] = PCICt::as.PCICt(pr$dim_extra[[v]] * as.numeric(delta), cal, origin_txt)
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
	un = try(suppressWarnings(as_units(u)), silent = TRUE)
	if (inherits(un, "try-error")) # try without ^:
		un = try(suppressWarnings(as_units(gsub("^", "", u, fixed = TRUE))), silent = TRUE)
	if (inherits(un, "try-error")) # try without **
		un = try(suppressWarnings(as_units(gsub("**", "", u, fixed = TRUE))), silent = TRUE)
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

	if (length(center) == 1)
		center = setNames(rep(center, length(x)), names(x))
	if (length(max) == 1)
		max = setNames(rep(max, length(x)), names(x))

	if (is.list(max))
		max = unlist(max)
	if (is.list(center))
		center = unlist(center)

	if (any(max & center, na.rm = TRUE))
		stop("only one of max and center can be TRUE, not both")

	where = setNames(rep(0.0, length(x)), names(x)) # offset
	for (i in seq_along(x)) {
		if (max[i])
			where[i] = 1.0
		if (isTRUE(center[i]))
			where[i] = 0.5
	}

	dimensions = x
	xy = attr(x, "raster")$dimensions
	gt = get_geotransform(x)
	lst = vector("list", length(dimensions))
	names(lst) = names(dimensions)
	if (! is.null(xy) && all(!is.na(xy))) { # we have raster: where defaulting to 0.5
		where[xy] = ifelse(!max[xy] & (is.na(center[xy]) | center[xy]), 0.5, where[xy])
#		where_xy = if (!max && (is.na(center) || center))
#				0.5
#			else
#				where
		if (xy[1] %in% names(lst)) # x
			lst[[ xy[1] ]] = get_dimension_values(dimensions[[ xy[1] ]], where[[ xy[1] ]], gt, "x")
		if (xy[2] %in% names(lst))  # y
			lst[[ xy[2] ]] = get_dimension_values(dimensions[[ xy[2] ]], where[[ xy[2] ]], gt, "y")
	}

	if ("crs" %in% names(lst))
		lst[[ "crs" ]] = dimensions[[ "crs" ]]$values

	for (nm in setdiff(names(lst), c(xy, "crs"))) # non-xy, non-crs dimensions
		lst[[ nm ]] = get_dimension_values(dimensions[[ nm ]], where[[nm]], NA, NA)

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
as.data.frame.dimensions = function(x, ..., digits = 6, usetz = TRUE, stars_crs = getOption("stars.crs") %||% 28) {
	lst = lapply(x, function(y) {
			if (length(y$values) > 3) {
				y$values = if (is.array(y$values))
						paste0("[", paste(dim(y$values), collapse = "x"), "] ", 
							format(min(y$values), digits = digits), ",...,", 
							format(max(y$values), digits = digits))
					else if (inherits(y$values[[1]], "crs"))
						paste0(format(y$values[[1]]), ",...,", format(y$values[[length(y$values)]]))
					else
						paste0(format(head(y$values, 1)), ",...,", 
							format(tail(y$values, 1)))
			}
			if (is.na(y$refsys))
				y$refsys = NA_character_
			else if (nchar(tail(format(y$refsys), 1)) > stars_crs)
				y$refsys = paste0(substr(tail(format(y$refsys), 1), 1L, stars_crs - 3),"...")
			y
		}
	)
	mformat = function(x, ..., digits) {
		if (inherits(x, c("PCICt", "POSIXct"))) 
			format(x, ..., usetz = usetz)
		else
			format(x, digits = digits, ...) 
	}
	lst = lapply(lst, function(x) sapply(x, mformat, digits = digits))
	ret = data.frame(do.call(rbind, lst), stringsAsFactors = FALSE)
	r = attr(x, "raster")
	if (! any(is.na(r$dimensions))) {
		ret$raster = rep("", nrow(ret))
		ret[r$dimensions[1], "raster"] = "[x]"
		ret[r$dimensions[2], "raster"] = "[y]"
		names(ret) = c(names(lst[[1]]), "x/y")
	}
	ret
}

#' @export
print.dimensions = function(x, ...) {
	ret = as.data.frame(x, ...)
	print(ret)
	print(attr(x, "raster"))
	invisible(x)
}

identical_dimensions = function(lst, ignore_resolution = FALSE, tolerance = 0) {
	if (length(lst) > 1) {
		d1 = attr(lst[[1]], "dimensions")
		for (i in 2:length(lst)) {
			di = attr(lst[[i]], "dimensions")
			if (ignore_resolution) {
				for (j in seq_along(d1))
					d1[[j]]$delta = d1[[j]]$to = NA_real_
				for (j in seq_along(di))
					di[[j]]$delta = di[[j]]$to = NA_real_
			}
			if (! isTRUE(all.equal(d1, di, tolerance = tolerance)))
				return(FALSE)
		}
	}
	TRUE
}

combine_dimensions = function(dots, along, check_dims_identical = TRUE) {
	dims = st_dimensions(dots[[1]])
	if (along > length(dims)) {
		if (length(dots) > 1 && check_dims_identical) {
			for (i in 2:length(dots))
				if (! isTRUE(all.equal(dims, st_dimensions(dots[[i]]))))
					stop(paste("dimensions of element", 1, "and", i, "are not identical"))
		}
		dims[[along]] = create_dimension(from = 1, to = length(dots), values = names(dots))
	} else {
		offset = lapply(dots, function(x) attr(x, "dimensions")[[along]]$offset)
		if (any(is.na(offset))) { # concatenate values if non-NULL:
			dims[[along]]$from = 1
			dims[[along]]$to = sum(sapply(dots, function(x) { d = st_dimensions(x)[[along]]; d$to - d$from + 1} ))
			if (!is.null(dims[[along]]$values))
				dims[[along]]$values = do.call(c, lapply(dots, function(x) attr(x, "dimensions")[[along]]$values))
		} else {
			values = do.call(c, lapply(dots, function(y) st_get_dimension_values(y, along)))
			dims[[along]] = create_dimension(values = values)
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
			if (max(i) > -1 && min(i) < 0)
				stop("cannot mix positive and negative indexes")
			if (is.logical(i))
				i = which(i)
			else if (all(i < 0))
				i = setdiff(rang, abs(i)) # subtract
			if (all(diff(i) == 1)) {
				if (max(i) > length(rang))
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
`[<-.dimensions` = function(x, i, value) {
	# stopifnot(length(i) == length(value))
	if (!is.null(value))
		create_dimensions(NextMethod(), raster = attr(x, "raster"))
	else
		NextMethod()
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
