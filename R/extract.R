#' Extract cell values at point locations
#'
#' Extract cell values at point locations
#' @name st_extract
#' @export
st_extract = function(x, ...) UseMethod("st_extract")

#' @name st_extract
#' @param x object of class \code{stars} or \code{stars_proxy}
#' @param at object of class \code{sf} or \code{sfc} with geometries, or two-column matrix with coordinate points in rows, indicating where to extract values of \code{x}, or a \code{stars} object with geometry and temporal dimensions (vector data cube)
#' @param bilinear logical; use bilinear interpolation rather than nearest neighbour?
#' @param time_column character or integer; name or index of a column with time or date values that will be matched to values of the first temporal dimension (matching classes \code{POSIXct}, \code{POSIXt}, \code{Date}, or \code{PCICt}), in \code{x}, after which this dimension is reduced. This is useful to extract data cube values along a trajectory; see https://github.com/r-spatial/stars/issues/352 .
#' @param interpolate_time logical; should time be interpolated? if FALSE, time instances are matched using the coinciding or the last preceding time in the data cube.
#' @param FUN function used to aggregate pixel values when geometries of \code{at} intersect with more than one pixel
#' @param resampling character; resampling method; for method cubic or cubicspline,
#' `stars_proxy` objects should be used and GDAL should have version >= 3.10.0
#' @param ... passed on to \link{aggregate.stars} when geometries are not exclusively POINT geometries
#' @returns if \code{at} is of class \code{matrix}, a matrix with extracted values is returned; 
#' if \code{at} is of class \code{stars} and a temporal dimension was passed to \code{time_column},
#' a \code{stars} object with the original object dimensions
#' and the extracted values as attributes.
#' otherwise: if \code{x} has more dimensions than only x and y (raster), an 
#' object of class \code{stars} with POINT geometries replacing x and y raster
#' dimensions, if this is not the case, an object of \code{sf} with extracted values.
#' @details points outside the raster are returned as \code{NA} values. For
#' large sets of points for which extraction is needed, passing a matrix as
#' to \code{at} may be much faster than passing an \code{sf} or \code{sfc} object.
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' r = read_stars(tif)
#' pnt = st_sample(st_as_sfc(st_bbox(r)), 10)
#' st_extract(r, pnt)
#' st_extract(r, pnt) %>% st_as_sf()
#' st_extract(r[,,,1], pnt)
#' st_extract(r, st_coordinates(pnt)) # "at" is a matrix: return a matrix
st_extract.stars = function(x, at, ..., bilinear = FALSE, time_column = 
		attr(at, "time_column") %||% attr(at, "time_col"),
		interpolate_time = bilinear, FUN = mean,
		resampling = c("nearest", "bilinear", "cubic", "cubicspline")) {

	stopifnot(inherits(at, c("sf", "sfc", "stars", "matrix")))
	resampling = match.arg(resampling)
	if (bilinear) {
		stopifnot(resampling %in% c("nearest", "bilinear"))
		resampling = "bilinear"
	}
	at_orig = at
	if (inherits(at_orig, "stars") & !is.null(time_column)) 
		at = st_as_sf(at, long = TRUE)
	if (inherits(at, "matrix"))
		pts = at
	else {
		stopifnot(st_crs(at) == st_crs(x))
		if (!all(st_dimension(at) == 0)) { # should check & branch here in case of MULTIPOINT?
			stopifnot(!bilinear) # bilinear interpolation not supported for time matching with lines/polygons
			# from aggregate.stars_proxy:
			by = st_geometry(at)
			# this assumes the result is small, no need to proxy
			l = lapply(seq_along(by), function(i) aggregate(st_normalize(st_as_stars(x[by[i]])), by[i], FUN, ...))
			if(is.null(time_column)) {
				return(do.call(c, c(l, along = list(which_sfc(l[[1]]))))) # RETURNS!
			} else {
				# to pass to time matching		
				x_agg = do.call(c, c(l, along = list(which_sfc(l[[1]]))))
			}
		}
		sf_column = attr(at, "sf_column") %||% "geometry"

		tm_pts = if (!is.null(time_column))
					at[[time_column]] # else NULL
		at = st_geometry(at)
		pts = st_coordinates(at)
	}

	if (resampling != "nearest" && !inherits(x, "stars_proxy"))
		x = st_as_stars_proxy(x)

	min_dist = NULL
	if (inherits(x, "mdim"))
		x = st_as_stars(x) # realize
	m = if (inherits(x, "stars_proxy")) {
			try_result = try(x0 <- st_as_stars(x, downsample = dim(x)/2), silent = TRUE)
			lapply(x, function(y) do.call(abind, lapply(get_names(y), 
				gdal_extract, pts, resampling)))
		} else if (exists("x_agg")) {
			# Allow time matching for lines and polygons using aggregate
			x = st_normalize(st_upfront(x_agg))
			lapply(seq_along(x), function(i) x[[i]]) 
		} else {
			x = st_normalize(st_upfront(x))
			if (is_curvilinear(x)) { # https://github.com/r-spatial/stars/issues/632
				d = st_distance(at, st_as_sfc(x, as_points = TRUE))
				ix = apply(d, 1, which.min) # FIXME: handle points outside the raster
				min_dist = d[cbind(seq_along(at), ix)]
			} else {
				cr = colrow_from_xy(pts, x, NA_outside = TRUE)
				ix = (cr[,2] - 1) * dim(x)[1] + cr[,1]
			}
			lapply(x, function(y) 
				array(y, dim = c(prod(dim(x)[1:2]), prod(dim(x)[-(1:2)])))[ix, , drop = FALSE])
		}
	
	# reset factors & units attributes:
	for (i in seq_along(m)) {
		if (inherits(x[[i]], "factor")) {
			if (is.character(m[[i]]))
				m[[i]] = match(as.vector(m[[i]]), levels(x[[i]]))
			m[[i]] = structure(m[[i]], levels = levels(x[[i]]), 
				colors = attr(x[[i]], "colors"), class = class(x[[i]]))
		} else if (inherits(x[[i]], "units"))
			units(m[[i]]) = units(x[[i]])
		else if (inherits(x, "stars_proxy") && !inherits(try_result, "try-error") && inherits(x0[[i]], "units"))
			units(m[[i]]) = units(x0[[i]])
	}
	# match times:
	if (!is.null(time_column)) {
		refsys_time = c("POSIXct", "POSIXt", "Date", "PCICt")
		## If there are more than two temporal dimensions, the first one is taken
		tm = names(which(sapply(
			st_dimensions(x),
			function(i) any(i$refsys %in% refsys_time))))[1]
		if (is.na(tm))
			stop("cannot match times: x does not have a temporal dimension")
		tm_cube = st_dimensions(x)[[tm]]$values %||% st_get_dimension_values(x, tm)
		tm_ix = match_time(tm_pts, tm_cube,
						   intervals = !st_dimensions(x)[[tm]]$point,
						   interpolate_time)
		if (!interpolate_time)
			m = lapply(m, function(p) p[cbind(seq_along(at), tm_ix)])
		else {
			interpolate = function(x, ix) { 
				i = floor(ix)
				di = ix - i
				if (is.na(ix) || di == 0)
					x[i]
				else 
					(1 - di) * x[i] + di * x[i+1]
			}
			# each time series is reduced to a _single_ time step, interpolated:
			m = lapply(m, function(n) mapply(interpolate, asplit(n, 1), tm_ix))
		}
	}
	if (NCOL(m[[1]]) > 1) { # multi-band:
		if (inherits(at, "matrix"))
			do.call(cbind, m)
		else { # return stars:
			for (i in seq_along(x))
				dim(m[[i]]) = c(length(at), dim(x)[-(1:2)])
			d = structure(st_dimensions(x),
				raster = get_raster(dimensions = rep(NA_character_,2)))
			d[[1]] = create_dimension(values = at)
			d[[2]] = NULL
			names(d)[1] = sf_column
			st = setNames(st_as_stars(m, dimensions = d), names(x))
			if (!is.null(min_dist))
				st$min_dist = min_dist
			st
		}
	} else {
		df = setNames(as.data.frame(lapply(m, function(i) structure(i, dim = NULL))), names(x))
		if (inherits(at, "matrix")) 
			df
		else { # return sf:
			df[[sf_column]] = st_geometry(at)
			if (!is.null(time_column)) { # add time columns of both cube and at:
				if (inherits(tm_cube, "intervals"))
					tm_cube = as.list(tm_cube)
				df[[tm]] = tm_cube[tm_ix]
				df[[time_column]] = tm_pts
			}
			sf = st_as_sf(df)
			if (!is.null(min_dist))
				sf$min_dist = min_dist
			if (!inherits(at_orig, "stars")) 
				sf
			else {
				st_as_stars(sf, dims = attr(attr(at_orig, "dimensions"), "names"))
			}
		}
	}
}

# match the times in a to those of b, or interpolate:
# if interpolate = FALSE, returns an integer in 1...length(b) or NA if outside
# if interpolate = TRUE, returns a continuous index in 1...length(b) or NA if outside
match_time = function(a, b, intervals = FALSE, interpolate = FALSE) {
	if (inherits(a, "POSIXct") && inherits(b, "Date"))
		a = as.Date(a)
	if (inherits(b, "POSIXct") && inherits(a, "Date"))
		b = as.Date(b)
	m = if (inherits(b, "intervals"))
			find_interval(a, b)
		else if (isTRUE(intervals) || interpolate) {
			m = findInterval(a, b, rightmost.closed = TRUE)
			m[ m == 0 | m == length(b) ] = NA
			m
		} else
			match(a, b)
	if (interpolate && !isTRUE(intervals) && !inherits(b, "intervals")) {
		b = as.numeric(b)
		a = as.numeric(a)
		b = c(b, tail(b, 1))
		m + (a - b[m])/(diff(b)[m])
	} else
		m
}
