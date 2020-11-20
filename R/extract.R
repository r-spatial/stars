#' Extract cell values at point locations
#'
#' Extract cell values at point locations
#' @name st_extract
#' @export
#' @returns if \code{x} has more dimensions than only x and y (raster), an 
#' object of class \code{stars} with POINT geometries replacing x and y raster
#' dimensions; otherwise an object of \code{sf} with extracted values.
#' @details points outside the raster are returned as \code{NA} values.
st_extract = function(x, ...) UseMethod("st_extract")

#' @name st_extract
#' @param x object of class \code{stars} or \code{stars_proxy}
#' @param pts object of class \code{sf} or \code{sfc} with POINT geometries
#' @param bilinear logical; use bilinear interpolation rather than nearest neighbour?
#' @param time_column character or integer; name or index of a column with time or date values that will be matched to values of the dimension "time" in \code{x}, after which this dimension is reduced. This is useful to extract data cube values along a trajectory; see https://github.com/r-spatial/stars/issues/352 .
#' @param ... ignored
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' r = read_stars(tif)
#' pnt = st_sample(st_as_sfc(st_bbox(r)), 10)
#' st_extract(r, pnt)
#' st_extract(r, pnt) %>% st_as_sf()
#' st_extract(r[,,,1], pnt)
st_extract.stars = function(x, pts, ..., bilinear = FALSE, time_column = attr(pts, "time_column")) {

	stopifnot(inherits(pts, c("sf", "sfc")), st_crs(pts) == st_crs(x), 
		all(st_dimension(pts) == 0))

	sf_column = attr(pts, "sf_column") %||% "geometry"

	tm_pts = if (!is.null(time_column))
				pts[[time_column]] # else NULL

	pts = st_geometry(pts)

	if (bilinear && !inherits(x, "stars_proxy"))
		x = st_as_stars_proxy(x)

	m = if (inherits(x, "stars_proxy")) {
			if (utils::packageVersion("sf") < "0.9-7") {
				stop("sf >= 0.9-7 required")
				# remove this else clause when sf 0.9-7 has become a requirement:
				gdal_extract = function(...) stop("sf >= 0.9-7 required for st_extract-ing a stars_proxy object")
			}
			try_result = try(x0 <- st_as_stars(x, downsample = dim(x)/2), silent = TRUE)
			lapply(x, function(y) do.call(abind, lapply(y, 
				gdal_extract, pts = st_coordinates(pts), bilinear = bilinear)))
		} else {
			x = st_normalize(st_upfront(x))
			cr = colrow_from_xy(st_coordinates(pts), x, NA_outside = TRUE)
			ix = (cr[,2] - 1) * dim(x)[1] + cr[,1]
			lapply(x, function(y) 
				array(y, dim = c(prod(dim(x)[1:2]), prod(dim(x)[-(1:2)])))[ix, , drop = FALSE])
		}
	# reset factors & units attributes:
	for (i in seq_along(m)) {
		if (inherits(x[[i]], "factor")) {
			if (is.character(m[[i]]))
				m[[i]] = structure(as.factor(m[[i]]), dim = dim(m[[i]]))
			m[[i]] = structure(m[[i]], levels = levels(x[[i]]), 
				colors = attr(x[[i]], "colors"), class = "factor")
		} else if (inherits(x[[i]], "units"))
			units(m[[i]]) = units(x[[i]])
		else if (inherits(x, "stars_proxy") && !inherits(try_result, "try-error") && inherits(x0[[i]], "units"))
			units(m[[i]]) = units(x0[[i]])
	}
	if (!is.null(time_column)) {
		tm = match("time", names(st_dimensions(x)))
		if (is.na(tm))
			stop("cannot match times: x does not have a dimension called 'time'")
		tm_cube = st_get_dimension_values(x, "time")
		tm_ix = match_time(tm_pts, tm_cube)
		m = lapply(m, function(p) p[cbind(seq_along(pts), tm_ix)])
	}
	if (NCOL(m[[1]]) > 1) { # return stars:
		for (i in seq_along(x))
			dim(m[[i]]) = c(length(pts), dim(x)[-(1:2)])
		d = structure(st_dimensions(x),
			raster = get_raster(dimensions = rep(NA_character_,2)))
		d[[1]] = create_dimension(values = pts)
		d[[2]] = NULL
		names(d)[1] = sf_column
		setNames(st_as_stars(m, dimensions = d), names(x))
	} else { # return sf:
		df = setNames(as.data.frame(lapply(m, function(i) structure(i, dim = NULL))), names(x))
		df[[sf_column]] = st_geometry(pts)
		if (!is.null(time_column))
			df$time = tm_cube[tm_ix]
		st_as_sf(df)
	}
}

match_time = function(a, b) {
	if (inherits(a, "POSIXct") && inherits(b, "Date"))
		a = as.Date(a)
	if (inherits(b, "POSIXct") && inherits(a, "Date"))
		b = as.Date(b)
	stopifnot(inherits(a, class(b)))
	match(a, b)
}
