# sf conversion things

#' @export
#' @name st_as_sf
st_as_sfc.stars = function(x, ..., as_points, which = seq_len(prod(dim(x)[1:2]))) {

	r = attr(st_dimensions(x), "raster")
	gt = get_geotransform(x)
	d = st_dimensions(x)[r$dimensions]
	if (!as_points && is_rectilinear(x)) {
		if (inherits(df <- d[[1]]$values, "data.frame"))
			d[[1]]$values = c(df$start, tail(df$end, 1))
		if (inherits(df <- d[[2]]$values, "data.frame"))
			d[[2]]$values = c(df$start, tail(df$end, 1))
	}
	st_as_sfc(d, ..., as_points = as_points, which = which, geotransform = gt, 
		end_points = dim(d) > dim(x)[r$dimensions])
}


#' replace x y raster dimensions with simple feature geometry list (points, or polygons = rasterize)
#' @param x object of class \code{stars}
#' @param as_points logical; if \code{TRUE}, generate points at cell centers, else generate polygons
#' @param ... arguments passed on to \code{st_as_sfc}
#' @param na.rm logical; remove cells with all missing values?
#' @return object of class \code{stars} with x and y raster dimensions replaced by a single sfc geometry list column containing either points or square polygons
#' @export
st_xy2sfc = function(x, as_points, ..., na.rm = TRUE) {

	d = st_dimensions(x)
	olddim = dim(x)

	if (! has_raster(x))
		stop("x and/or y not among dimensions")

	dxy = attr(d, "raster")$dimensions
	xy_pos = match(dxy, names(d))
	if (! all(xy_pos == 1:2))
		stop("raster dimensions need to be first and second dimension")

	# find which records are NA for all attributes:
	a = abind(x, along = length(dim(x)) + 1)
	keep = if (na.rm)
			as.vector(apply(a, c(1,2), function(x) !all(is.na(x))))
		else
			rep(TRUE, prod(dim(x)[1:2]))

	# flatten two dims x,y to one dim sfc (replacing "x")
	sfc = st_as_sfc(x, as_points = as_points, ..., which = which(keep))
	# overwrite raster-x with sfc:
	d[[ dxy[1] ]] = create_dimension(from = 1, to = length(sfc), values = sfc)
	# rename raster-x to sfc:
	names(d)[names(d) == dxy[1] ] = "sfc"
	# remove y:
	d[[ dxy[2] ]] = NULL
	attr(d, "raster") = get_raster(dimensions = rep(NA_character_, 2))
	# flatten arrays:
	for (i in seq_along(x))
		dim(x[[i]]) = c(sfc = length(keep), olddim[-xy_pos]) 
	# reduce arrays to non-NA cells:
	if (na.rm) {
		args = rep(list(rlang::missing_arg()), length(dim(x[[1]])))
		args[[1]] = which(keep)
		for (i in seq_along(x))
			x[[i]] = structure(eval(rlang::expr(x[[i]][ !!!args ])), levels = attr(x[[i]], "levels"))
	}

	structure(x, dimensions = d)
}

#' Convert stars object into an sf object
#' 
#' Convert stars object into an sf object
#' @name st_as_sf
#' @param x object of class \code{stars}
#' @param as_points logical; should cells be converted to points or to polygons? See details.
#' @param which linear index of cells to keep (this argument is not recommended to be used)
#' @param na.rm logical; should missing valued cells be removed, or also be converted to features?
#' @param merge logical; if \code{TRUE}, cells with identical values are merged (using \code{GDAL_Polygonize} or \code{GDAL_FPolygonize}); if \code{FALSE}, a polygon for each raster cell is returned; see details
#' @param use_integer (relevant only if \code{merge} is \code{TRUE}): if \code{TRUE}, before polygonizing values are rounded to 32-bits signed integer values (GDALPolygonize), otherwise they are converted to 32-bit floating point values (GDALFPolygonize).
#' @param long logical; if \code{TRUE}, return a long table form \code{sf}, with geometries and other dimensinos recycled
#' @param ... ignored
#' @details If \code{merge} is \code{TRUE}, only the first attribute is converted into an \code{sf} object. If \code{na.rm} is \code{FALSE}, areas with \code{NA} values are also written out as polygons.
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' x = x[,,,6] # a band with lower values in it
#' x[[1]][x[[1]] < 30] = NA # set lower values to NA
#' x[[1]] = x[[1]] < 100 # make the rest binary
#' x
#' (p = st_as_sf(x)) # removes NA areas
#' plot(p, axes = TRUE)
#' (p = st_as_sf(x, na.rm = FALSE)) # includes polygons with NA values
#' plot(p, axes = TRUE)
st_as_sf.stars = function(x, ..., as_points = !merge, na.rm = TRUE, 
		merge = has_raster(x) && !(is_curvilinear(x) || is_rectilinear(x)), 
		use_integer = is.logical(x[[1]]) || is.integer(x[[1]]), long = FALSE) { 

	crs = st_crs(x)
	if (merge) {
		mask = if(na.rm) {
				mask = x[1]
				mask[[1]] = !is.na(mask[[1]])
				mask
			} else
				NULL
		ret = gdal_polygonize(x, mask, use_integer = use_integer, geotransform = get_geotransform(x),
				use_contours = as_points, ...)
		# factor levels?
		if (!is.null(lev <- attr(x[[1]], "levels")))
			ret[[1]] = structure(ret[[1]], class = "factor", levels = lev)
		return(st_set_crs(ret, crs))
	}

	if (has_raster(x))
		x = st_xy2sfc(x, as_points = as_points, ..., na.rm = na.rm)

	if (! has_sfc(x))
		stop("no feature geometry column found")

	if (long)
		st_as_sf(as.data.frame(x), crs = crs)
	else {
		ix = which_sfc(x)
		if (length(ix) > 1)	
			warning("working on the first sfc dimension only") # FIXME: this probably only works for 2D arrays, now
		sfc = st_dimensions(x)[[ ix[1] ]]$values
		dfs = lapply(x, function(y) as.data.frame(y))
		nc = sapply(dfs, ncol)
		df = do.call(cbind, dfs)

		if (length(unique(names(df))) < ncol(df) && length(names(dfs)) == ncol(df)) # I hate this
			names(df) = names(dfs)
		else { # another exception... time as second dimension
			e = expand_dimensions(x)
			if (length(e[-ix]) == 1 && inherits(e[-ix][[1]], c("Date", "POSIXt", "PCICt")))
				names(df) = as.character(e[-ix][[1]])
		}

		st_sf(df, geometry = sfc, crs = crs)
	}
}


#' @export
st_as_stars.sfc = function(.x, ..., FUN = length, as_points = TRUE) {
	st = st_as_stars(st_bbox(.x), ...)
	sfc = st_as_sfc(st, as_points = as_points)
	i = st_intersects(sfc, .x)
	vals = sapply(i, FUN)
	st[[1]] = array(vals, dim(st[[1]]))
	st
}


#' @name st_as_stars
#' @export
st_as_stars.sf = function(.x, ...) {
	geom = st_geometry(.x)
	if (length(list(...)))
		stop("secondary arguments ignored")
	dimensions = create_dimensions(list(sfc = 
			create_dimension(1, length(geom), refsys = st_crs(geom)$proj4string, values = geom)))
	lst = lapply(st_set_geometry(.x, NULL), function(x) { dim(x) = length(geom); x })
	st_as_stars(lst, dimensions = dimensions)
}
