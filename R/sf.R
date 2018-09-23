# sf conversion things

#' @export
#' @name st_as_sf
st_as_sfc.stars = function(x, ..., as_points, which = seq_len(prod(dim(x)[1:2]))) {

	r = attr(st_dimensions(x), "raster")
	gt = get_geotransform(x)
	st_as_sfc(st_dimensions(x)[r$dimensions], ..., as_points = as_points, which = which, geotransform = gt)
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

	xy_pos = match(c("x", "y"), names(d)) # FIXME: hard coded raster dims
	if (! all(xy_pos == 1:2))
		stop("x and y need to be first and second dimension")

	# find which records are NA for all attributes:
	a = abind(x, along = length(dim(x)) + 1)
	keep = if (na.rm)
			as.vector(apply(a, c(1,2), function(x) !all(is.na(x))))
		else
			rep(TRUE, prod(dim(x)[1:2]))

	# flatten two dims x,y to one dim sfc (replacing "x")
	sfc = st_as_sfc(x, as_points = as_points, ..., which = which(keep))
	# overwrite x:
	d[["x"]] = create_dimension(from = 1, to = length(sfc), values = sfc)
	# rename x to sfc:
	names(d)[names(d) == "x"] = "sfc"
	# remove y:
	d[["y"]] = NULL
	# flatten arrays:
	for (i in seq_along(x))
		dim(x[[i]]) = c(sfc = length(keep), olddim[-xy_pos]) 
	# reduce arrays to non-NA cells:
	if (na.rm) {
		for (i in seq_along(x))
			x[[i]] = switch(as.character(length(dim(x[[i]]))), 
				"1" = x[[i]][which(keep),drop=FALSE],
				"2" = x[[i]][which(keep),,drop=FALSE],
				"3" = x[[i]][which(keep),,,drop=FALSE],
				"4" = x[[i]][which(keep),,,,drop=FALSE], # etc -- FIXME: use tidy eval here
				)
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
		use_integer = is.logical(x[[1]]) || is.integer(x[[1]])) { 

	if (merge) {
		if (as_points)
			stop("for polygonizing using GDAL_Polygonize, you need to set as_points = FALSE")
		mask = if(na.rm) {
				mask = x[1]
				mask[[1]] = !is.na(mask[[1]])
				mask
			} else
				NULL
		return(gdal_polygonize(x, mask, use_integer = use_integer, geotransform = get_geotransform(x)))
	}

	if (has_raster(x))
		x = st_xy2sfc(x, as_points = as_points, ..., na.rm = na.rm)

	if (! has_sfc(x))
		stop("no feature geometry column found")

	# FIXME: this probably only works for 2D arrays, now
	sfc = st_dimensions(x)$sfc$values
	# may choose units method -> is broken; drop units TODO: if fixed:
	dfs = lapply(x, function(y) as.data.frame(y))
	nc = sapply(dfs, ncol)
	df = do.call(cbind, dfs)
	if (length(dim(x)) > 1) {
		if (length(unique(names(df))) == 1) {
			labels = format(expand_dimensions(st_dimensions(x))[[2]])
			names(df) = if (length(labels) == ncol(df))
					labels
				else
					apply(expand.grid(labels, names(x))[,2:1], 1, paste0, collapse = " ")
		}
	} else
		names(df) = names(x)
	st_sf(df, geometry = sfc)
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

#' @name st_as_stars
#' @export
#' @param sf object of class \code{sf}
#' @param template stars object with desired target geometry 
#' @param file temporary file name
#' @param driver driver for temporary file
#' @param options character; options vector for \code{GDALRasterize}
#' @examples
#' demo(nc, echo = FALSE, ask = FALSE)
#' (x = stars:::st_rasterize(nc)) # default grid:
#' plot(x, axes = TRUE)
#' # a bit more customized grid:
#' (x = stars:::st_rasterize(nc, st_as_stars(st_bbox(nc), nx = 100, ny = 50, values = NA_real_)))
#' plot(x, axes = TRUE)
#' (ls = st_sf(a = 1:2, st_sfc(st_linestring(rbind(c(0.1,0), c(1.1,1))), st_linestring(rbind(c(0, 0.05),c(1, 0.05))))))
#' (grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0,1.0), ylim = c(0,1), values = NA_real_))
#' # the following two plots suggests a half-gridcell-shift problem:
#' sf_extSoftVersion()["GDAL"]
#' plot(st_rasterize(ls, grd), axes=TRUE, reset = FALSE) # ALL_TOUCHED=FALSE; 
#' plot(ls, add = TRUE, col = 'red')
#' plot(st_rasterize(ls, grd, options = "ALL_TOUCHED=TRUE"), axes=TRUE, reset = FALSE)
#' plot(ls, add = TRUE, col = 'red')
#' # add lines to existing 0 values, summing values in case of multiple lines:
#' (grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0,1.0), ylim = c(0,1), values = 0))
#' r = st_rasterize(ls, grd, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE"))
#' plot(r, axes=TRUE, reset = FALSE)
#' plot(ls, add = TRUE, col = 'red')
st_rasterize = function(sf, template = st_as_stars(st_bbox(sf), values = NA_real_), 
		file = tempfile(), driver = "GTiff", options = character(0)) {
	gdal_rasterize(sf, template, get_geotransform(template), file, driver, options)
	setNames(read_stars(file, driver = driver), names(sf)[1])
}
