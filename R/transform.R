# create a target grid from x and crs, determining cellsize if unknown
# return a dimensions object
create_target_grid = function(x, crs, cellsize = NA_real_, segments = NA) {
	bb_x = st_bbox(x)
	envelope = st_as_sfc(bb_x)
	if (! is.na(segments))
		envelope = st_segmentize(envelope, st_length(st_cast(envelope, "LINESTRING"))/segments)
	envelope_new = st_transform(envelope, crs)
	bb = st_bbox(envelope_new) # in new crs
	if (is.na(cellsize)) {
		area = if (st_is_longlat(crs)) # we need a cell size in degree lon lat
				diff(bb[c("xmin", "xmax")]) * diff(bb[c("ymin", "ymax")])
			else 
				st_area(envelope_new)
		ratio = if (has_rotate_or_shear(x)) {
				d = st_dimensions(x)
				xy = xy_from_colrow(rbind(c(0,0), c(1,1)), get_geotransform(d))
				cellarea = sum((xy[1,] - xy[2,])^2) / 2 # lenght is cell diagonal
				xy_dims = attr(d, "raster")$dimensions
				unclass(st_area(envelope)) / (prod(dim(x)[xy_dims]) * cellarea) # > 1
			} else
				1.0
		dxy = attr(st_dimensions(x), "raster")$dimensions
		cellsize = sqrt(unclass(area)/prod(dim(x)[dxy])/ratio)
		# TODO: divide by st_area(evelope_new)/st_area(envelope) ?
	}
	cellsize = rep(abs(cellsize), length.out = 2)
	p4s = crs$proj4string
	nx = ceiling(diff(bb[c("xmin", "xmax")])/cellsize[1]) 
	ny = ceiling(diff(bb[c("ymin", "ymax")])/cellsize[2])
	x = create_dimension(from = 1, to = nx, offset = bb["xmin"], delta =  cellsize[1], refsys = p4s)
	y = create_dimension(from = 1, to = ny, offset = bb["ymax"], delta = -cellsize[2], refsys = p4s)
	create_dimensions(list(x = x, y = y), get_raster())
}

# transform grid x to dimensions target
# x is a stars object, target is a dimensions object
transform_grid_grid = function(x, target) {
	stopifnot(inherits(target, "dimensions"))
	new_pts = st_coordinates(target[1:2])
	dxy = attr(target, "raster")$dimensions
	pts = sf_project(from = target[[ dxy[1] ]]$refsys, to = st_crs(x)$proj4string, pts = new_pts)

	# at xy (target) locations, get values from x, or put NA
	# to array:
	d = st_dimensions(x)
	# get col/row from x/y:
	xy = ceiling(colrow_from_xy(pts, get_geotransform(x)))
	xy[ xy[,1] < 1 | xy[,1] > d[[ dxy[1] ]]$to | xy[,2] < 1 | xy[,2] > d[[ dxy[2] ]]$to, ] = NA

	from = x[[1]] #[,,1]
	dims = dim(x)
	index = matrix(1:prod(dims[dxy]), dims[ dxy[1] ], dims[ dxy[2] ])[xy]
	if (length(dims) > 2) {
		remaining_dims = dims[setdiff(names(dims), dxy)]
		newdim = c(prod(dims[dxy]), prod(remaining_dims))
		for (i in seq_along(x)) {
			dim(x[[i]]) = newdim
			x[[i]] = x[[i]][index,]
			dim(x[[i]]) = c(dim(target)[dxy], remaining_dims)
		}
	} else {
		for (i in seq_along(x)) {
			x[[i]] = x[[i]][index]
			dim(x[[i]]) = dim(target)
		}
	}
	d[dxy] = target[1:2]
	structure(x, dimensions = create_dimensions(d, attr(target, "raster")))
}

transform_curvilinear = function(x, crs, ...) {
	if (inherits(crs, "crs"))
		crs = crs$proj4string
	d = st_dimensions(x)
	xy = attr(d, "raster")$dimensions
	cc = cbind(as.vector(d[[ xy[1] ]]$values), as.vector(d[[ xy[2] ]]$values))
	pts = sf_project(from = d[[ xy[1] ]]$refsys, to = crs, pts = cc)
	d[[ xy[1] ]]$refsys = d[[ xy[2] ]]$refsys = crs
	d[[ xy[1] ]]$values = matrix(pts[,1], dim(x)[xy])
	d[[ xy[2] ]]$values = matrix(pts[,2], dim(x)[xy])
	st_stars(x, d)
}

transform_raster = function(x, crs, ..., cellsize = NA_real_, segments = NA) {
	# the hard part
	target = if (inherits(crs, "crs")) {
			if (st_crs(x) == crs)
				return(x) # do nothing!
			create_target_grid(x, crs, cellsize = cellsize, segments = segments)
		} else if (inherits(crs, "stars")) {
			stopifnot(has_raster(crs))
			st_dimensions(crs)
		} else
			stop("crs needs to be of class crs or have the target grid, of class stars")
	transform_grid_grid(x, target)
}

transform_sfc = function(x, crs, ...) {
	crs = st_crs(crs)
	d = st_dimensions(x)
	ix = which(sapply(d, function(i) inherits(i$values, "sfc")))
	for (j in ix) {
		d[[j]]$values = st_transform(d[[j]]$values, crs, ...)
		d[[j]]$refsys = crs
	}
	structure(x, dimensions = d)
}

#' transform features, or warp/resample grids in stars objects to a new coordinate reference system
#'
#' @name st_transform
#' @param x object of class \code{stars}, with either raster or simple feature geometries
#' @param crs object of class \code{crs} with target crs, or object of class \code{stars} with target grid
#' @param cellsize cellsize in target coordinate reference system
#' @param segments (total) number of segments for segmentizing the bounding box before transforming to the new crs
#' @param make_regular logical; 
#' @param ... passed on
#' @examples
#' geomatrix = system.file("tif/geomatrix.tif", package = "stars")
#' (x = read_stars(geomatrix))
#' new = st_crs(4326)
#' y = st_transform(x, new)
#' plot(st_transform(st_as_sfc(st_bbox(x)), new), col = NA, border = 'red')
#' plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
#' image(y, add = TRUE)
#' plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
#' plot(st_transform(st_as_sfc(x, as_points=FALSE), new), add = TRUE)
#' @details For simple feature dimensions, \link[sf]{st_transform} is called, leading to lossless transformation. For gridded spatial data (dimensions \code{x} and \code{y}), see figure; the existing grid is transformed into a regular grid in the new coordinate reference system, using the same procedure as \link[raster]{projectRaster} (currently only with \code{method='ngb'}). This entails: (i) the envelope (bounding box polygon) is transformed into the new crs, possibly after segmentation (red box); (ii) a grid is formed in the new crs, touching the transformed envelope on its East and North side, (if cellsize is not given) with a cellsize similar to the origin cell size, with an extent that at least covers \code{x}; (iii) for each cell center of this new grid, the matching grid cell of \code{x} is used; if there is no match, an \code{NA} value is used.
#' @export
st_transform.stars =  function(x, crs, ..., cellsize = NA_real_, segments = 100, make_regular = FALSE) {

	if (is_curvilinear(x) && !make_regular)
		return(transform_curvilinear(x, crs))

	if (!inherits(crs, "crs") && !inherits(crs, "stars"))
		crs = st_crs(crs)

	if (has_raster(x)) # raster:
		transform_raster(x, crs, ..., cellsize = cellsize, segments = segments)
	else if (has_sfc(x))
		transform_sfc(x, crs, ...)
	else {
		warning("no spatial coordinates present: st_transform does nothing")
		x
	}
}
