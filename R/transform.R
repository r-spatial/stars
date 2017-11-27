# create a target grid from x and crs, determining cellsize if unknown
# return a dimensions object
create_target_grid = function(x, crs, cellsize = NA_real_, segments = NA) {
	bb_x = st_bbox(x)
	envelope = st_as_sfc(bb_x)
	if (! is.na(segments))
		envelope = st_segmentize(envelope, st_length(envelope)/segments)
	envelope_new = st_transform(envelope, crs)
	bb = st_bbox(envelope_new) # in new crs
	if (is.na(cellsize)) {
		area = if (st_is_longlat(crs)) # we need a cell size in degree lon lat
				diff(bb[c("xmin", "xmax")]) * diff(bb[c("ymin", "ymax")])
			else 
				st_area(envelope_new)
		ratio = if (has_affine(x)) {
				d = st_dimensions(x)
				xy = xy_from_colrow(rbind(c(0,0), c(1,1)), d$x$geotransform)
				cellarea = sum((xy[1,] - xy[2,])^2) / 2 # lenght is cell diagonal
				unclass(st_area(envelope)) / (prod(dim(x)[c("x","y")]) * cellarea) # > 1
			} else
				1.0
		cellsize = sqrt(unclass(area)/prod(dim(x)[c("x", "y")])/ratio)
		# TODO: divide by st_area(evelope_new)/st_area(envelope) ?
	}
	cellsize = rep(abs(cellsize), length.out = 2)
	gt = c(
		bb["xmin"],
		cellsize[1],
		0,
		bb["ymax"],
		0,
		-cellsize[2])
	p4s = crs$proj4string
	nx = ceiling(diff(bb[c("xmin", "xmax")])/cellsize[1]) 
	ny = ceiling(diff(bb[c("ymin", "ymax")])/cellsize[2])
	x = create_dimension(from = 1, to = nx, offset = bb["xmin"], 
		delta = cellsize[1], geotransform = gt, refsys = p4s)
	y = create_dimension(from = 1, to = ny, offset = bb["ymax"],
		delta = -cellsize[2], geotransform = gt, refsys = p4s)
	structure(list(x = x, y = y), class = "dimensions")
}

# transform grid x to dimensions target
transform_grid_grid = function(x, target) {
	#new_pts = st_as_sfc(target, as_points = TRUE)
	new_pts = st_coordinates(target)
	pts = sf_project(target$x$refsys, st_crs(x)$proj4string, new_pts)
	# at xy (target) locations, get values from x, or put NA
	# to array:
	d = st_dimensions(x)
	# get col/row from x/y:
	xy = ceiling(xy_from_colrow(pts, d$x$geotransform, inverse = TRUE)) 
	xy[ xy[,1] < 1 | xy[,1] > d$x$to | xy[,2] < 1 | xy[,2] > d$y$to, ] = NA

	from = x[[1]] #[,,1]
	dims = dim(x)
	index = matrix(1:prod(dims[c("x","y")]), dims["x"], dims["y"])[xy]
	if (length(dims) > 2) {
		remaining_dims = dims[setdiff(names(dims), c("x", "y"))]
		newdim = c(prod(dims[c("x", "y")]), prod(remaining_dims))
		for (i in seq_along(x)) {
			dim(x[[i]]) = newdim
			x[[i]] = x[[i]][index,]
			dim(x[[i]]) = c(dim(target)[c("x", "y")], remaining_dims)
		}
	} else {
		for (i in seq_along(x)) {
			x[[i]] = x[[i]][index]
			dim(x[[i]]) = dim(target)
		}
	}
	d[c("x", "y")] = target
	structure(x, dimensions = d)
}

transform_raster = function(x, crs, ..., cellsize = NA_real_, segments = NA) {
	# the hard part
	if (inherits(crs, "crs")) {
		if (st_crs(x) == crs)
			return(x) # do nothing!
		target = create_target_grid(x, crs, cellsize = cellsize, segments = segments)
	} else if (inherits(crs, "stars")) {
		stopifnot(has_raster(crs))
		target = crs
	} else
		stop("crs needs to be of class crs or have the target grid, of class stars")
	transform_grid_grid(x, target)
}

transform_sfc = function(x, crs, ...) {
	d = st_dimensions(x)
	d$sfc$values = st_transform(d$sfc$values, crs, ...)
	structure(x, dimensions = d)
}

#' transform features, or warp/resample grids in stars objects to a new coordinate reference system
#'
#' @param x object of class \code{stars}, with either raster or simple feature geometries
#' @param crs object of class \code{crs} with target crs, or object of class \code{stars} with target grid
#' @param cellsize cellsize in target coordinate reference system
#' @param segments number of (total) segments to segmentize the bounding box before transforming to new crs
#' @param ... passed on
#' @examples
#' geomatrix = system.file("tif/geomatrix.tif", package = "stars")
#' (x = st_stars(geomatrix))
#' new = st_crs(4326)
#' y = st_transform(x, new)
#' plot(st_transform(st_as_sfc(st_bbox(x)), new), col = NA, border = 'red')
#' plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
#' image(y, add = TRUE)
#' plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
#' plot(st_transform(st_as_sfc(x, as_points=FALSE), new), add = TRUE)
#' @details For simple feature dimensions, \link[sf]{st_transform} is
#' called, leading to lossless transformation. For gridded spatial data
#' (dimensions \code{x} and \code{y}), see figure; the existing grid
#' is transformed into a regular grid in the new coordinate reference
#' system, using the same procedure as \link[raster]{projectRaster}
#' (currently only with \code{method='ngb')). This entails: (i) the
#' envelope (bounding box polygon) is transformed into the new crs,
#' possibly after segmentation (red box); (ii) a grid is formed in the
#' new crs, touching the transformed envelope on its East and North
#' side, (if cellsize is not given) with a cellsize similar to the
#' origin cell size, with an extent that at least covers \code{x}; (iii)
#' for each cell center of this new grid, the matching grid cell of
#' \code{x} is used; if there is no match, an \code{NA} value is used.
#' @export
st_transform.stars =  function(x, crs, ..., cellsize = NA_real_, segments = NA_integer_) {

	if (!inherits(crs, "crs") && !inherits(crs, "stars"))
		crs = st_crs(crs)

	d = st_dimensions(x)

	if (has_raster(x)) # raster:
		x = transform_raster(x, crs, ..., cellsize = cellsize, segments = segments)

	if (has_sfc(x))
		x = transform_sfc(x, crs, ...)

	x
}
