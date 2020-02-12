has_global_longitude = function(x) {
	st_is_longlat(x) && isTRUE(all.equal(as.numeric(st_bbox(x))[c(1,3)], c(-180,180)))
}

cut_latitude_to_3857 = function(bb) { # Pseudomercator does not have global coverage:
	bb[2] = max(bb[2], -85.06)
	bb[4] = min(bb[4],  85.06)
	bb
}

# create a target grid from x and crs, determining cellsize if unknown
# return a dimensions object
default_target_grid = function(x, crs, cellsize = NA_real_, segments = NA) {
	bb_x = st_bbox(x)
	if (st_is_longlat(x) && crs == st_crs(3857)) # common case:
		bb_x = cut_latitude_to_3857(bb_x)
	envelope = st_as_sfc(bb_x)
	# global adjustment: needed to have st_segmentize span global extent
	# https://github.com/r-spatial/mapview/issues/256
	if (!is.na(segments) && !has_global_longitude(x))
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
	if (has_global_longitude(x)) { # if global coverage, don't cross the boundaries:
		cellsize[1] = diff(bb[c("xmin", "xmax")])/nx
		cellsize[2] = diff(bb[c("ymin", "ymax")])/ny
	}
	x = create_dimension(from = 1, to = nx, offset = bb["xmin"], delta =  cellsize[1], refsys = p4s)
	y = create_dimension(from = 1, to = ny, offset = bb["ymax"], delta = -cellsize[2], refsys = p4s)
	create_dimensions(list(x = x, y = y), get_raster())
}

# transform grid x to dimensions target
# x is a stars object, target is a dimensions object
transform_grid_grid = function(x, target) {
	stopifnot(inherits(target, "dimensions"))
	xy_names = attr(target, "raster")$dimensions
	new_pts = st_coordinates(target[xy_names])
	dxy = attr(target, "raster")$dimensions

	from = if (inherits(target[[ dxy[1] ]]$refsys, "crs")) # FIXME: drop support for this?
			target[[ dxy[1] ]]$refsys$proj4string
		else
			target[[ dxy[1] ]]$refsys

	pts = sf_project(from = from, to = st_crs(x)$proj4string, pts = new_pts)

	# at xy (target) locations, get values from x, or put NA
	# to array:
	d = st_dimensions(x)
	# get col/row from x/y:
	xy = colrow_from_xy(pts, x, NA_outside = TRUE)

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
	st_stars(x, dimensions = create_dimensions(d, attr(target, "raster")))
}


#' Warp (resample) grids in stars objects to a new grid, possibly in an new coordinate reference system
#'
#' @param src object of class \code{stars} with source raster
#' @param dest object of class \code{stars} with target raster geometry
#' @param crs coordinate reference system for destination grid, only used when \code{dest} is missing 
#' @param cellsize cellsize in target coordinate reference system
#' @param segments (total) number of segments for segmentizing the bounding box before transforming to the new crs
#' @param use_gdal logical; if \code{TRUE}, use gdalwarp, through \link[sf]{gdal_utils}
#' @param options character vector with options, passed on to gdalwarp
#' @param no_data_value value used by gdalwarp for no_data (NA) when writing to temporaray file
#' @param debug logical; if \code{TRUE}, do not remove the temporary gdalwarp destination file, and print its name
#' @param method character; see details for options; methods other than \code{near} only work when \code{use_gdal=TRUE} 
#' @param ... ignored
#' @details \code{method} should be one of \code{near}, \code{bilinear}, \code{cubic}, \code{cubicspline}, \code{lanczos}, \code{average}, \code{mode}, \code{max}, \code{min}, \code{med}, \code{q1} or \code{q3}; see https://github.com/r-spatial/stars/issues/109
#' @examples
#' geomatrix = system.file("tif/geomatrix.tif", package = "stars")
#' (x = read_stars(geomatrix))
#' new_crs = st_crs(4326)
#' y = st_warp(x, crs = new_crs)
#' plot(st_transform(st_as_sfc(st_bbox(x)), new_crs), col = NA, border = 'red')
#' plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
#' image(y, add = TRUE, nbreaks = 6)
#' plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
#' plot(st_transform(st_as_sfc(x, as_points=FALSE), new_crs), add = TRUE)
#' @details For gridded spatial data (dimensions \code{x} and \code{y}), see figure; the existing grid is transformed into a regular grid defined by \code{dest}, possibly in a new coordinate reference system. If \code{dest} is not specified, but \code{crs} is, the procedure used to choose a target grid is similar to that of \link[raster]{projectRaster} (currently only with \code{method='ngb'}). This entails: (i) the envelope (bounding box polygon) is transformed into the new crs, possibly after segmentation (red box); (ii) a grid is formed in this new crs, touching the transformed envelope on its East and North side, with (if cellsize is not given) a cellsize similar to the cell size of \code{src}, with an extent that at least covers \code{x}; (iii) for each cell center of this new grid, the matching grid cell of \code{x} is used; if there is no match, an \code{NA} value is used.
#' @export
st_warp = function(src, dest, ..., crs = NA_crs_, cellsize = NA_real_, segments = 100, 
		use_gdal = FALSE, options = character(0), no_data_value = NA_real_, debug = FALSE,
		method = "near") {

	if (!inherits(src, "stars_proxy"))
		src = st_normalize(src)

	if (!is.na(crs))
		crs = st_crs(crs)

	if (use_gdal) {
		options = c(options, "-dstnodata", no_data_value, "-r", method)
		if (! inherits(src, "stars_proxy")) {
			src = st_as_stars_proxy(src, NA_value = no_data_value)
			if (debug)
				cat("Writing input to: ", src[[1]], "\n")
			else
				on.exit(unlink(src[[1]])) # temp file
		}
		if (missing(dest) && is.na(crs)) {
			delete = TRUE
			dest = tempfile(fileext = ".tif")
			sf::gdal_utils("warp", src[[1]], dest, options = options)
		} else {  # dest exists, and should be used: should use warper rather than warp
			dest = if (! inherits(dest, "stars_proxy")) {
					dest[[1]] = NA_real_ * dest[[1]] # blank out values
					delete = !debug
					st_as_stars_proxy(dest, NA_value = no_data_value)[[1]]
				} else {
					delete = FALSE
					dest[[1]] # the file name of the stars_proxy, not to be deleted
				}
			sf::gdal_utils("warper", src[[1]], dest, options = method) # not "warp"!
		}
		if (debug)
			cat("Writing result to: ", dest, "\n")
		else if (delete)
			on.exit(unlink(dest)) # a temp file
		read_stars(dest)
	} else {
		if (missing(dest)) {
			if (is.na(crs))
				stop("either dest or crs should be specified")
			dest = default_target_grid(src, crs = crs, cellsize = cellsize, segments = segments)
		} else if (!inherits(dest, "stars") && !inherits(dest, "dimensions"))
			stop("dest should be a stars object, or a dimensions object")
		transform_grid_grid(st_as_stars(src), st_dimensions(dest))
	}
}
