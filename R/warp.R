has_global_longitude = function(x) {
	st_is_longlat(x) && isTRUE(all.equal(as.numeric(st_bbox(x)$xlim), c(-180.,180.)))
}

cut_latitude_to_3857 = function(bb) { # Pseudomercator does not have global coverage:
	bb["ymin"] = max(bb["ymin"], -85.06)
	bb["ymax"] = min(bb["ymax"],  85.06)
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
	envelope = if (!is.na(segments) && !has_global_longitude(x)) # FIXME: should this branch be retained?
				st_segmentize(envelope, st_length(st_cast(envelope, "LINESTRING"))/segments)
			else {
				# https://github.com/mtennekes/tmap/issues/526 :
				old_crs = st_crs(envelope)
				st_crs(envelope) = NA_crs_
				st_set_crs(st_segmentize(envelope, st_length(st_cast(envelope, "LINESTRING"))/segments), old_crs)
			}
	envelope_new = st_transform(envelope, crs)
	bb = st_bbox(envelope_new) # in new crs
	if (any(is.na(cellsize))) {
		area = if (st_is_longlat(crs)) # we need a cell size in degree lon lat
				diff(bb[c("xmin", "xmax")]) * diff(bb[c("ymin", "ymax")])
			else
				st_area(envelope_new)
		ratio = if (has_rotate_or_shear(x)) {
				d = st_dimensions(x)
				xy = xy_from_colrow(rbind(c(0,0), c(1,1)), st_geotransform(d))
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
	nx = ceiling(diff(bb[c("xmin", "xmax")])/cellsize[1])
	ny = ceiling(diff(bb[c("ymin", "ymax")])/cellsize[2])
	if (has_global_longitude(x)) { # if global coverage, don't cross the boundaries:
		cellsize[1] = diff(bb[c("xmin", "xmax")])/nx
		cellsize[2] = diff(bb[c("ymin", "ymax")])/ny
	}
	x = create_dimension(from = 1, to = nx, offset = bb["xmin"], delta =  cellsize[1], refsys = crs)
	y = create_dimension(from = 1, to = ny, offset = bb["ymax"], delta = -cellsize[2], refsys = crs)
	create_dimensions(list(x = x, y = y), get_raster())
}

rename_xy_dimensions = function(x, dims) {
	#stopifnot(inherits(x, "stars"), inherits(dims, "dimensions"))
	dx = st_dimensions(x)
	xxy = attr(dx, "raster")$dimensions
	dimsxy = attr(dims, "raster")$dimensions
	if (all(xxy == dimsxy))
		x
	else {
		na = names(dx)
		names(dx)[match(xxy, na)] = dimsxy
		attr(dx, "raster")$dimensions = dimsxy
		structure(x, dimensions = dx)
	}
}

# transform grid x to dimensions target
# x is a stars object, target is a dimensions object
transform_grid_grid = function(x, target, threshold) {
	stopifnot(inherits(x, "stars"), inherits(target, "dimensions"))
	x = rename_xy_dimensions(x, target) # so we can match by name
	xy_names = attr(target, "raster")$dimensions
	new_pts = st_coordinates(target[xy_names])
	dxy = attr(target, "raster")$dimensions

	from = st_crs(target)
	pts = sf::sf_project(from = from, to = st_crs(x), pts = new_pts)

	# at xy (target) locations, get values from x, or put NA
	# to array:
	d = st_dimensions(x)
	# get col/row from x/y:
	xy = if (is_curvilinear(x)) {
    		if (!requireNamespace("FNN", quietly = TRUE))
        		stop("package FNN required, please install it first") #nocov
			if (st_is_longlat(x))
				warning("using Euclidean distance measures on geodetic coordinates")
			fnn = FNN::get.knnx(cc_x <- st_coordinates(x)[, 1:2, drop = FALSE], pts, 1)
			if (is.na(threshold)) {
				p12 = st_as_sf(as.data.frame(cc_x[1:2,]), coords = 1:2)
				threshold = signif(st_distance(p12)[1,2])
				message(paste("threshold set to", format(threshold), 
					 ": set a larger value if you see missing values where they shouldn't be"))
			}
			i = fnn$nn.index - 1
			i[fnn$nn.dist > threshold] = NA
			ny = dim(x)[1]
			cbind(i %% ny, i %/% ny) + 1
		} else
			colrow_from_xy(pts, x, NA_outside = TRUE)
	dims = dim(x)
	index = matrix(seq_len(prod(dims[dxy])), dims[ dxy[1] ], dims[ dxy[2] ])[xy]
	x = unclass(x) # avoid using [[<-.stars:
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
#' @param cellsize length 1 or 2 numeric; cellsize in target coordinate reference system units
#' @param segments (total) number of segments for segmentizing the bounding box before transforming to the new crs
#' @param use_gdal logical; if \code{TRUE}, use gdal's warp or warper, through \link[sf]{gdal_utils}
#' @param options character vector with options, passed on to gdalwarp
#' @param no_data_value value used by gdalwarp for no_data (NA) when writing to temporary file; 
#'  not setting this when \code{use_gdal} is \code{TRUE} leads to a warning
#' @param debug logical; if \code{TRUE}, do not remove the temporary gdalwarp destination file, and print its name
#' @param method character; see details for options; methods other than \code{near} only work when \code{use_gdal=TRUE}
#' @param threshold numeric; distance threshold for warping curvilinear grids: new cells at distances larger than threshold are assigned NA values.
#' @param ... ignored
#' @details \code{method} should be one of \code{near}, \code{bilinear}, \code{cubic}, \code{cubicspline}, \code{lanczos}, \code{average}, \code{mode}, \code{max}, \code{min}, \code{med}, \code{q1} or \code{q3}; see https://github.com/r-spatial/stars/issues/109
#' @examples
#' geomatrix = system.file("tif/geomatrix.tif", package = "stars")
#' (x = read_stars(geomatrix))
#' new_crs = st_crs('OGC:CRS84')
#' y = st_warp(x, crs = new_crs)
#' plot(st_transform(st_as_sfc(st_bbox(x)), new_crs), col = NA, border = 'red')
#' plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
#' image(y, add = TRUE, nbreaks = 6)
#' plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
#' plot(st_transform(st_as_sfc(x, as_points=FALSE), new_crs), add = TRUE)
#' # warp 0-360 raster to -180-180 raster:
#' r = read_stars(system.file("nc/reduced.nc", package = "stars"))
#' r %>% st_set_crs('OGC:CRS84') %>% st_warp(st_as_stars(st_bbox(), dx = 2)) -> s
#' plot(r, axes = TRUE) # no CRS set, so no degree symbols in labels
#' plot(s, axes = TRUE)
#' # downsample raster (90 to 270 m)
#' r = read_stars(system.file("tif/olinda_dem_utm25s.tif", package = "stars"))
#' r270 = st_as_stars(st_bbox(r), dx = 270)
#' r270 = st_warp(r, r270)
#' @details For gridded spatial data (dimensions \code{x} and \code{y}), see figure; the existing grid is transformed into a regular grid defined by \code{dest}, possibly in a new coordinate reference system. If \code{dest} is not specified, but \code{crs} is, the procedure used to choose a target grid is similar to that of \link[raster]{projectRaster}. This entails: (i) the envelope (bounding box polygon) is transformed into the new crs, possibly after segmentation (red box); (ii) a grid is formed in this new crs, touching the transformed envelope on its East and North side, with (if cellsize is not given) a cellsize similar to the cell size of \code{src}, with an extent that at least covers \code{x}; (iii) for each cell center of this new grid, the matching grid cell of \code{x} is used; if there is no match, an \code{NA} value is used.
#' @export
st_warp = function(src, dest, ..., crs = NA_crs_, cellsize = NA_real_, segments = 100,
		use_gdal = FALSE, options = character(0), no_data_value = NA_real_, debug = FALSE,
		method = "near", threshold = NA_real_) {

	if (!inherits(src, "stars_proxy"))
		src = st_normalize(src)

	if (!is.na(crs))
		crs = st_crs(crs)

	if (!missing(dest)) {
		if (inherits(dest, "crs"))
			stop("target crs should be specified with crs = ..., not as argument dest")
	} else if (is.na(crs) && !use_gdal)
		stop("when use_gdal is FALSE, either dest or crs need to be specified")

	ret = if (use_gdal) {
		if (is_curvilinear(src))
			stop("for warping curvilinear grids using GDAL, use gdal_utils(\"warp\",...) directly on the source dataset of this object")
		if (!is.na(no_data_value))
			options = c(options, "-dstnodata", no_data_value)
		else 
			warning("no_data_value not set: missing values will appear as zero values")
		options = c(options, "-r", method)
		if (all(!is.na(cellsize))) {
			cellsize = rep(abs(cellsize), length.out = 2)
			options = c(options, "-tr", cellsize[1], cellsize[2])
		}
		if (! inherits(src, "stars_proxy")) {
			src = st_as_stars_proxy(src, NA_value = no_data_value)
			if (debug)
				cat("Writing input to: ", src[[1]], "\n")
			else
				on.exit(unlink(src[[1]])) # temp file
		}
		# collapse a multi-file proxy objects into single-file/multi-band:
		if (length(src[[1]]) > 1 || length(src) > 1) {
			tmp = tempfile(fileext = ".vrt") # multi-band destination
			sf::gdal_utils("buildvrt", unlist(src), tmp, options = "-separate")
			src[[1]] = tmp
		}
		if (missing(dest) && is.na(crs)) {
			delete = TRUE
			dest = tempfile(fileext = ".tif")
			sf::gdal_utils("warp", src[[1]], dest, options = options)
		} else {  # dest exists, and should be used: should use warper rather than warp
			# https://github.com/r-spatial/stars/issues/407
			if (missing(dest)) {
				dest = default_target_grid(src, crs = crs, cellsize = cellsize, segments = segments) # dimensions
				dest = st_stars(list(values = array(NA_real_, dim(dest))), dest)
			}
			if (length(dim(src)) == 3 && length(dim(dest)) == 2)
				dest = merge(do.call(c, lapply(seq_len(dim(src)[3]), function(x) dest)))
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
		if (method != "near")
			stop("methods other than \"near\" are only supported if use_gdal=TRUE")
		if (missing(dest))
			dest = default_target_grid(src, crs = crs, cellsize = cellsize, segments = segments)
		if (!inherits(dest, "stars") && !inherits(dest, "dimensions"))
			stop("dest should be a stars object, or a dimensions object")
		transform_grid_grid(st_as_stars(src), st_dimensions(dest), threshold)
	}
	# restore attributes?
	if (method %in% c("near", "mode")) {
		a = lapply(src, attributes)
		for (i in seq_along(ret))
			ret[[i]] = structure(ret[[i]],
				levels = attr(ret[[i]], "levels") %||% a[[i]]$levels,
				colors = attr(ret[[i]], "colors") %||% a[[i]]$colors,
				class =  attr(ret[[i]], "class")  %||% a[[i]]$class)
	}
	ret
}

rotate = function(lon, lat, lon0, lat0, north = TRUE) {
	# https://gis.stackexchange.com/questions/10808/manually-transforming-rotated-lat-lon-to-regular-lat-lon/14445 , by Miha!
	if (north) {
		lat0 = -lat0
		lon0 = pi + lon0
	}
	vartheta = -(pi/2 + lat0)
	varphi = -lon0
	cbind(
		lon = atan2(sin(lon), tan(lat) * sin(vartheta) + 
				cos(lon) * cos(vartheta)) - varphi,
		lat = asin(cos(vartheta) * sin(lat) - cos(lon) * sin(vartheta) * cos(lat))
	)
}

#' Transform rotated long/lat to regular long/lat
#'
#' Transform rotated long/lat to regular long/lat
#' @param .x object of class \code{stars}
#' @param lon0 longitude of the rotated pole
#' @param lat0 latitude of the rotated pole
#' @param north logical; if \code{TRUE} the pole refers to the North pole, otherwise the South pole
#' @returns curvilinear stars object with coordinates in regular long/lat (North pole at lat=90)
#' @export
#' @examples
#' if (require("starsdata") && require("maps")) {
#'   nc = "netcdf/ts_EUR-6km_ECMWF-ERAINT_REA6_r1i1p1f1_COSMO_v1_mon_201801-201812.nc"
#'   f = system.file(nc, package = "starsdata")
#'   m = read_mdim(f, "ts")
#'   print(m)
#'   # NOTE this function is obsolete when reading m as
#'   # m = read_mdim(f, "ts", curvilinear = c("longitude", "latitude"))
#'   if (require(RNetCDF)) {
#'      x = open.nc(f)
#'      lon = att.get.nc(x, "rotated_latitude_longitude", "grid_north_pole_latitude")
#'      lat = att.get.nc(x, "rotated_latitude_longitude", "grid_north_pole_longitude")
#'      print(c(lon = lon, lat = lat))
#'   } else {
#'      lon = -162
#'      lat = 39.25
#'   } 
#'   m1 = st_rotate(m, lon, lat)
#'   print(m1)
#'   h = function() maps::map(add = TRUE)
#'   plot(m1, downsample = c(10, 10, 5), axes = TRUE, hook = h) # downsample for speed
#' }
st_rotate = function(.x, lon0, lat0, north = TRUE) {
	stopifnot(inherits(.x, "stars"), 
			  is.na(st_crs(.x)) || st_is_longlat(.x), 
			  !is_curvilinear(.x), 
			  is.logical(north), 
			  !is.na(north))
	torad = function(x) x * pi / 180
	todeg = function(x) x * 180 / pi
	d = dim(.x)
	n = prod(d[1:2])
	cc = torad(st_coordinates(.x)[1:n, 1:2])
	cc = todeg(rotate(cc[,1], cc[,2], torad(lon0), torad(lat0), north))
	st_as_stars(.x, curvilinear = setNames(list(matrix(cc[,1], d[1], d[2]),
					  matrix(cc[,2], d[1], d[2])), names(d)[1:2]))
}
