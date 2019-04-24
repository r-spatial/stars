to_curvilinear = function(x) {
	if (! is_curvilinear(x)) {
		xy = attr(st_dimensions(x), "raster")$dimensions
		nx = dim(x)[ xy[1] ]
		ny = dim(x)[ xy[2] ]
		cl = if (has_rotate_or_shear(x)) {
			pts = xy_from_colrow(as.matrix(expand.grid(seq_len(nx), seq_len(ny))) - 0.5,
				get_geotransform(x))
			list(matrix(pts[,1], nx, ny), matrix(pts[,2], nx, ny))
		} else {
			ed = expand_dimensions(x)
			list(matrix(ed[[ xy[1] ]], nx, ny), matrix(ed[[ xy[2] ]], nx, ny, byrow = TRUE))
		}
		st_as_stars(x, curvilinear = setNames(cl, xy), crs = st_crs(x))
	} else
		x
}

transform_curvilinear = function(x, crs, ...) {
	if(is.numeric(crs))
		crs = st_crs(crs)  	 # nocov
	
	if(inherits(crs, "crs"))
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


#' transform features, or warp/resample grids in stars objects to a new coordinate reference system
#'
#' @name st_transform
#' @param x object of class \code{stars}, with either raster or simple feature geometries
#' @param crs object of class \code{crs} with target crs
#' @param ... ignored
#' @examples
#' geomatrix = system.file("tif/geomatrix.tif", package = "stars")
#' (x = read_stars(geomatrix))
#' new = st_crs(4326)
#' y = st_transform(x, new)
#' plot(st_transform(st_as_sfc(st_bbox(x)), new), col = NA, border = 'red')
#' plot(st_as_sfc(y, as_points=FALSE), col = NA, border = 'green', axes = TRUE, add = TRUE)
#' image(y, col = heat.colors(12), add = TRUE)
#' plot(st_as_sfc(y, as_points=TRUE), pch=3, cex=.5, col = 'blue', add = TRUE)
#' plot(st_transform(st_as_sfc(x, as_points=FALSE), new), add = TRUE)
#' @seealso \link{st_warp}
#' @details For simple feature dimensions, \link[sf]{st_transform} is called, leading to lossless transformation. For gridded spatial data, a curvilinear grid with transformed grid cell (centers) is returned. To convert this to a regular grid in the new \code{CRS}, use \link{st_warp}.
#' @export
st_transform.stars =  function(x, crs, ...) {
	if (has_sfc(x)) {
		if (!inherits(crs, "crs") && !inherits(crs, "stars"))
			crs = st_crs(crs) # needed for GDAL's transform of features
		d = st_dimensions(x)
		ix = which_sfc(x)
		for (j in ix) {
			d[[j]]$values = st_transform(d[[j]]$values, crs, ...)
			d[[j]]$refsys = crs$proj4string
		}
		structure(x, dimensions = d)
	} else {
		if (! has_raster(x)) {
			warning("no spatial coordinates present: st_transform does nothing")
			x
		} else
			transform_curvilinear(to_curvilinear(x), crs)
	}
}

#' @name st_transform
st_transform_proj.stars =  function(x, crs, ...) {

	if (inherits(crs, "crs"))
		crs = crs$proj4string
	if (has_sfc(x)) {
    	if (!requireNamespace("lwgeom", quietly = TRUE))
        	stop("package lwgeom required, please install it first") # nocov
		d = st_dimensions(x)
		ix = which_sfc(x)
		for (j in ix) {
			d[[j]]$values = lwgeom::st_transform_proj(d[[j]]$values, crs, ...)
			d[[j]]$refsys = crs
		}
		structure(x, dimensions = d)
	} else {
		if (! has_raster(x)) {
			warning("no spatial coordinates present: st_transform_proj does nothing")
			x
		} else
			transform_curvilinear(to_curvilinear(x), crs)
	}
}
