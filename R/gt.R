get_geotransform = function(x) {
	r = attr(x, "raster")
	if (is.null(r))
		rep(NA_real_, 6)
	else {
		xd = x[[ r$dimensions[1] ]]
		yd = x[[ r$dimensions[2] ]]
		as.numeric(c(xd$offset, xd$delta, r$affine[1], yd$offset, r$affine[2], yd$delta))
	}
}

#' get or set the geotransform, or rotation matrix
#' @param x object of class stars or dimensions
#' @param ... ignored
#' @export
st_geotransform = function(x, ...) UseMethod("st_geotransform")

#' @export
st_geotransform.default = function(x, ...) {
	stop(paste("no st_geotransform method available for objects of class", class(x)[1]))
}

#' @export
st_geotransform.stars = function(x, ...) st_geotransform(st_dimensions(x))

#' @export
st_geotransform.dimensions = function(x, ...) get_geotransform(x)


#' @export
#' @name st_geotransform
#' @param value length 6 numeric vector, or 2 x 2 (scaled) rotation matrix
`st_geotransform<-` = function(x, value) UseMethod("st_geotransform<-")

#' @export
#' @name st_geotransform
#' @examples
#' # using the "classical" rotation matrix, see https://en.wikipedia.org/wiki/Rotation_matrix :
#' rot = function(theta, dxdy = c(1., -1.)) {
#'    th = theta / 180 * pi
#'    matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2, 2) %*% 
#' 		   matrix(c(dxdy[2], 0, 0, dxdy[1]), 2, 2)
#' }
#' l = st_downsample(st_as_stars(L7_ETMs), 9) # save time in plotting
#' st_geotransform(l) = rot(20, c(28.5, 28.5)) # clockwise, 20 degrees, scale by cell size
#' plot(l[,,,1])
#' m = rot(20, c(1, 2))
#' g = expand.grid(x = 0:4, y = 0:4)
#' plot(g[1:2])
#' text(g[,1], g[,2], labels = seq_along(g[,1]), pos = 4)
#' g = t(m %*% t(as.matrix(g)))
#' points(g, col = 'red')
#' text(g[,1], g[,2], labels = seq_along(g[,1]), pos = 4, col = 'red')
#'
#' m = matrix(1:20, 4)
#' s0 = st_as_stars(m)
#' s = s0
#' # dy>0, clockwise rotation:
#' st_geotransform(s) = rot(10, c(1,1))
#' plot(s0, reset = FALSE)
#' plot(s, add = TRUE)
#' # dy<0, counter clockwise rotation, + expansion in x-direction:
#' layout(1)
#' s0 = st_as_stars(st_bbox(s0), dx = 1)
#' s0$values = 1:20
#' s0
#' plot(s0, reset = FALSE)
#' s = s0
#' st_geotransform(s) = rot(10, c(2,1))
#' plot(s, add = TRUE)
`st_geotransform<-.stars` = function(x, value) {
	d = st_dimensions(x)
	r = attr(d, "raster")
	# https://gdal.org/tutorials/geotransforms_tut.html
	if (is.matrix(value)) { # gdal has col-row (pixel-line) order, where linear algebra uses row-col
		stopifnot(all(dim(value) == c(2, 2)))
		r$affine = c(value[2,1] * sign(d[[ r$dimensions[1] ]]$delta), 
					 value[1,2] * sign(d[[ r$dimensions[2] ]]$delta))
		d[[ r$dimensions[1] ]]$delta = value[2,2] * sign(d[[ r$dimensions[1] ]]$delta)
		d[[ r$dimensions[2] ]]$delta = value[1,1] * sign(d[[ r$dimensions[2] ]]$delta)
	} else {
		stopifnot(is.numeric(value), length(value) == 6)
		d[[ r$dimensions[1] ]]$offset = value[1]
		d[[ r$dimensions[1] ]]$delta  = value[2]
		r$affine[1]                   = value[3]
		d[[ r$dimensions[2] ]]$offset = value[4]
		r$affine[2]                   = value[5]
		d[[ r$dimensions[2] ]]$delta  = value[6]
	}
	attr(d, "raster") = r
	st_stars(x, d)
}
