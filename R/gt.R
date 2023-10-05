get_geotransform = function(x) {
	if (inherits(x, "stars"))
		x = st_dimensions(x)
	stopifnot(inherits(x, "dimensions"))
	r = attr(x, "raster")
	if (is.null(r))
		rep(NA_real_, 6)
	else {
		xd = x[[ r$dimensions[1] ]]
		yd = x[[ r$dimensions[2] ]]
		c(as.numeric(xd$offset), as.numeric(xd$delta), r$affine[1], 
		  as.numeric(yd$offset), r$affine[2], as.numeric(yd$delta))
	}
}

#' get or set the geotransform, or rotation matrix
#' @param x object of class stars or dimensions
#' @param ... ignored
#' @export
st_geotransform = function(x, ...) UseMethod("st_geotransform")

#' @export
st_geotransform.default = function(x, ...) {
	get_geotransform(x)
}

#' @export
#' @name st_geotransform
#' @name value length 6 numeric vector, or 2 x 2 (scaled) rotation matrix
`st_geotransform<-` = function(x, value, ...) UseMethod("st_geotransform<-")

#' @export
`st_geotransform<-.stars` = function(x, value, ...) {
	d = st_dimensions(x)
	r = attr(d, "raster")
	if (is.matrix(value)) {
		stopifnot(all(dim(value) == c(2, 2)))
		d[[ r$dimensions[1] ]]$delta = value[1,1]
		d[[ r$dimensions[2] ]]$delta = value[2,2]
		r$affine = c(value[1,2], value[2,1])
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
