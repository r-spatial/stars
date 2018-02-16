
#' @name st_as_stars
#' @export
st_as_stars.Raster = function(x, ...) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first")
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first")
	#0 360 -90  90
	e = as.vector(raster::extent(x)) # xmin xmax ymin ymax
	v = raster::values(x)
	dim(v) = dim(x)[c(2,1,3)]
	gt = c(e[1], (e[2]-e[1])/dim(v)[1], 0, e[4], 0, (e[3]-e[4])/dim(v)[2])
	dimensions = list()
	dimensions$x = create_dimension(from = 1, to = dim(v)[1], offset = e[1], 
		delta = (e[2]-e[1])/dim(v)[1], geotransform = gt, refsys = sp::proj4string(x))
	dimensions$y = create_dimension(from = 1, to = dim(v)[2], offset = e[4],
		delta = (e[3]-e[4])/dim(v)[2], geotransform = gt, refsys = sp::proj4string(x))
	z = raster::getZ(x)
	dimensions$band = if (is.null(z))
			create_dimension(values = names(x))
		else
			create_dimension(values = z)
	st_stars(list(v), dimensions = structure(dimensions, class = "dimensions"))
}

st_as_raster = function(x, ...) {
	stopifnot(inherits(x, "stars"))
	stopifnot(length(dim(x)) %in% c(2, 3))
	d = st_dimensions(x)
	stopifnot(all(c("x", "y") %in% names(d)))
	bb = st_bbox(x)
	if (length(dim(x)) == 2) {
    	raster::raster(nrows=dim(x)["y"], ncols=dim(x)["x"],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], 
            crs = st_crs(x)$proj4string, vals = as.vector(x[[1]]))
	} else {
		third = setdiff(names(d), c("x", "y"))
		b = raster::brick(nrows=dim(x)["y"], ncols=dim(x)["x"],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], nl = dim(x)[third],
            crs = st_crs(x)$proj4string)
		raster::values(b) = as.vector(x[[1]])
		raster::setZ(b, seq(d[[third]]))
	}
}

#' Coerce stars object into a Raster raster or brick
#' 
#' Coerce stars object into a Raster raster or brick
#' @param from object to coerce
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,stars,Raster-method
setAs("stars", "Raster", function(from) { 
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first")
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first")
	st_as_raster(from)
})
