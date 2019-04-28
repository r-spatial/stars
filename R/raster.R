
#' @name st_as_stars
#' @export
st_as_stars.Raster = function(.x, ...) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") # nocov
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first") # nocov
	#0 360 -90  90
	e = as.vector(raster::extent(.x)) # xmin xmax ymin ymax
	v = raster::values(.x)
	dim(v) = dim(.x)[c(2,1,3)]
	dimensions = list(
		x = create_dimension(from = 1, to = dim(v)[1], offset = e[1], 
			delta = (e[2]-e[1])/dim(v)[1], refsys = sp::proj4string(.x)),
		y = create_dimension(from = 1, to = dim(v)[2], offset = e[4],
			delta = (e[3]-e[4])/dim(v)[2], refsys = sp::proj4string(.x)))
	z = raster::getZ(.x)
	dimensions$band = if (is.null(z))
			create_dimension(values = names(.x))
		else
			create_dimension(values = z)
	l = if (length(names) > 1)
			setNames(list(v), deparse(substitute(.x), 50))
		else
			setNames(list(v), names(.x)[1])
	adrop(st_as_stars(l, dimensions = create_dimensions(dimensions, get_raster())))
}

st_as_raster = function(x, ...) {
	stopifnot(inherits(x, "stars"))
	if (length(dim(x)) > 3) {
		warning("folding all higher dimensions into the third dimension") # nocov
		x = st_apply(x, 1:2, as.vector) # fortunes::fortune("side effect") # nocov
	}
	d = st_dimensions(x)
	dxy = attr(d, "raster")$dimensions
	stopifnot(all(dxy %in% names(d)))
	bb = st_bbox(x)
	if (length(dim(x)) == 2) {
    	raster::raster(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], 
            crs = st_crs(x)$proj4string, vals = as.vector(x[[1]]))
	} else {
		third = setdiff(names(d), dxy)
		b = raster::brick(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], nl = dim(x)[third],
            crs = st_crs(x)$proj4string)
		raster::values(b) = as.vector(x[[1]])
		z = seq(d[[third]])
		if (all(!is.na(z)))
			raster::setZ(b, z)
		b
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
        stop("package sp required, please install it first") # nocov
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to Raster* objects")
	st_as_raster(from)
})
