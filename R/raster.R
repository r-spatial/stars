
#' @name st_as_stars
#' @param att see \link[raster]{factorValues}; column in the RasterLayer's attribute table
#' @export
st_as_stars.Raster = function(.x, ..., att = 1) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") # nocov
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first") # nocov
	#0 360 -90  90
	e = as.vector(raster::extent(.x)) # xmin xmax ymin ymax
	v = raster::values(.x)
	dim(v) = dim(.x)[c(2,1,3)]
	if (all(raster::is.factor(.x))) {
		l = raster::levels(.x)[[1]]$levels
		if (length(l) == 0) # get the layer's RAT, column att:
			l = raster::factorValues(.x, seq_len(max(v, na.rm = TRUE)), att = att)[[1]]
		colors = try(.x@legend@colortable, silent = TRUE)
		if (inherits(colors, "try-error") || length(colors) == 0)
			colors = NULL
		else
			colors = colors[-1]
		v = structure(v, class = "factor", levels = as.character(l), colors = colors)
		# FIXME: should we handle levels for all layers here, or break on multiple different ones?
	}
	dimensions = list(
		x = create_dimension(from = 1, to = dim(v)[1], offset = e[1], 
			delta = (e[2]-e[1])/dim(v)[1], refsys = st_crs(raster::crs(.x))),
		y = create_dimension(from = 1, to = dim(v)[2], offset = e[4],
			delta = (e[3]-e[4])/dim(v)[2], refsys = st_crs(raster::crs(.x))))
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
	x = st_upfront(x) # x/y dimensions first
	if (length(dim(x)) > 3) {
		warning("folding all higher dimensions into the third dimension") # nocov
		x = st_apply(x, 1:2, as.vector) # fortunes::fortune("side effect") # nocov
	}
	d = st_dimensions(x)
	if (d[[2]]$delta > 0) { # swap:
		ny = dim(x)[2]
		d[[2]]$offset = d[[2]]$offset + ny * d[[2]]$delta # top
		d[[2]]$delta = -d[[2]]$delta # going down
		x[[1]] = if (length(dim(x)) == 2)
				x[[1]][,ny:1]
			else
				x[[1]][,ny:1,]
	}
	dxy = attr(d, "raster")$dimensions
	stopifnot(all(dxy %in% names(d)))
	bb = st_bbox(x)
	values = if (is.factor(x[[1]]))
			structure(x[[1]], dim = NULL)
		else
			as.vector(x[[1]]) # would convert factor into character
	if (length(dim(x)) == 2) {
    	raster::raster(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], 
            crs = as(st_crs(x), "CRS"), vals = values) 
	} else {
		third = setdiff(names(d), dxy)
		b = raster::brick(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
			xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], nl = dim(x)[third],
            crs = as(st_crs(x), "CRS"))
		raster::values(b) = values
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
#' @aliases coerce,stars_proxy,Raster-method
setAs("stars", "Raster", function(from) { 
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") # nocov
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to Raster* objects")
	st_as_raster(from)
})

setAs("stars_proxy", "Raster", function(from) { 
    if (!requireNamespace("raster", quietly = TRUE))
        stop("package raster required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to Raster* objects")
	if (length(attr(from, "call_list"))) {
		fname = paste0(tempfile(), ".tif")
		write_stars(from, fname)
		from = fname
	}
	raster::brick(unlist(from))
})
