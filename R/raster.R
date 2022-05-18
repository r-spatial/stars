#' @name st_as_stars
#' @param att see \link[raster:factor]{factorValues}; column in the RasterLayer's attribute table
#' @param ignore_file logical; if \code{TRUE}, ignore the Raster object file name
#' @export
st_as_stars.Raster = function(.x, ..., att = 1, ignore_file = FALSE) {
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required, please install it first") # nocov

	#0 360 -90  90
	e = as.vector(raster::extent(.x)) # xmin xmax ymin ymax

	RasterIO = if ("data" %in% slotNames(.x) && inherits(.x@data, ".SingleLayerData"))
			list(bands = .x@data@band)
		else
			list()
	if (!ignore_file) {
		file = if ("file" %in% slotNames(.x))
				.x@file@name
			else if ("filename" %in% slotNames(.x))
				.x@filename
			else
				""
		if (file != "") {
			r = try(read_stars(file, RasterIO = RasterIO, ...), silent = TRUE)
			if (!inherits(r, "try-error")) {

				if (is.na(st_crs(r)))
					r = st_set_crs(r, st_crs(raster::crs(.x)))

				r = fix_dims(r, e)

				if (inherits(.x, "RasterLayer"))
					names(r) <- names(.x)

				return(r)
			}

		}
	}

	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first") # nocov
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
	ret = st_as_stars(l, dimensions = create_dimensions(dimensions, get_raster()))
	if (dim(ret)[3] == 1)
		adrop(ret, 3)
	else
		ret
}
#' Coerce stars object into a Raster raster or brick
#'
#' Coerce stars object into a Raster raster or brick
#' @param from object to coerce
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,stars,Raster-method
#' @aliases coerce,stars_proxy,Raster-method
#' @returns RasterLayer or RasterBrick
#' @details If the stars object has more than three dimensions, all dimensions higher than the third will be collapsed into the third dimensions. If the stars object has only an x/y raster but multiple attributes, these are merged first, then put in a raster brick.
setAs("stars", "Raster", function(from) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first") # nocov
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to Raster* objects")
	st_as_raster(from, class = "Raster")
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

#' @name st_as_stars
#' @param ignore_file logical; if \code{TRUE}, ignore the SpatRaster object file name
#' @export
st_as_stars.SpatRaster = function(.x, ..., ignore_file = FALSE) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov

	#0 360 -90  90
	e = as.vector(terra::ext(.x)) # xmin xmax ymin ymax

	src = terra::sources(.x, bands=TRUE)

	attr_name = basename(src$source[1])
	if (attr_name == "")
		attr_name = "values"

	if (!ignore_file && all(src$source != "")) {
	# there can be multiple files, but only the first one is used here.
	# perhaps a warning should be given; better would be to iterate over "sid"
	# but you might have a situation where some sources are filenames and others are not
		lst = vector("list", length(unique(src$sid)))
		for (i in unique(src$sid)) {
			file = unique(src$source[src$sid == i])
			if (length(file) > 1)
				stop("more than one file per sid: giving up; try ignore_file=FALSE")
			# 	RasterIO = if (dim(.x)[3] == 1)
			# > 1 would be more sensible? 
			# But this can only be ignored if the _file_ has 1 band
			RasterIO = list(bands = src$bands[src$sid == i])
			r = try(read_stars(file, RasterIO = RasterIO, ...), silent = TRUE)
			if (! inherits(r, "try-error")) {
				if (is.na(st_crs(r)))
					r = st_set_crs(r, st_crs(terra::crs(.x)))
				r = fix_dims(r, e)
				if (length(unique(src$sid)) > 1 && length(dim(r)) > 2)
					r = split(r)
				#transfer the layer/band names as well?
				# ... = names(.x)[1:(dim(r)[3])]
				# perhaps check whether they represent a time dimension (all(!is.na(time(.x))))
			} else
				stop(paste("error reading", file, "bands", paste0(RasterIO$bands, collapse = " ")))
			lst[[i]] = r
		}
		ret = if (length(lst) > 1)
				merge(setNames(do.call(c, lst), names(.x)))
			else
				r
		if (!all(is.na(terra::time(.x))))
			ret = st_set_dimensions(ret, 3, values = terra::time(.x), names = "time")

		return(setNames(ret, attr_name))
	}

	v = terra::values(.x, mat = FALSE)
	dim(v) = dim(.x)[c(2,1,3)]
	if (all(terra::is.factor(.x))) {
		l = terra::levels(.x)[[1]]
		colors = try(rgb(terra::coltab(.x)[[1]], maxColorValue = 255), silent = TRUE)
		if (inherits(colors, "try-error") || length(colors) == 0)
			colors = NULL
		else if (length(colors) == length(levels) + 1) # remove last color?
			colors = colors[-length(colors)]
		v = structure(v + 1, class = "factor", levels = l, colors = colors)
		# FIXME: should we handle levels for all layers here, or break on multiple different ones?
	}
	dimensions = list(
		x = create_dimension(from = 1, to = dim(v)[1], offset = e[1],
							 delta = (e[2]-e[1])/dim(v)[1], refsys = st_crs(terra::crs(.x))),
		y = create_dimension(from = 1, to = dim(v)[2], offset = e[4],
							 delta = (e[3]-e[4])/dim(v)[2], refsys = st_crs(terra::crs(.x))))
	dimensions$band = create_dimension(values = names(.x))
#	l = if (length(names) > 1)
#		setNames(list(v), deparse(substitute(.x), 50))
#	else
#		setNames(list(v), names(.x)[1])
	ret = st_as_stars(list(v), dimensions = create_dimensions(dimensions, get_raster()))
	if (dim(ret)[3] == 1)
		ret = adrop(ret, 3)
	else if (!all(is.na(terra::time(.x))))
		ret = st_set_dimensions(ret, 3, values = terra::time(.x), names = "time")

	setNames(ret, attr_name)
}

#' Coerce stars object into a terra SpatRaster
#'
#' Coerce stars object into a terra SpatRaster
#' @param from object to coerce
#' @name as
#' @rdname coerce-methods
#' @aliases coerce,stars,Terra-method
#' @aliases coerce,stars_proxy,Terra-method
#' @returns SpatRaster
#' @details If the stars object has more than three dimensions, all dimensions higher than the third will be collapsed into the third dimensions. If the stars object has only an x/y raster but multiple attributes, these are merged first, then put in a SpatRaster.
setAs("stars", "SpatRaster", function(from) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to SpatRaster objects")
	st_as_raster(from, class = "SpatRaster")
})

setAs("stars_proxy", "SpatRaster", function(from) {
	if (!requireNamespace("terra", quietly = TRUE))
		stop("package terra required, please install it first") # nocov
	if (!is_regular_grid(from))
		stop("only regular rasters can be converted to SpatRaster objects")
	if (length(attr(from, "call_list"))) {
		fname = paste0(tempfile(), ".tif")
		write_stars(from, fname)
		from = fname
	}
	terra::rast(unlist(from))
})

fix_dims = function(r, e){
	e = unname(e)
	rdims = attr(r, "dimensions")
	dxy = attr(rdims, "raster")$dimensions
	dimx = rdims[[dxy[1]]]
	dimy = rdims[[dxy[2]]]

	xrev = dimx$delta < 0
	yrev = dimy$delta < 0

	if (xrev) {
		dimx$offset = e[2]
		dimx$delta = (e[1] - e[2]) / (dimx$to - dimx$from + 1)
	} else {
		dimx$offset = e[1]
		dimx$delta = (e[2] - e[1]) / (dimx$to - dimx$from + 1)
	}

	if (yrev) {
		dimy$offset = e[4]
		dimy$delta = (e[3] - e[4]) / (dimy$to - dimy$from + 1)
	} else {
		dimy$offset = e[3]
		dimy$delta = (e[4] - e[3]) / (dimy$to - dimy$from + 1)
	}

	attr(r, "dimensions")[[dxy[1]]] = dimx
	attr(r, "dimensions")[[dxy[2]]] = dimy

	r
}

st_as_raster = function(x, class, ...) {
	stopifnot(inherits(x, "stars"))
	x_crs = st_crs(x)
	x = st_upfront(x) # x/y dimensions first
	if (length(dim(x)) > 3) {
		warning("folding all higher dimensions into the third dimension") # nocov
		x = st_apply(x, 1:2, as.vector) # fortunes::fortune("side effect") # nocov
	}
	if (length(dim(x)) == 2 && length(x) > 1)
		x = merge(x)
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
	values = if (is.factor(x[[1]])) {
		structure(x[[1]], dim = NULL)
	} else {
		as.vector(x[[1]]) # would convert factor into character
	}
	if (class == "SpatRaster"){
		third = setdiff(names(d), dxy)
		b = terra::rast(nrows = dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
						xmin = bb[1], xmax = bb[3], ymin = bb[2], ymax = bb[4],
						nlyrs = ifelse(length(dim(x)) == 2, 1, dim(x)[third]),
						crs = x_crs$wkt)
		terra::values(b) = values
		if (all(vapply(x, is.factor, FUN.VALUE = logical(1)))) {
			terra::coltab(b) = lapply(x, function(x) t(col2rgb(attr(x, "colors"), alpha = TRUE)))
		}
		if (length(dim(x)) != 2){
			z = seq(d[[third]])
			if (!any(is.na(z))) {
				if (is.character(z)) {
					names(b) = z
				} else {
					names(b) = paste0(third, z)
				}
			}
		}
	} else {
		if (length(dim(x)) == 2) {
			b = raster::raster(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
							   xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4],
							   crs = as(x_crs, "CRS"), vals = values)
		} else {
			third = setdiff(names(d), dxy)
			b = raster::brick(nrows=dim(x)[ dxy[2] ], ncols=dim(x)[ dxy[1] ],
							  xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], nl = dim(x)[third],
							  crs = as(x_crs, "CRS"))
			raster::values(b) = values
			z = seq(d[[third]])
			if (!any(is.na(z))) {
				if (is.character(z))
					names(b) = z
				else
					b = raster::setZ(b, z)
			}
		}
	}
	return(b)
}
