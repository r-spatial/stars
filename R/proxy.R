#' @export
print.stars_proxy = function(x, ..., n = 1e5) {
	cat("stars_proxy object with", length(x), 
		if (length(x) > 1) "attributes in files:\n" else "attribute in file:\n")
	print(structure(unclass(x), dimensions = NULL))
	cat("dimension(s):\n")
	print(st_dimensions(x), ...)
}

#' @export
dim.stars_proxy = function(x) {
	dim(st_dimensions(x))
}

#' @export
"[.stars_proxy" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
  if (drop)
    stop("cannot drop dimensions of stars_proxy objects")
  missing.i = missing(i)
  # special case:
  if (! missing.i && inherits(i, c("sf", "sfc", "bbox")))
  	return(st_crop(x, i, crop = crop))
  mc <- match.call(expand.dots = TRUE)
  # select list elements from x, based on i:
  d = st_dimensions(x)
  ed = expand_dimensions(d)
  x = unclass(x)[i]
  # selects also on dimensions:
  if (length(mc) > 3) {
    mc[[1]] <- `[`
    if (! missing(i))
		mc[[3]] <- NULL # remove i
	mc[["drop"]] = FALSE
#	for (i in names(x)) {
#		mc[[2]] = as.name(i)
#		x[[i]] = eval(mc, x, parent.frame())
#	}
	mc0 = mc[1:3] # "[", x, first dim
	j = 3 # first dim
	for (i in names(d)) {
		mc0[[2]] = as.name(i)
		mc0[[3]] = mc[[j]]
		mc0[["values"]] = ed[[i]]
		d[[i]] = eval(mc0, d, parent.frame())
		j = j + 1
	}
  }
  structure(x, dimensions = d, class = c("stars_proxy", "stars"))
}

#' @name st_crop
#' @export
st_crop.stars_proxy = function(x, y, ..., crop = TRUE) {
	d = dim(x)
	dm = st_dimensions(x)
	args = rep(list(rlang::missing_arg()), length(d)+1)
	if (st_crs(x) != st_crs(y))
		stop("for cropping, the CRS of both objects has to be identical")
	if (crop) {
		bb = if (!inherits(y, "bbox"))
				st_bbox(y)
			else
				y
		cr = colrow_from_xy(matrix(bb, 2, byrow=TRUE), dm$x$geotransform)
		for (i in seq_along(d)) {
			if (names(d[i]) == "x")
				args[[i+1]] = seq(max(1, floor(cr[1, 1])), min(d["x"], ceiling(cr[2, 1])))
			if (names(d[i]) == "y") {
				if (dm$y$delta < 0)
					cr[1:2, 2] = cr[2:1, 2]
				args[[i+1]] = seq(max(1, floor(cr[1, 2])), min(d["y"], ceiling(cr[2, 2])))
			}
		}
		x = eval(rlang::expr(x[!!!args]))
	}
# what to do with a shaped crop / mask?
#	if (inherits(obj, "bbox"))
#		obj = st_as_sfc(obj)
#	xy_grd = st_as_sf(do.call(expand.grid, expand_dimensions.stars(x)[c("x", "y")]),
#		coords = c("x", "y"), crs = st_crs(x))
#	inside = st_intersects(obj, xy_grd)[[1]]
#	d = dim(x) # cropped x
#	raster = rep(NA_real_, prod(d[c("x", "y")]))
#	raster[inside] = 1
#	x * array(raster, d) # replicates over secondary dims
	x
}

#' @export
plot.stars_proxy = function(x, y, ...) {
	x = fetch(x, downsample = get_downsample(dim(x)))
	NextMethod()
}

fetch = function(x, downsample = 0) {
	d = st_dimensions(x)
	dx = d[["x"]]
	dy = d[["x"]]
	nBufXSize = nXSize = dx$to - dx$from + 1
	nBufYSize = nYSize = dy$to - dy$from + 1
	if (any(downsample > 0)) {
		nBufXSize = nBufXSize / (downsample + 1)
		nBufYSize = nBufYSize / (downsample + 1)
	}
	bands = d[["bands"]]
	bands = if (!is.null(bands$values))
			bands$values
		else
			bands$from:bands$to
	rasterio = list(nXOff = dx$from, nYOff = dy$from, nXSize = nXSize, nYSize = nYSize, 
		nBufXSize = nBufXSize, nBufYSize = nBufYSize, bands = bands)
	read_stars(unlist(x), RasterIO = rasterio)
}


