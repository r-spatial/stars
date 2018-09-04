#' @export
print.stars_proxy = function(x, ..., n = 1e5) {
	cat("stars_proxy object with", length(x), 
		if (length(x) > 1) "attributes in files:\n" else "attribute in file:\n")
	print(structure(unclass(x), dimensions = NULL))
	cat("dimension(s):\n")
	print(st_dimensions(x), ...)
	if (!is.null(attr(x, "call_list"))) {
		cat("call list:\n")
		print(unlist(attr(x, "call_list")))
	}
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
  st_stars_proxy(x, d)
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

st_stars_proxy = function(x, dimensions)
	structure(x, dimensions = dimensions, class = c("stars_proxy", "stars"))

c.stars_proxy = function(..., along = NA_integer_) {
	dots = list(...)
	# Case 1: merge attributes of several objects by simply putting them together in a single stars object;
	# dim does not change:
	if (is.na(along) && length(dots) > 1) { 
		if (identical_dimensions(dots))
			st_stars_proxy(do.call(c, lapply(dots, unclass)), attr(dots[[1]], "dimensions"))
		else {
			# currently catches only the special case of ... being a broken up time series:
			along = sort_out_along(dots)
			if (is.na(along))
				stop("don't know how to merge arrays: please specify parameter along")
			do.call(c, c(dots, along = along))
		}
	} else {
		# Case 2: single stars object, collapse attributes into new array dimension:
		if (length(dots) == 1) {
			if (is.list(along)) {
				values = along[[1]]
				dim_name = names(along)[1]
			} else {
				values = names(dots[[1]])
				dim_name = "new_dim"
			}
			old_dim = st_dimensions(dots[[1]])
			new_dim = create_dimension(values = values)
			dims = create_dimensions(c(old_dim, new_dim = list(new_dim)), attr(old_dim, "raster"))
			names(dims)[names(dims) == "new_dim"] = dim_name
			# FIXME: to be tested:
			st_stars_proxy(unlist(do.call(c, c(dots, along = length(dim(dots[[1]])) + 1))), dimensions = dims) 
		} else if (is.list(along)) { # custom ordering of ... over dimension(s) with values specified
			stop("not implemented yet")
			# FIXME: t.b.d.
			if (prod(lengths(along)) != length(dots))
				stop("number of objects does not match the product of lenghts of the along argument", call. = FALSE)
			# abind all:
			d = st_dimensions(dots[[1]])
			ret = mapply(abind, ..., along = length(d) + 1, SIMPLIFY = FALSE)
			# make dims:
			newdim = c(dim(dots[[1]]), lengths(along))
			ret = lapply(ret, function(x) { dim(x) = newdim; x })
			ret = propagate_units(ret, dots[[1]])
			# make dimensions:
			for (i in seq_along(along))
				d[[ names(along)[i] ]] = create_dimension(values = along[[i]])
			st_as_stars(ret, dimensions = d)
		} else { # loop over attributes, abind them:
			# along_dim: the number of the dimension along which we merge arrays
			d = st_dimensions(dots[[1]])
			along_dim = if (is.character(along)) {
				along_dim = which(along == names(d))
				if (length(along_dim) == 0)
					length(d) + 1
				else
					along_dim
			} else
				along
			# ret = propagate_units(mapply(abind, ..., along = along_dim, SIMPLIFY = FALSE), dots[[1]])
			m = mapply(c, lapply(dots, unclass)) # simplifies to list matrix
			ret = setNames(lapply(seq_len(nrow(m)), function(i) unlist(m[i,])), names(dots[[1]]))
			dims = combine_dimensions(dots, along_dim)
			if (along_dim == length(d) + 1)
				names(dims)[along_dim] = if (is.character(along)) along else "new_dim"
			st_stars_proxy(ret, dimensions = dims)
		}
	}
}

fetch = function(x, downsample = 0, ...) {
	stopifnot(inherits(x, "stars_proxy"))
	d = st_dimensions(x)
	dx = d[["x"]]
	dy = d[["y"]]
	nBufXSize = nXSize = dx$to - dx$from + 1
	nBufYSize = nYSize = dy$to - dy$from + 1
	if (any(downsample > 0)) {
		nBufXSize = nBufXSize / (downsample + 1)
		nBufYSize = nBufYSize / (downsample + 1)
	}
	bands = d[["band"]]
	if (is.null(bands))
		bands = list(values = 1)
	bands = if (!is.null(bands$values))
			bands$values
		else
			bands$from:bands$to
	rasterio = list(nXOff = dx$from, nYOff = dy$from, nXSize = nXSize, nYSize = nYSize, 
		nBufXSize = nBufXSize, nBufYSize = nBufYSize, bands = bands)
	setNames(do.call(c, lapply(x, read_stars, RasterIO = rasterio, ...)), names(x))
}

collect = function(x, call) {
	call_list = attr(x, "call_list")
	if (is.null(call_list))
		call_list = list()
	structure(x, call_list = c(call_list, call))
}


#' @export
adrop.stars_proxy = function(x, drop = which(dim(x) == 1), ...) {
	collect(x, match.call())
}

#' @export
"[.stars_proxy" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
	collect(x, match.call())
}
