#' @export
print.stars_proxy = function(x, ..., n = 1e5) {
	cat("stars_proxy object with", length(x), 
		if (length(x) > 1) "attributes in files:\n" else "attribute in file:\n")
	print(structure(unclass(x), dimensions = NULL, call_list = NULL))
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
  	return(st_crop(x, i, crop = crop, ...))
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

#' @export
plot.stars_proxy = function(x, y, ..., downsample = get_downsample(dim(x))) {
	x = st_as_stars(x, downsample = downsample)
	NextMethod()
}

st_stars_proxy = function(x, dimensions)
	structure(x, dimensions = dimensions, class = c("stars_proxy", "stars"))

#' @export
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
			st_stars_proxy(unlist(do.call(c, c(lapply(dots, unclass), along = length(dim(dots[[1]])) + 1))),
				dimensions = dims) 
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
	xy = attr(d, "raster")$dimensions
	dx = d[[ xy[1] ]]
	dy = d[[ xy[2] ]]
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


#' @export
st_as_stars.stars_proxy = function(.x, ..., downsample = 0) {
	cl = attr(.x, "call_list")
	# FIXME: this means we ALLWAYS process after (possibly partial) reading; 
	# there are cases where this is not right. Hence:
	if (downsample != 0 && length(attr(.x, "call_list")) > 0) 
		warning("deferred processes applied to downsampled image(s)")
	process_call_list(fetch(.x, ..., downsample = downsample), cl)
}

# execute the call list on a stars object
process_call_list = function(x, cl) {
	pf_copy <- as.environment(as.list(parent.frame(), all.names=TRUE))
	pf_copy = parent.frame()
	#pf_copy$x = NULL # just in case
	for (i in seq_along(cl)) {
		lst = as.list(cl[[i]]) 
		pf_copy [[ names(lst)[[2]] ]] = x # FIXME: side effects because we trash parent.frame()?
		x = eval(cl[[i]], envir = pf_copy)
	}
	x
}

# add a call to the call list, possibly replacing function name (fn) and first arg name
collect = function(x, call, fn, first_arg = "x") {
	call_list = attr(x, "call_list")
	if (is.null(call_list))
		call_list = list()
	lst = as.list(call)
	if (!missing(fn))
		lst[[1]] = as.name(fn)
	lst[[2]] = as.name(first_arg)
	names(lst)[[2]] = first_arg
	call = as.call(lst)
	# append:
	structure(x, call_list = c(call_list, call))
}


#' @export
adrop.stars_proxy = function(x, drop = which(dim(x) == 1), ...) {
	collect(x, match.call(), "adrop")
}

#' @export
"[.stars_proxy" = function(x, ..., drop = FALSE, crop = TRUE) {
	mc = match.call()
	lst = as.list(mc)
	if (length(lst) < 3)
		return(x) # 
	if (as.character(lst[[3]]) != "" && crop) { # i present: do attr selection or bbox now:
		x = if (is.character(lst[[3]]))
			st_stars_proxy(unclass(x)[ lst[[3]] ], st_dimensions(x))
		else {
			i = as.character(lst[[3]])
			if (inherits(get(i), c("sf", "sfc", "stars", "bbox")))
				st_crop(x, get(i))
			else
				stop(paste("unrecognized selector:", i))
		}
		if (length(lst) == 3 && crop) # we're done
			return(x)
		lst[[3]] = TRUE # this one has been handled
	}
	collect(x, as.call(lst), "[") # postpone every aruments > 3 to after reading cells
}

# shrink bbox with e * width in each direction
bb_shrink = function(bb, e) {
	dx = diff(bb[c("xmin", "xmax")])
	dy = diff(bb[c("ymin", "ymax")])
	st_bbox(setNames(c(bb["xmin"] + e * dx, 
		bb["ymin"] + e * dy, 
		bb["xmax"] - e * dx, 
		bb["ymax"] - e * dy), c("xmin", "ymin", "xmax", "ymax")))
}

#' @name st_crop
#' @export
st_crop.stars_proxy = function(x, y, ..., crop = TRUE, epsilon = 0) {
	d = dim(x)
	dm = st_dimensions(x)
	if (st_crs(x) != st_crs(y))
		stop("for cropping, the CRS of both objects has to be identical")
	if (crop && has_raster(x)) {
		rast = attr(dm, "raster")$dimensions
		xd = rast[1]
		yd = rast[2]
		bb = if (!inherits(y, "bbox"))
				st_bbox(y)
			else
				y
		if (epsilon != 0)
			bb = bb_shrink(bb, epsilon)
		# FIXME: document how EXACTLY cropping works; https://github.com/hypertidy/tidync/issues/73
		cr = round(colrow_from_xy(matrix(bb, 2, byrow=TRUE), get_geotransform(dm)) + 0.5)
		for (i in seq_along(dm)) {
			if (names(d[i]) == xd) {
				dm[[ xd ]]$from = max(1, cr[1, 1])
				dm[[ xd ]]$to = min(d[xd], cr[2, 1])
			}
			if (names(d[i]) == yd) {
				if (dm[[ yd ]]$delta < 0)
					cr[1:2, 2] = cr[2:1, 2]
				dm[[ yd ]]$from = max(1, cr[1, 2])
				dm[[ yd ]]$to = min(d[yd], cr[2, 2])
			}
		}
	}
	st_stars_proxy(x, dm)
}

#' @export
st_apply.stars_proxy = function(X, MARGIN, FUN, ...) {
	collect(X, match.call(), "st_apply", "X")
}
