shorten_names = function(x, n) {
	x = head(x ,n)
	bn = basename(x)
	here = paste0(normalizePath("."), .Platform$file.sep, bn)
	clean = gsub("\"", "", bn)
	if (any(x != here))
		paste0("[...]", .Platform$file.sep, clean)
	else
		clean
}

#' @export
print.stars_proxy = function(x, ..., n = 1e5, nfiles = 10, simplify = TRUE) {
	cat("stars_proxy object with", length(x), 
		if (length(x) > 1) "attributes" else "attribute",
		"in", if (sum(lengths(x)) > 1) "files" else "file")
	if (length(x[[1]]) > nfiles)
		cat("; showing the first", min(length(x[[1]]), nfiles), "filenames\n")
	else
		cat(":\n")
	if (simplify)
		print(lapply(x, shorten_names, n = nfiles))
	else
		print(lapply(x, head, n = nfiles))
	if (!is.null(attr(x, "NA_value")) && !is.na(attr(x, "NA_value")))
		cat("NA_value: ", attr(x, "NA_value"), "\n")
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
as.data.frame.stars_proxy = function(x, ...) {
	as.data.frame(st_as_stars(x), ...)
}


#' @name plot
#' @export
#' @details when plotting a subsetted \code{stars_proxy} object, the default value for argument \code{downsample} will not be computed correctly, and it and has to be set manually.
plot.stars_proxy = function(x, y, ..., downsample = get_downsample(dim(x))) {
	x = st_as_stars(x, downsample = downsample, ...)
	plot(x, ..., downsample = 0)
}

st_stars_proxy = function(x, dimensions, NA_value = NA_real_) {
	stopifnot(is.list(x))
	stopifnot(inherits(dimensions, "dimensions"))
	structure(x, dimensions = dimensions, NA_value = NA_value,
		class = c("stars_proxy", "stars"))
}

#' @export
#' @param along_crs logical; if \code{TRUE}, combine arrays along a CRS dimension
#' @name c.stars
c.stars_proxy = function(..., along = NA_integer_, along_crs = FALSE, try_hard = FALSE, nms = names(list(...))) {
	dots = list(...)
	# Case 1: merge attributes of several objects by simply putting them together in a single stars object;
	# dim does not change:
	if (length(dots) == 1) # do nothing
		dots[[1]]
	else if (along_crs)
		combine_along_crs_proxy(dots)
	else if (identical(along, NA_integer_)) { 
		if (identical_dimensions(dots))
			st_stars_proxy(do.call(c, lapply(dots, unclass)), dimensions = st_dimensions(dots[[1]]))
		else {
			# currently catches only the special case of ... being a broken up time series:
			along = sort_out_along(dots)
			if (!is.na(along))
				do.call(c, c(dots, along = along))
			else if (!try_hard)
				stop("don't know how to merge arrays: please specify parameter along")
			else {
				d = lapply(dots, st_dimensions)
				ident = c(TRUE, sapply(d[-1], identical, d[[1]]))
				if (!all(ident))
					warning(paste(
					"ignored subdataset(s) with dimensions different from first subdataset:", 
					paste(which(!ident), collapse = ", "), 
					"\nuse gdal_subdatasets() to find all subdataset names"))
				setNames(st_stars_proxy(do.call(c, 
						lapply(dots[ident], unclass)), dimensions = st_dimensions(dots[[1]])),
						nms[ident])
			}
		}
	} else {
		if (is.list(along)) { # custom ordering of ... over dimension(s) with values specified
			stop("for proxy objects, along argument as list is not implemented")
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
			ret = if (length(dots[[1]]) == 1)
					list(attr = unlist(do.call(c, lapply(dots, unclass))))
				else {
					m = mapply(c, lapply(dots, unclass)) # simplifies to list matrix
					setNames(lapply(seq_len(nrow(m)), function(i) unlist(m[i,])), names(dots[[1]]))
				}

			dims = combine_dimensions(dots, along_dim)
			if (along_dim == length(d) + 1)
				names(dims)[along_dim] = if (is.character(along)) along else "new_dim"
			st_stars_proxy(ret, dimensions = dims)
		}
	}
}

combine_along_crs_proxy = function(dots) {
	#crs = as.integer(sapply(l, function(x) st_crs(x)$epsg))
	crs = lapply(l, st_crs)
	# erase offset:
	erase_offset = function(x) { 
		d = st_dimensions(x)
		# xy = attr(d, "raster")$dimensions
		# d[[ xy[1] ]]$offset = d[[ xy[2] ]]$offset = NA
		st_set_crs(st_stars_proxy(x, d), NA)
	}
	l = lapply(dots, erase_offset)
	ret = do.call(c, c(l, along = "crs"))
	st_set_dimensions(ret, "crs", values = crs, point = TRUE)
}


#' @export
#' @name redimension
st_redimension.stars_proxy = function(x, new_dims = st_dimensions(x), along = list(new_dim = names(x)), ...) {

	d = st_dimensions(x)
	new_dim = create_dimension(values = along[[1]])
	dims = create_dimensions(c(d, new_dim = list(new_dim)), attr(d, "raster"))
	names(dims)[names(dims) == "new_dim"] = names(along)
	ret = list(unlist(do.call(c, lapply(x, unclass))))
	st_stars_proxy(setNames(ret, paste(names(x), collapse = ".")), dimensions = dims)
}

# fetch a stars object from a stars_proxy object, using downsampling
fetch = function(x, downsample = 0, ...) {
	stopifnot(inherits(x, "stars_proxy"))
	d = st_dimensions(x)
	xy = attr(d, "raster")$dimensions
	dx = d[[ xy[1] ]]
	dy = d[[ xy[2] ]]
	nBufXSize = nXSize = dx$to - dx$from + 1
	nBufYSize = nYSize = dy$to - dy$from + 1

	downsample = rep(downsample, length.out = 2)
	if (downsample[1] > 0) 
		nBufXSize = nBufXSize / (downsample[1] + 1)
	if (downsample[2] > 0) 
		nBufYSize = nBufYSize / (downsample[2] + 1)

	rasterio = list(nXOff = dx$from, nYOff = dy$from, nXSize = nXSize, nYSize = nYSize, 
		nBufXSize = nBufXSize, nBufYSize = nBufYSize)
	if (!is.null(bands <- d[["band"]]) && !is.null(bands$values) && is.numeric(bands$values)) # we want to select here
		rasterio$bands = bands$values

	# do it:
	ret = lapply(x, read_stars, RasterIO = rasterio, 
		NA_value = attr(x, "NA_value") %||% NA_real_, normalize_path = FALSE, 
		proxy = FALSE, ...)

	along = if (length(dim(x)) > 3)
			setNames(list(st_get_dimension_values(x, 4)), tail(names(st_dimensions(x)), 1))
		else
			list(new_dim = names(ret))
	
	ret = if (length(ret) == 1)
		st_redimension(ret[[1]], along = along)
	else
		do.call(c, lapply(ret, st_redimension, along = along))
	
	new_dim = st_dimensions(ret)
	for (dm in setdiff(names(d), xy)) # copy over non x/y dimension values, if present:
		if (dm %in% names(new_dim))
			new_dim[[dm]] = d[[dm]]

	st_set_crs(st_stars(setNames(ret, names(x)), new_dim), st_crs(x))
}

check_xy_warn = function(call, dimensions) {
	if (as.character(as.list(call)[[1]]) == "st_apply") {
		# check dims
		MARGIN = as.list(call)$MARGIN
		if (inherits(MARGIN, "call"))
			MARGIN = eval(MARGIN)
		xy = attr(dimensions, "raster")$dimensions
		ok = if (is.numeric(MARGIN))
				all(which(names(dimensions) %in% xy) %in% MARGIN)
			else
				all(xy %in% MARGIN)
		if (!ok)
			warning("st_apply on x/y dimensions applied to downsampled image(s)")
	}
}

#' @name st_as_stars
#' @param downsample integer: if larger than 0, downsample with this rate (number of pixels to skip in every row/column); if length 2, specifies downsampling rate in x and y.
#' @param url character; URL of the stars endpoint where the data reside 
#' @param envir environment to resolve objects in
#' @export
st_as_stars.stars_proxy = function(.x, ..., downsample = 0, url = attr(.x, "url"), envir = parent.frame()) {
	if (! is.null(url)) { # execute/get remotely: # nocov start
		# if existing, convert call_list to character:
		attr(.x, "call_list") = lapply(attr(.x, "call_list"), deparse)
		# push the object to url, then st_as_stars() it there:
		tempnam = substr(tempfile(pattern = "Z", tmpdir = "", fileext = ""), 2, 15)
		put_data_url(url, tempnam, .x)
		expr = paste0("st_as_stars(", tempnam, ", url = NULL, downsample=", downsample, ", envir = data)") # evaluate in "data" first
		ret = get_data_url(url, expr)
		put_data_url(url, tempnam, NULL) # remove the temporary object
		ret # nocov end
	} else {
		cl = attr(.x, "call_list")
		# FIXME: this means we ALLWAYS process after (possibly partial) reading; 
		# there are cases where this is not right. Hence:
		# TODO: only warn when there is a reason to warn.
		if (!all(downsample == 0))
			lapply(attr(.x, "call_list"), check_xy_warn, dimensions = st_dimensions(.x))
		process_call_list(fetch(.x, ..., downsample = downsample), cl, envir = envir)
	}
}

st_as_stars_proxy = function(x, fname = tempfile(fileext = ".tif"), quiet = TRUE, NA_value = NA_real_) {
	stopifnot(inherits(x, "stars"))
	if (inherits(x, "stars_proxy"))
		return(x)
	write_stars(x, fname, NA_value = NA_value)
	if (!quiet)
		cat(paste("writing to", fname, "\n"))
	st_stars_proxy(setNames(list(fname), names(x)[1]), st_dimensions(x), NA_value = NA_value)
}

# execute the call list on a stars object
process_call_list = function(x, cl, envir = new.env()) {
	for (i in seq_along(cl)) {
		if (is.character(cl[[i]]))
			cl[[i]] = parse(text = cl[[i]])[[1]]
		stopifnot(is.call(cl[[i]]))
		lst = as.list(cl[[i]]) 
		envir [[ names(lst)[[2]] ]] = x
		x = eval(cl[[i]], envir = envir, enclos = parent.frame())
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
aperm.stars_proxy = function(a, perm = NULL, ...) {
	collect(a, match.call(), "aperm", "a")
}


#' @export
merge.stars_proxy = function(x, y, ...) {
	if (!missing(y))
		stop("argument y needs to be missing: merging attributes of x")
	# collect(x, match.call(), "merge")
	if (length(x) > 1) { 
		x[[1]] = unlist(x)
		for (i in 2:length(x))
			x[[i]] = NULL
	}
	st_stars_proxy(x, dimensions = create_dimensions(append(st_dimensions(x), 
		list(band = create_dimension(values = names(x[[1]])))), 
		raster = attr(st_dimensions(x), "raster")))
}


#' @export
"[.stars_proxy" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
	get_range = function(expr) {
		v = try(eval(expr, parent.frame(2)), silent = TRUE)
		if (is.numeric(v) && all(diff(v) == 1))
			range(v)
		else
			NA
	}
	mc = match.call()
	lst = as.list(mc)
	if (length(lst) < 3)
		return(x) # 
	if (missing(i)) # insert:
		lst = c(lst[1:2], i = TRUE, lst[-(1:2)])
	if (inherits(i, c("character", "logical", "numeric"))) {
		x = st_stars_proxy(unclass(x)[ lst[[3]] ], st_dimensions(x))
		lst[["i"]] = TRUE # this one has been handled now
		for (ix in 1:3) { # FIXME: further than 3?
			if (length(lst) >= 4 && !any(is.na(r <- get_range(lst[[4]])))) {
				attr(x, "dimensions")[[ix]]$from = r[1]
				attr(x, "dimensions")[[ix]]$to = r[2]
				lst[[4]] = NULL # remove
			}
		}
	} else if (crop && inherits(i, c("sf", "sfc", "stars", "bbox"))) {
		x = st_crop(x, i, ..., collect = FALSE) # does bounding box cropping only
		if (inherits(i, c("stars", "bbox")))
			lst[["i"]] = TRUE # this one has been handled now
	}

	# return:
	if (length(lst) == 3 && isTRUE(lst[["i"]])) 
		x
	else # still processing the geometries inside the bbox:
		collect(x, as.call(lst), "[") # postpone every arguments > 3 to after reading cells
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
#' @param collect logical; if \code{TRUE}, repeat cropping on \code{stars} object, i.e. after data has been read
#' @export
st_crop.stars_proxy = function(x, y, ..., crop = TRUE, epsilon = sqrt(.Machine$double.eps), collect = TRUE) {
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
		cr = colrow_from_xy(matrix(bb, 2, byrow=TRUE), dm)
		for (i in seq_along(dm)) {
			if (names(d[i]) == xd) {
				dm[[ xd ]]$from = max(1, cr[1, 1], na.rm = TRUE)
				dm[[ xd ]]$to = min(d[xd], cr[2, 1], na.rm = TRUE)
			}
			if (names(d[i]) == yd) {
				if (dm[[ yd ]]$delta < 0)
					cr[1:2, 2] = cr[2:1, 2]
				dm[[ yd ]]$from = max(1, cr[1, 2], na.rm = TRUE)
				dm[[ yd ]]$to = min(d[yd], cr[2, 2], na.rm = TRUE)
			}
		}
	}
	x = st_stars_proxy(x, dm) # crop to bb
	if (collect)
		collect(x, match.call(), "st_crop") # crops further when realised
	else
		x
}

#' @export
st_apply.stars_proxy = function(X, MARGIN, FUN, ...) {
	collect(X, match.call(), "st_apply", "X")
}

#' @export
predict.stars_proxy = function(object, model, ...) {
	collect(object, match.call(), "predict", "object")
}

#' @export
split.stars_proxy = function(x, ...) {
	collect(x, match.call(), "split")
}

#nocov start
get_data_url = function(url, expr = NULL) {
	if (!requireNamespace("httr", quietly = TRUE)) # GET, POST, PUT
		stop("package httr required, please install it first") # nocov
	if (!requireNamespace("jsonlite", quietly = TRUE)) # base64_dec, base64_enc, toJSON, fromJSON
		stop("package jsonlite required, please install it first") # nocov

    if (is.null(expr))
        jsonlite::fromJSON( httr::content(httr::GET(url), "text", encoding = "UTF-8"))
    else {
        js = jsonlite::fromJSON(
            httr::content(httr::POST(url, body = list(expr = expr), encode = "json"),
                "text", encoding = "UTF-8"))
		if (is.list(js) && !is.null(js$error))
			stop(paste(js$error, ":", js$message))
        unserialize(jsonlite::base64_dec(js))
	}
}

put_data_url = function(url, name, value) {
	if (!requireNamespace("httr", quietly = TRUE)) # GET, POST, PUT
		stop("package httr required, please install it first") # nocov
	if (!requireNamespace("jsonlite", quietly = TRUE)) # base64_dec, base64_enc, toJSON, fromJSON
		stop("package jsonlite required, please install it first") # nocov

    value = jsonlite::toJSON(jsonlite::base64_enc(serialize(value, NULL)))
    httr::PUT(url, body = list(name = name, value = value), encode = "json")
}
#nocov end
