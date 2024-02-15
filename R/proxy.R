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
	if (!is.null(attr(x, "resolutions")))
		cat("multi-resolution ")
	cat("stars_proxy object with", length(x), 
		if (length(x) > 1) "attributes" else "attribute",
		"in", sum(lengths(x)), "file(s)")
	if (length(x[[1]]) > nfiles)
		cat("; showing the first", min(length(x[[1]]), nfiles), "filenames\n")
	else
		cat(":\n")
	names = lapply(x, function(nm) if (is.function(nm)) nm() else nm)
	if (simplify)
		print(lapply(names, shorten_names, n = nfiles))
	else
		print(lapply(names, head, n = nfiles))
	if (!is.null(attr(x, "NA_value")) && !is.na(attr(x, "NA_value")))
		cat("NA_value: ", attr(x, "NA_value"), "\n")
	cat("dimension(s):\n")
	print(st_dimensions(x), ...)
	if (!is.null(attr(x, "call_list"))) {
		cat("call_list:\n")
		print(unlist(attr(x, "call_list")))
		cat("This object has pending lazy operations: dimensions as printed may not reflect this.\n")
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
#' @details when plotting a subsetted \code{stars_proxy} object, the default value for argument \code{downsample} will not be computed correctly, and has to be set manually.
plot.stars_proxy = function(x, y, ..., downsample = get_downsample(dim(x))) {
	plot(st_as_stars(x, downsample = downsample, ...), ..., downsample = 0)
}

st_stars_proxy = function(x, dimensions, ..., NA_value, resolutions, RasterIO = list(), file_dim = NULL) {
	stopifnot(!missing(NA_value))
	stopifnot(!missing(resolutions))
	stopifnot(length(list(...)) == 0)
	stopifnot(is.list(x))
	stopifnot(inherits(dimensions, "dimensions"))
	if (length(RasterIO) == 0)
		RasterIO = NULL
	structure(x, dimensions = dimensions, NA_value = NA_value, resolutions = resolutions,
		RasterIO = RasterIO, file_dim = file_dim, class = c("stars_proxy", "stars"))
}

add_resolution = function(lst) {
	n = length(lst)
	resolutions = data.frame(x = numeric(n), y = numeric(n))
	for (i in seq_along(lst)) {
		d = st_dimensions(lst[[i]])
		xy = attr(d, "raster")$dimensions
		resolutions[i,] = c(d[[ xy[1] ]]$delta, d[[ xy[2] ]]$delta)
	}
	rownames(resolutions) = sapply(lst, names)
	structure(lst, resolutions = resolutions)
}

#' @export
#' @param along_crs logical; if \code{TRUE}, combine arrays along a CRS dimension
#' @name c.stars
c.stars_proxy = function(..., along = NA_integer_, along_crs = FALSE, try_hard = FALSE, 
						 nms = names(list(...)), tolerance = sqrt(.Machine$double.eps)) {
	dots = list(...)
	get_file_dim = function(dots) {
			do.call(rbind, lapply(dots, attr, "file_dim"))
	}
	if (!all(sapply(dots, function(x) inherits(x, "stars_proxy"))))
		stop("all arguments to c() should be stars_proxy objects")
	rio = attr(dots[[1]], "RasterIO")

	# Case 1: merge attributes of several objects by simply putting them together in a single stars object;
	# dim does not change:
	if (length(dots) == 1 && length(along) == 1 && is.na(along)) # do nothing
		dots[[1]]
	else if (along_crs)
		combine_along_crs_proxy(dots)
	else if (identical(along, NA_integer_)) { 
		if (identical_dimensions(dots))
			st_stars_proxy(setNamesIfnn(do.call(c, lapply(dots, unclass)), nms),
						   dimensions = st_dimensions(dots[[1]]), 
						   NA_value = attr(dots[[1]], "NA_value"), 
						   resolutions = NULL,
						   file_dim = get_file_dim(dots),
						   RasterIO = rio)
		else if (identical_dimensions(dots, ignore_resolution = TRUE, tolerance = tolerance)) {
			dots = add_resolution(dots)
			st_stars_proxy(setNamesIfnn(do.call(c, lapply(dots, unclass)), nms),
						   dimensions = st_dimensions(dots[[1]]), 
						   resolutions = attr(dots, "resolutions"),
						   NA_value = attr(dots[[1]], "NA_value"), 
						   file_dim = get_file_dim(dots),
						   RasterIO = rio)
		} else {
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
				if (!is.null(nms))
					nms = nms[ident]
				st_stars_proxy(setNamesIfnn(do.call(c, lapply(dots[ident], unclass)), nms),
							   dimensions = st_dimensions(dots[[1]]), 
							   NA_value = attr(dots[[1]], "NA_value"), 
							   resolutions = NULL, 
							   file_dim = get_file_dim(dots),
							   RasterIO = rio)
			}
		}
	} else { # arrange along "along" dimension:
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
			st_stars_proxy(ret, dimensions = dims, NA_value = attr(dots[[1]], "NA_value"),
				resolutions = NULL,
				file_dim = get_file_dim(dots),
				RasterIO = rio)
		}
	}
}

combine_along_crs_proxy = function(dots) {
	crs = lapply(l, st_crs)
	l = lapply(dots, st_set_crs, value = NA)
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
	st_stars_proxy(setNames(ret, paste(names(x), collapse = ".")), dimensions = dims,
		NA_value = attr(x, "NA_value"), resolutions = attr(x, "resolutions"))
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
		nBufXSize = ceiling(nBufXSize / (downsample[1] + 1))
	if (downsample[2] > 0)
		nBufYSize = ceiling(nBufYSize / (downsample[2] + 1))

	# issue #438:
	if (any(downsample > 0) && !is.null(attr(x, "RasterIO")))
		warning("with RasterIO defined, argument downsample is ignored")
	rasterio = attr(x, "RasterIO") %||% list(nXOff = dx$from, nYOff = dy$from, 
			nXSize = nXSize, nYSize = nYSize, nBufXSize = nBufXSize, nBufYSize = nBufYSize)

	# select bands?
	bands <- d[["band"]]
	if (!is.null(bands)) {
		if (!is.null(bands$values) && is.numeric(bands$values)) 
			rasterio$bands = bands$values
		else if (!is.na(bands$from) && !is.na(bands$to) 
				 # && (bands$to - bands$from + 1) < length(x[[1]])
			)
			rasterio$bands = seq(bands$from, bands$to)
		if (!is.null(rasterio$bands) && length(rasterio$bands) > 1 && 
				length(rasterio$bands) == length(x[[1]])) # one band in each file
			rasterio$bands = NULL # https://github.com/r-spatial/stars/issues/608
	}

	# do it:
	ret = vector("list", length(x))
	res <- attr(x, "resolutions")
	for (i in seq_along(ret)) {
		if (!is.null(res) && any(res[1,] != res[i,])) {
			mult = c(res[i,1] / res[1,1], res[i,2] / res[1,2])
			rasterio$nXOff = floor((dx$from - 1) / mult[1]) + 1
			rasterio$nYOff = floor((dy$from - 1) / mult[2]) + 1
			rasterio$nXSize = ceiling(dx$to / mult[1]) - floor((dx$from - 1) / mult[1])
			rasterio$nYSize = ceiling(dy$to / mult[2]) - floor((dy$from - 1) / mult[2])
			rasterio$nBufXSize = ceiling(rasterio$nXSize * mult[1] / (downsample[1] + 1))
			rasterio$nBufYSize = ceiling(rasterio$nXSize * mult[2] / (downsample[2] + 1))
			mod = function(a, n) { a - n * floor(a/n) }
			offset = round(c(mod(dx$from - 1, mult[1]), mod(dy$from - 1, mult[2])))
		} else
			offset = c(0,0)
		file_name = unclass(x)[[i]]
		if (is.function(file_name)) # realise/evaluate:
			file_name = file_name()
		ret[[i]] = read_stars(file_name, RasterIO = rasterio, 
			NA_value = attr(x, "NA_value") %||% NA_real_, normalize_path = FALSE,
			proxy = FALSE, ...)
		if (i == 1)
			dm1 = dim(ret[[1]])
		else {
			xrange = seq_len(dm1[1]) + offset[1]
			yrange = seq_len(dm1[2]) + offset[2]
			ret[[i]] = ret[[i]] [ , xrange, yrange ]
			st_dimensions(ret[[i]]) = st_dimensions(ret[[1]])
		}
	}

	along = if (length(dim(x)) > 3)
			setNames(list(st_get_dimension_values(x, 4)), tail(names(st_dimensions(x)), 1))
		else
			list(new_dim = names(x))
	
	ret = if (length(ret) == 1)
		st_redimension(ret[[1]], name = along)
	else
		do.call(c, lapply(ret, st_redimension, along = along))
	
	new_dim = st_dimensions(ret)
#	for (dm in setdiff(names(d), xy)) # copy over non x/y dimension values, if present:
#		if (dm %in% names(new_dim))
#			new_dim[[dm]] = d[[dm]]
	if (length(d) > 2)
		for (dm in 3:length(d)) {
			new_dim[[dm]] = d[[dm]] # copy all fields - what if this was downsampled?
			names(new_dim)[dm] = names(d)[dm]
		}

	ret = unclass(ret)
	for (i in seq_along(ret)) {
		file_dim = attr(x, "file_dim")
		if (is.null(bands) && !is.null(file_dim) && ncol(file_dim) == length(dim(new_dim)) 
				&& ncol(file_dim) == 3) { # https://github.com/r-spatial/stars/issues/596
			r = new_dim[[3]]$from:new_dim[[3]]$to # FIXME: or else use $values?
			ret[[i]] = ret[[i]][,,r]
		}
		dim(ret[[i]]) = dim(new_dim)
	}
	adrop(st_set_crs(st_stars(setNames(ret, names(x)), new_dim), st_crs(x)))
}

check_xy_warn = function(call, dimensions) {
	if (as.character(as.list(call)[[1]]) == "st_apply") {
		# check dims
		MARGIN = as.list(call)$MARGIN
		if (inherits(MARGIN, "call"))
			MARGIN = eval(MARGIN, environment(call))
		if (inherits(MARGIN, "name"))
			MARGIN = get("MARGIN", environment(call))
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
st_as_stars.stars_proxy = function(.x, ..., downsample = 0, url = attr(.x, "url"), 
		envir = parent.frame()) {
	if (! is.null(url)) { # execute/get remotely: # nocov start
		# if existing, convert call_list to character:
		attr(.x, "call_list") = lapply(attr(.x, "call_list"), deparse)
		# push the object to url, then st_as_stars() it there:
		tempnam = substr(tempfile(pattern = "Z", tmpdir = "", fileext = ""), 2, 15)
		put_data_url(url, tempnam, .x)
		expr = paste0("st_as_stars(", tempnam, ", url = NULL, downsample=", downsample, 
			", envir = data)") # evaluate in "data" first
		ret = get_data_url(url, expr)
		put_data_url(url, tempnam, NULL) # remove the temporary object
		ret # nocov end
	} else {
		cl = attr(.x, "call_list")
		# FIXME: this means we ALWAYS process after (possibly partial) reading; 
		# there are cases where this is not right. Hence:
		# TODO: only warn when there is a reason to warn.
		if (!all(downsample == 0))
			lapply(attr(.x, "call_list"), check_xy_warn, dimensions = st_dimensions(.x))
		process_call_list(fetch(.x, ..., downsample = downsample), cl, envir = envir, downsample = downsample)
	}
}

st_as_stars_proxy = function(x, fname = tempfile(fileext = rep_len(".tif", length(x))),
		quiet = TRUE, NA_value = NA_real_) {
	stopifnot(inherits(x, "stars"))
	if (inherits(x, "stars_proxy"))
		return(x)
	for (i in seq_along(x))
		write_stars(x[i], fname[i], NA_value = NA_value)
	if (!quiet)
		cat(paste("writing to", fname, "\n"))
	st_stars_proxy(setNames(as.list(fname), names(x)), st_dimensions(x), 
		NA_value = NA_value, resolutions = NULL)
}

# execute the call list on a stars object
process_call_list = function(x, cl, envir = new.env(), downsample = 0) {
	for (i in seq_along(cl)) {
		if (is.character(cl[[i]]))
			cl[[i]] = parse(text = cl[[i]])[[1]]
		stopifnot(is.call(cl[[i]]))
		env = environment(cl[[i]])
		env [[ names(cl[[i]])[2] ]] = x # here, a stars_proxy may be replaced with the fetched stars object
		old_downsample = env$downsample # might be NULL
		if (!is.null(env$downsample) && any(env$downsample != downsample)) {
			cat(paste0("overriding downsample of (sub)expression to c(", paste(downsample, collapse = ","), ")\n"))
			env$downsample = downsample
		}
		# so we need to do that for other args too: https://github.com/r-spatial/stars/issues/390 :
		if ("e2" %in% names(env) && inherits(env$e2, "stars_proxy")) # binary ops: also fetch the second arg
			env$e2 = st_as_stars(env$e2, downsample = downsample)
		x = eval(cl[[i]], env, parent.frame())
		env$downsample = old_downsample
	}
	x
}

# add a call to the call list, possibly replacing function name (fn) and first arg name
collect = function(x, call, fn, args = "x", env, ...) {
	call_list = attr(x, "call_list") %||% list()
	dots = list(...)
	nd = names(dots)
	# I would say now to do
	# env = as.environment(append(as.list(env), dots)) -> but that didn't work.
	# so we iterate over ... :
	for (i in seq_along(dots))
		env[[ nd[i] ]] = dots[[i]]
	args = c(args, nd)
	# set function to call:
	lst = as.list(call)
	if (!missing(fn))
		lst[[1]] = as.name(fn)
	# set argument names:
	if (!missing(fn) && fn == "[") {
		lst[[2]] = as.name(args[1])
		lst[[3]] = as.name(args[2])
		for (i in seq_along(args)[-(1:2)]) {
			if (!args[i] %in% names(lst))
				lst[[ args[i] ]] = as.name(args[i]) # appends
		}
	} else {
		for (i in seq_along(args)) {
			lst[[i+1]] = as.name(args[i])
			names(lst)[[i+1]] = args[i]
		}
	}
	call = as.call(lst)
	environment(call) = env
	structure(x, call_list = c(call_list, call))
}

#' @export
adrop.stars_proxy = function(x, drop = which(dim(x) == 1), ...) {
	collect(x, match.call(), "adrop", c("x", "drop"), env = environment(), ...)
}

#' @export
aperm.stars_proxy = function(a, perm = NULL, ...) {
	collect(a, match.call(), "aperm", c("a", "perm"), env = environment(), ...)
}

#' @export
is.na.stars_proxy = function(x) {
	collect(x, match.call(), "is.na", "x", env = environment())
}

#' @name stars_subset
#' @export
"[<-.stars_proxy" = function(x, i, downsample = 0, value) {
	# https://stackoverflow.com/questions/9965577/copy-move-one-environment-to-another
	# copy the environment, to avoid side effect later on:
	# FIXME: to investigate - should this be done to env in every call to collect()?
	env = as.environment(as.list(environment(), all.names = TRUE)) # copies
	parent.env(env) = parent.env(environment())
	collect(x, match.call(), "[<-", c("x", "i", "value", "downsample"), env)
}


#' @export
split.stars_proxy = function(x, ...) {
	collect(x, match.call(), "split", env = environment())
}

#' @export
merge.stars_proxy = function(x, y, ..., name = "attributes") {
	if (!missing(y))
		stop("argument y needs to be missing: merging attributes of x")
	if (!is.null(attr(x, "call_list")) || !is.null(attr(x, "resolutions"))) # postpone:
		collect(x, match.call(), "merge", c("x", "y", "name"), env = environment(), ...)
	else {
		if (length(x) > 1) { 
			cl = class(x)
			x = unclass(x)
			x[[1]] = unlist(x)
			x[2:length(x)] = NULL
			class(x) = cl
		}
		st_stars_proxy(x, dimensions = create_dimensions(append(st_dimensions(x), 
			list(band = create_dimension(values = names(x[[1]])))), 
			raster = attr(st_dimensions(x), "raster")), 
			NA_value = attr(x, "NA_value"),
			resolutions = attr(x, "resolutions"))
	}
}


#' @export
"[.stars_proxy" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
	get_range = function(expr) {
		v = try(eval(expr, parent.frame(2)), silent = TRUE)
		if (is.numeric(v) && all(diff(v) == 1))
			range(v)
		else
			NULL
	}
	rio = attr(x, "RasterIO")
	dim_orig = dim(x)
	mc = match.call()
	lst = as.list(mc)
	cl = attr(x, "call_list")
	if (length(lst) < 3)
		return(x) # 
	if (missing(i) | !"i" %in% names(lst)) # insert:
		lst = c(lst[1:2], i = TRUE, lst[-(1:2)])
	if (inherits(i, c("character", "logical", "numeric")) && is.null(cl)) {
		if (!is.null(unclass(x)[[i]])) { # can/should be selected now:
			if (!is.null(resolutions <- attr(x, "resolutions")))
				resolutions = resolutions[i, ]
			x = st_stars_proxy(unclass(x)[i], st_dimensions(x), NA_value = attr(x, "NA_value"),
				resolutions = resolutions, file_dim = attr(x, "file_dim"),
				RasterIO = rio)
			lst[["i"]] = TRUE # this one has been handled now
		}
		ix = 1
		while (length(lst) >= 4) { # https://github.com/r-spatial/stars/issues/496
			if (!is.null(r <- get_range(lst[[4]]))) {
				attr(x, "dimensions")[[ix]]$from = r[1]
				attr(x, "dimensions")[[ix]]$to = r[2]
				if(!is.null(attr(x, "dimensions")[[ix]]$values)) {
					attr(x, "dimensions")[[ix]]$values <- 
						attr(x, "dimensions")[[ix]]$values[r[1]:r[2]]
				}
			}
			ix = ix + 1
			lst[[4]] = NULL # eat/remove
		}
	} else if (crop && inherits(i, c("sf", "sfc", "stars", "bbox"))) {
		x = st_crop(x, i, ..., collect = FALSE) # does bounding box cropping only
		if (inherits(i, c("stars", "bbox")))
			lst[["i"]] = TRUE # this one has been handled now
	}

	# return or collect?
	file_dim = attr(x, "file_dim") %||% matrix(dim(x)[1:2], 1)
	n_file_dim = ncol(file_dim)
	if (length(lst) == 3 && isTRUE(lst[["i"]]) && is.null(cl) &&  # drop a number of files in the lists of files?
			length(x) && length(dim(x)) > n_file_dim && 
			length(x[[1]]) == prod(dim_orig[-(seq_len(ncol(file_dim)))])) { # https://github.com/r-spatial/stars/issues/561
		# select from the vectors of proxy object names:
		get_ix = function(d) {
			stopifnot(inherits(d, "dimension"))
			if (!is.na(d$from))
				seq(d$from, d$to)
			else
				d$values
		}
		d = st_dimensions(x)[-seq_len(n_file_dim)] # dimensions not in the file(s)
		e = do.call(expand.grid, lapply(dim_orig[-seq_len(n_file_dim)], seq_len)) # all combinations
		e$rn = seq_len(nrow(e)) # their index
		f = do.call(expand.grid, lapply(d, get_ix)) # the ones we want
		if (!requireNamespace("dplyr", quietly = TRUE))
			stop("package dplyr required, please install it first") # nocov
		sel = dplyr::inner_join(e, f, by = colnames(f))$rn
		for (i in seq_along(x)) # select:
			x[[i]] = x[[i]][sel]
		x
	} else { # still processing the geometries inside the bbox:
		if (length(lst) == 3 && isTRUE(lst[["i"]]) && is.null(cl))
			x
		else
			collect(x, as.call(lst), "[", c("x", "i", "drop", "crop"), 
				env = environment()) # postpone every arguments > 3 to after reading cells
	}
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
	dm = st_dimensions(x)
	d_max = dim(x) + sapply(dm, function(x) x$from) - 1
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
		# crop x:
		dm[[ xd ]]$from = max(1, cr[1, 1], na.rm = TRUE)
		dm[[ xd ]]$to = min(d_max[xd], cr[2, 1], na.rm = TRUE)
		if(!is.null(dm[[ xd ]]$values))
			dm[[ xd ]]$values = dm[[ xd ]]$values[dm[[ xd ]]$from:dm[[ xd ]]$to]
		
		# crop y:
		if (!is.na(dm[[ yd ]]$delta) && dm[[ yd ]]$delta < 0) # FIXME: just subtract values to avoid NA miss?
			cr[1:2, 2] = cr[2:1, 2]
		dm[[ yd ]]$from = max(1, cr[1, 2], na.rm = TRUE)
		dm[[ yd ]]$to = min(d_max[yd], cr[2, 2], na.rm = TRUE)
		if(!is.null(dm[[ yd ]]$values))
			dm[[ yd ]]$values = dm[[ yd ]]$values[dm[[ yd ]]$from:dm[[ yd ]]$to]
	}
	x = st_stars_proxy(x, dm, NA_value = attr(x, "NA_value"), resolutions = attr(x, "resolutions"),
		file_dim = attr(x, "file_dim")) # crop to bb
	if (collect)
		collect(x, match.call(), "st_crop", c("x", "y", "crop", "epsilon"),
			env = environment(), ...) # crops further when realised
	else
		x
}

#' @export
st_apply.stars_proxy = function(X, MARGIN, FUN, ..., CLUSTER = NULL, PROGRESS = FALSE, 
	FUTURE = FALSE, rename = TRUE, .fname) {
	mc = match.call()
	if (missing(.fname))
		.fname = as.character(mc[["FUN"]])[1]
	collect(X, mc, "st_apply", c("X", "MARGIN", "FUN", "CLUSTER", "PROGRESS", "FUTURE", 
		"rename", ".fname"), env = environment(), ...)
}

#' @export
#' @name predict.stars
predict.stars_proxy = function(object, model, ...) {
	collect(object, match.call(), "predict", c("object", "model"), env = environment(), ...)
}

#' @export
"[[<-.stars_proxy" = function(x, i, value) {
	y = unclass(x)
	y[[i]] = value
	structure(y, class = class(x))
}

#' @export
st_normalize.stars_proxy = function(x, domain = c(0, 0, 1, 1), ...) {
	stopifnot(all(domain == c(0,0,1,1)))
	d = st_dimensions(x)
	stopifnot(d[[1]]$from == 1, d[[2]]$from == 1)
	x
}

#' @export
image.stars_proxy <- function(x, ..., downsample = get_downsample(dim(x))) {
    image(st_as_stars(x, downsample = downsample), ...)
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
