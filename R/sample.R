get_index_ranges = function(d, n, offset) {
	nd = (d - offset) %/% (n + 1)
	g = do.call(expand.grid, lapply(nd, seq_len))
	g = t(apply(g, 1, function(x) offset + 1 + (x - 1) * (n + 1)))
	dimnames(g) = NULL
	f = function(x) mapply(seq, x, length.out = n + 1, SIMPLIFY = FALSE)
	apply(g, 1, f, simplify = FALSE)
}

# reduce resolution of x, keeping (most of) extent
#' @export
#' @name st_downsample
st_downsample = function(x, n, ...) UseMethod("st_downsample")

#' downsample stars or stars_proxy objects
#'
#' downsample a stars or stars_proxy object either by skipping rows, columns and bands,
#' or by computing a single value (e.g. the mean) from the sub-tiles involved
#' 
#' @param x object of class stars or stars_proxy
#' @param n integer; for each dimension the number of pixels/lines/bands etc that will be skipped; see Details.
#' @param offset integer; offset(s) for downsampling, in pixels, starting at the offset of 
#' each dimension; should be smaller or equal to \code{n}
#' @param FUN function; if given, downsampling will apply FUN to each of the the subtiles
#' @param ... arguments passed on to \code{FUN} (e.g., \code{na.rm = TRUE} to ignore missing values if FUN is \code{mean})
#' @details If all n == 0, no downsampling takes place; if it is 1, every second row/column/band
#' is skipped, if it is 2, every second+third row/column/band are skipped, etc.
#' 
#' Downsampling a \code{stars_proxy} object returns a \code{stars} object, is
#' equivalent to calling \code{st_as_stars(x, downsample = 2)}, and only downsamples
#' the first two (x and y) dimensions.
#'
#' Downsampled regular rasters keep their dimension offsets, have a cell size (delta) that
#' is n[i]+1 times larger, and may result in a (slightly) different extent.
#'
#' Note that terra's \link[terra]{aggregate} with \code{fact=2} corresponds to
#' \code{st_downsample(x, n = 1, FUN = mean)}: \code{fact} is one larger than \code{n}.
#' @name st_downsample
#' @export
#' @examples
#' (m = matrix(1:121, 11, 11))
#' (s = st_as_stars(m))
#' st_downsample(s, 1)
#' st_downsample(s, 1)[[1]]
#' st_downsample(s, 1, offset = 1)
#' st_downsample(s, 1, offset = 1)[[1]]
#' st_downsample(s, 1, offset = c(0,1))
#' st_downsample(s, 1, offset = c(0,1))[[1]]
#' st_downsample(s, 1, FUN = mean)
#' st_downsample(s, 1, FUN = mean)[[1]]
#' st_downsample(s, 1, offset = 1, FUN = mean)
#' st_downsample(s, 1, offset = c(0,1), FUN = mean)[[1]]
st_downsample.stars = function(x, n, ..., offset = 0, FUN) {

	d = dim(x)
	n = rep_len(as.integer(n), length(d))
	offset = rep_len(as.integer(offset), length(d))
	stopifnot(all(n >= 0), all(offset <= n), all(offset < d))
	new_dim = if (! missing(FUN))
			(d - offset) %/% (n + 1)
		else
			ceiling((d - offset) / (n + 1)) # include last, incomplete tile

	dims = st_dimensions(x)
	ix = setNames(vector("list", length(dims)), names(dims))
	for (i in seq_along(d)) # need to precompute:
		ix[[i]] = seq(1 + offset[i], d[i], n[i] + 1)
	xy = attr(dims, "raster")$dimensions
	for (i in seq_along(d)) {
		if (is_CFTime(dims[[i]]$refsys)) {
			dims[[i]]$to = unname(new_dim[i])
			time = dims[[i]]$values
			bnds = CFtime::bounds(time)
			time = CFtime::CFtime(CFtime::definition(time), 
								  CFtime::calendar(time), 
								  CFtime::offsets(time)[ ix[[i]] ])
			if (!is.null(bnds))
				CFtime::bounds(time) <- bnds[, ix[[i]] ]
			dims[[i]]$values = time
		} else {
			dims[[i]]$offset = if (offset[i] != 0)
					dims[[i]]$offset + offset[i] * dims[[i]]$delta
				else
					dims[[i]]$offset = dims[[i]]$offset
			dims[[i]]$delta = dims[[i]]$delta * (n[i] + 1)
			dims[[i]]$from = 1
			dims[[i]]$to = unname(new_dim[i])
			if (!is.null(dims[[i]]$values)) {
				if (is.matrix(dims[[i]]$values) && names(ix)[i] %in% xy)
					dims[[i]]$values = dims[[i]]$values[ ix[[ xy[1] ]], ix[[ xy[2] ]] ] # speaks for itself 
				else
					dims[[i]]$values = dims[[i]]$values[ ix[[i]] ]
			}
		}
	}
	if (!all(attr(dims, "raster")$affine == 0.0)) {
		if (!all(xy %in% names(d)[1:2]))
			stop("downsampling an affine raster needs to have x/y in dims 1 and 2")
		attr(dims, "raster")$affine = attr(dims, "raster")$affine * (n[1:2] + 1)
	}

	if (all(n == 0))
		x
	else if (!missing(FUN)) { # compute FUN() values over the subtiles:
		stopifnot(is.function(FUN))
		l = get_index_ranges(d, n, offset)
		x = unclass(x) # so that `[[<-` doesn't go into the stars method, which recycles
		for (i in seq_along(x))
			x[[i]] = structure(sapply(l, function(y) FUN(as.vector(asub(x[[i]], y)), ...)),
							   dim = new_dim)
		structure(x, class = "stars", dimensions = dims)
	} else { # downsample using `[`:
		args = rep(list(rlang::missing_arg()), length(d)+1)
		for (i in seq_along(d))
			if (n[i] > 0)
				args[[i+1]] = ix[[i]] # i==1 skipped: retain all attributes
		x = eval(rlang::expr(x[!!!args]))
		structure(x, dimensions = dims)
	}
}

#' @export
#' @name st_downsample
st_downsample.stars_proxy = function(x, n, ...) {
	if (length(n) != 2)
		message("for stars_proxy objects, downsampling only happens for dimensions x and y")
	st_as_stars(x, downsample = n)
}


#' @export
st_sample.stars = function(x, size, ..., type = "random", replace = FALSE) {
	if (length(x) > 1)
		warning("only sampling the first attribute")
	if (type != "random")
		warning("only type 'random' supported")
	v = structure(x[[1]], dim = NULL)
	if (missing(replace))
		replace = size > length(v)
	v[sample(length(v), size, replace = replace, ...)]
}

#' @export
st_sample.stars_proxy = function(x, size, ..., type = "regular", quiet = TRUE) {
	if (type != "regular")
		stop("only type 'regular' for stars_proxy objects supported") # FIXME: tbd
	d = dim(x)
	downsampling_rate = c(floor(d[1:2] / sqrt(size)), d[-(1:2)])
	if (!quiet)
		print(downsampling_rate) # nocov
	st_as_stars(x, downsample = downsampling_rate)
	# st_sample(st, size, ...)
}
