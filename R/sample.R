# reduce resolution of x, keeping (most of) extent
st_downsample = function(x, n, fill_out = TRUE) {
	stopifnot(all(n >= 0))
	d = dim(x)
	n = rep(n, length.out = length(d))
	dims = st_dimensions(x)
	regular = is_regular_grid(x)
	if (! all(n <= 1)) {
		args = rep(list(rlang::missing_arg()), length(d)+1)
		for (i in seq_along(d))
			if (n[i] > 1)
				args[[i+1]] = seq(1, d[i], n[i])
		x = eval(rlang::expr(x[!!!args]))
		if (fill_out && regular) {
			d_new = st_dimensions(x)
			for (i in seq_along(d)) {
				dims[[i]]$delta = dims[[i]]$delta * n[i]
				dims[[i]]$from = d_new[[i]]$from
				dims[[i]]$to = d_new[[i]]$to
			}
			x = structure(x, dimensions = dims)
		}
	}
	x
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
	if (length(x) > 1)
		warning("only sampling the first attribute")
	if (type != "regular")
		warning("only type 'regular' for stars_proxy objects supported")
	d = dim(x)
	downsampling_rate = round(d / sqrt(size)) # right for x,y-only, oversamples in case of others
	if (!quiet)
		print(downsampling_rate) # nocov
	st = fetch(x, downsampling_rate)
	st_sample(st, size, ...)
}
