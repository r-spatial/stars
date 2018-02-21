#' @export
st_as_stars.STFDF = function(.x, ...) {
	d = st_dimensions(
			sfc = st_as_sfc(.x@sp), # FIXME: doesn't do pixels well!?
			time = zoo::index(.x@time)
		)
	vals = lapply(.x@data, function(y) { dim(y) = dim(d); y })
	st_as_stars(vals, dimensions = d)
}

st_as_STFDF = function(x) {
	if (has_raster(x))
		x = st_xy2sfc(x)
	d = st_dimensions(x)
	e = expand_dimensions(d)
	if (length(d) > 2)
		stop("STIDF only supports spatial+temporal dimensions")
	x = data.frame(lapply(x, as.vector))
	spacetime::STFDF(as(d$sfc$values, "Spatial"), xts::xts(1:dim(d)[2], e$time), x)
}

setAs("stars", "STFDF", function(from) { 
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first")
    if (!requireNamespace("zoo", quietly = TRUE))
        stop("package zoo required, please install it first")
    if (!requireNamespace("xts", quietly = TRUE))
        stop("package xts required, please install it first")
    if (!requireNamespace("spacetime", quietly = TRUE))
        stop("package spacetime required, please install it first")
	st_as_STFDF(from)
})
