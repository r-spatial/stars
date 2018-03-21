#' @export
st_as_stars.STFDF = function(.x, ...) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") #nocov
    if (!requireNamespace("zoo", quietly = TRUE))
        stop("package zoo required, please install it first") #nocov
	d = if (sp::gridded(.x@sp)) {
			gp = sp::gridparameters(.x@sp)
			# offs_x dx 0 offs_y 0 dy
			gt = c(gp[1,1] - gp[1,2]/2, gp[1,2], 0.0, gp[2,1] + (gp[2,3] - 0.5) * gp[2,2], 0.0, -gp[2,2])
			vals = lapply(seq_len(nrow(gp)), function(i) seq(gp[i,1], by = gp[i,2], length.out = gp[i,3]))
			st_dimensions(
				x = vals[[1]] - gp[1,2]/2,
				y = rev(vals[[2]]) + gp[2,2]/2,
				time = zoo::index(.x@time),
				geotransform = gt # FIXME: somewhere we should check that gt, x and y are compatible!
			)
		} else
			st_dimensions(
				sfc = st_as_sfc(.x@sp), # FIXME: doesn't do SpatialPixels -> x/y
				time = zoo::index(.x@time)
			)
	vals = if (sp::gridded(.x@sp)) # flip y:
			lapply(.x@data, function(y) { dim(y) = dim(d); y[,(dim(y)[2]):1,] })
		else
			lapply(.x@data, function(y) { dim(y) = dim(d); y })
	st_set_crs(st_as_stars(vals, dimensions = d), sp::proj4string(.x@sp))
}

st_as_STFDF = function(x) {
	rst = has_raster(x)
	vals = x
	if (rst)
		x = st_xy2sfc(x, as_points = TRUE, na.rm = FALSE)
	d = st_dimensions(x)
	e = expand_dimensions(d)
	if (length(d) > 2)
		stop("STIDF only supports spatial+temporal dimensions")
	sp = as(d$sfc$values, "Spatial")
	if (rst)
		sp::gridded(sp) = TRUE
	spacetime::STFDF(sp, xts::xts(1:dim(d)[2], e$time), data.frame(lapply(vals, as.vector)))
}

setAs("stars", "STFDF", function(from) { 
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") #nocov
    if (!requireNamespace("zoo", quietly = TRUE))
        stop("package zoo required, please install it first") #nocov
    if (!requireNamespace("xts", quietly = TRUE))
        stop("package xts required, please install it first") #nocov
    if (!requireNamespace("spacetime", quietly = TRUE))
        stop("package spacetime required, please install it first") #nocov
	st_as_STFDF(from)
})
