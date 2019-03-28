#' @export
st_as_stars.STSDF = function(.x, ...) st_as_stars(as(.x, "STFDF"), ...)


#' @export
st_as_stars.STFDF = function(.x, ...) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") #nocov
    if (!requireNamespace("zoo", quietly = TRUE))
        stop("package zoo required, please install it first") #nocov
	#ix = 1:prod(dim(.x)[c("space", "time")])
	d = if (sp::gridded(.x@sp)) {
			gp = sp::gridparameters(.x@sp)
			nx = gp[1,3]
			ny = gp[2,3]
			if (! sp::fullgrid(.x@sp)) { # SpatialPixels
				lst = vector("list", ncol(.x@data))
				for (j in seq_along(.x@data)) {
					px2vec = function(pix) {
						sp::fullgrid(pix) = TRUE
						as.vector(as.matrix(pix))
					}
					a = array(NA_real_, c(nx, ny, dim(.x)[2]))
					for (i in seq_len(dim(.x)[2]))
						a[,,i] = px2vec(.x[,i])
					lst[[j]] = as.vector(a)
				}
				.x@data = as.data.frame(setNames(lst, names(.x@data)))
			}
			# offs_x dx 0 offs_y 0 dy
			gt = c(gp[1,1] - gp[1,2]/2, gp[1,2], 0.0, gp[2,1] + (gp[2,3] - 0.5) * gp[2,2], 0.0, -gp[2,2])
			vals = lapply(seq_len(nrow(gp)), function(i) seq(gp[i,1], by = gp[i,2], length.out = gp[i,3]))
			create_dimensions(list(
				x = create_dimension(values = vals[[1]] - gp[1,2]/2),
				y = create_dimension(values = rev(vals[[2]]) + gp[2,2]/2),
				time = create_dimension(values = zoo::index(.x@time))),
				raster = get_raster())
		} else
			create_dimensions(list(
				sfc = create_dimension(values = st_as_sfc(.x@sp)), # FIXME: doesn't do SpatialPixels -> x/y
				time = create_dimension(values = zoo::index(.x@time))))
	vals = lapply(.x@data, function(y) { dim(y) = dim(d); y })
	st_set_crs(st_as_stars(vals, dimensions = d), sp::proj4string(.x@sp))
}

st_as_STFDF = function(x) {

    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") #nocov
    if (!requireNamespace("spacetime", quietly = TRUE))
        stop("package spacetime required, please install it first") #nocov
	rst = has_raster(x)
	d = st_dimensions(x)
	geom = if (rst) {
			x = st_upfront(x)
			time_dim = 3
			sp::geometry(as(adrop(x[,,,1]), "Spatial"))
		} else {
			x = st_upfront(x, which_sfc(x))
			time_dim = 2
			sp::geometry(as(adrop(x[,,1]), "Spatial"))
		}
	tm = st_get_dimension_values(x, time_dim, center = FALSE)

	st = suppressWarnings(spacetime::STF(geom, tm)) # would warn for SpatialGrid -> SpatialPixels

	sp::addAttrToGeom(st, as.data.frame(lapply(x, function(y) structure(y, dim = NULL))), match.ID = FALSE)
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
