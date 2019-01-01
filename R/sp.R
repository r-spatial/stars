setAs("stars", "Spatial", function(from) { 
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") #nocov
	geom = if (has_raster(from)) {
		if (length(dim(from)) > 2)
			stop("stars object must have two (raster: x, y) dimensions")
		if (!is_regular(from))
			stop("only regular rasters can be converted to Spatial")
		d = st_dimensions(from)
		xy = attr(d, "raster")$dimensions
		if (!all(match(xy, names(d)) == 1:2))
			stop("x/y dimensions should be at pos 1 and 2")
		offset = c( d[[ 1 ]]$offset, d[[ 2 ]]$offset )
		delta = c( d[[ 1 ]]$delta, d[[ 2 ]]$delta )
		cells.dim = dim(from)
		if (delta[1] < 0)
			stop("negative x cell size not supported")
		if (delta[2] > 0)
			stop("only negative y delta supported")
		cellcentre.offset = c(offset[1] + 0.5 * delta[1],
			offset[2] + (cells.dim[2] - 0.5) * delta[2])
		gt = sp::GridTopology(cellcentre.offset, abs(delta), cells.dim)
		sp::SpatialGrid(gt, sp::CRS(st_crs(d[[ 1 ]]$refsys)$proj4string))
	} else {
		if (!has_sfc(from))
			stop("no feature dimension in stars object")
		as(st_dimensions(from)[[1]]$values, "Spatial")
	}
	sp::addAttrToGeom(geom, as.data.frame(lapply(from, function(y) structure(y, dim = NULL))), match.ID = FALSE)
})

#' @export
st_as_stars.Spatial = function(.x, ...) {
    if (!requireNamespace("sp", quietly = TRUE))
        stop("package sp required, please install it first") # nocov
	if (sp::gridded(.x)) {
		sp::fullgrid(.x) = TRUE
		gt = sp::gridparameters(.x)
		# st_as_stars(raster::stack(.x)) --- we can do better
		# UL corner:
		x = create_dimension(1, gt$cells.dim[1], 
			offset = gt$cellcentre.offset[1] - 0.5 * gt$cellsize[1], delta = gt$cellsize[1],
			refsys = st_crs(sp::proj4string(.x))$proj4string)
		y = create_dimension(1, gt$cells.dim[2], 
			offset = gt$cellcentre.offset[2] + (gt$cells.dim[2] - 0.5) * gt$cellsize[1], 
			delta = -gt$cellsize[2], refsys = st_crs(sp::proj4string(.x))$proj4string)
		d = create_dimensions(list(x = x, y = y), raster = get_raster(dimensions = c("x", "y")))
		lst = lapply(.x@data, function(x, dims) structure(x, dim = dims), dims = dim(d))
		st_stars(lst, d)
	} else
		st_as_stars(st_as_sf(.x))
}
