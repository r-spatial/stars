as.owin.stars = function(W, ..., fatal) {
	if (isTRUE(st_is_longlat(W)))
		stop("Only projected coordinates may be converted to spatstat class objects")
	if (!requireNamespace("spatstat", quietly = TRUE))
		stop("package spatstat required: install first?")
	if (length(dim(W)) != 2)
		stop("as.owin.stars requires a 2-dimensional object (single raster layer)")
	m = t(W[[1]])
	bb = st_bbox(W)
	spatstat::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")], mask = m[nrow(m):1,])
}
