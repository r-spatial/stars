# sf conversion things

#' @export
st_as_sfc.stars = function(x, ..., as_points = st_dimensions(x)$x$point) {
	st_as_sfc(structure(st_dimensions(x)[c("x", "y")], class = "dimensions"),
		..., as_points = as_points)
}

#' replace x y raster dimensions with simple feature geometry list (points or polygons)
#' @param x object of class \code{stars}
#' @param as_points logical; if \code{TRUE}, generate points at cell centers, else generate polygons
#' @param ... arguments passed on to \code{st_as_sfc}
#' @return object of class \code{stars} with x and y raster dimensions replaced by a single sfc geometry list column containing either points or square polygons
#' @export
st_xy2sfc = function(x, as_points = st_dimensions(x)$x$point, ...) {

	d = st_dimensions(x)
	olddim = dim(x)

	if (! has_raster(x))
		stop("x and/or y not among dimensions")

	xy_pos = match(c("x", "y"), names(d))
	if (! all(xy_pos == 1:2))
		stop("x and y need to be first and second dimension")

	stopifnot(identical(which(names(d) %in% c("x", "y")), 1:2))

	sfc = st_as_sfc(x, as_points = as_points, ...)
	# overwrite x:
	d[["x"]] = create_dimension(from = 1, to = length(sfc), values = sfc)
	# rename:
	names(d)[names(d) == "x"] = "sfc"
	# remove y:
	d[["y"]] = NULL
	for (i in seq_along(x))
		dim(x[[i]]) = c(length(sfc), olddim[-xy_pos])
	structure(x, dimensions = d)
}

#' @export
st_as_sf.stars = function(x, ..., as_points = st_dimensions(x)$x$point, na.rm = FALSE) {

	if (has_raster(x))
		x = st_xy2sfc(x, as_points = as_points, ..., na.rm = na.rm)

	if (!has_sfc(x))
		stop("no feature geometry column found")

	# FIXME: this probably only works for 2D arrays, now
	sfc = st_dimensions(x)$sfc$values
	# may choose units method -> is broken; drop units TODO: if fixed:
	dfs = lapply(x, function(y) as.data.frame(y))
	nc = sapply(dfs, ncol)
	df = do.call(cbind, dfs)
	if (length(dim(x)) > 1) {
		labels = format(expand_dimensions(st_dimensions(x))[[2]])
		names(df) = apply(expand.grid(labels, names(x))[,2:1], 1, paste0, collapse = " ")
	} else
		names(df) = names(x)
	if (na.rm) {
		keep = apply(df, 1, function(x) any(!is.na(x)))
		df = df[keep, ]
		sfc = sfc[keep]
	}
	st_sf(df, geometry = sfc)
}

#' @name st_stars
#' @param times time instances
#' @export
st_stars.sf = function(.x, ..., times = colnames(data[[1]])) {
	geom = st_geometry(.x)
	dots = list(...)
	data = if (length(dots)) {
			if (length(dots) == 1 && is.list(dots[[1]]))
				dots[[1]]
			else
				dots
		} else
			structure(list(as.matrix(st_set_geometry(.x, NULL))), names = deparse(substitute(.x)))
	dimensions = list(
		sfc = create_dimension(1, length(geom), refsys = st_crs(geom)$proj4string, values = geom),
		time = create_dimension(from = 1, to = ncol(data[[1]]), values = times))
	class(dimensions) = "dimensions"
	st_stars(data, dimensions = dimensions)
}
