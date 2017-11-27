# sf conversion things

# convert x/y gdal dimensions into a list of points, or a list of square polygons
#' @export
st_as_sfc.dimensions = function(x, ..., as_points = NA, use_cpp = FALSE) {

	stopifnot(identical(names(x), c("x", "y")))
	if (is.na(as_points))
		stop("as_points should be set to TRUE (`points') or FALSE (`polygons')")

	xy2sfc = function(cc, dm, as_points) { # form points or polygons from a matrix with corner points
		if (as_points)
			unlist(apply(cc, 1, function(x) list(sf::st_point(x))), recursive = FALSE)
		else {
			stopifnot(prod(dm) == nrow(cc))
			lst = vector("list", length = prod(dm - 1))
			for (y in 1:(dm[2]-1)) {
				for (x in 1:(dm[1]-1)) {
					i1 = (y - 1) * dm[1] + x      # top-left
					i2 = (y - 1) * dm[1] + x + 1  # top-right
					i3 = (y - 0) * dm[1] + x + 1  # bottom-right
					i4 = (y - 0) * dm[1] + x      # bottlom-left
					lst[[ (y-1)*(dm[1]-1) + x ]] = sf::st_polygon(list(cc[c(i1,i2,i3,i4,i1),]))
				}
			}
			lst
		}
	}

	y = x$y
	x = x$x
	stopifnot(identical(x$geotransform, y$geotransform))
	xy = if (as_points) # grid cell centres:
			expand.grid(x = seq(x$from, x$to) - 0.5, y = seq(y$from, y$to) - 0.5)
		else # grid corners: from 0 to n
			expand.grid(x = seq(x$from - 1, x$to), y = seq(y$from - 1, y$to))
	cc = xy_from_colrow(as.matrix(xy), x$geotransform)
	dims = c(x$to, y$to) + 1
	if (use_cpp)
		structure(CPL_xy2sfc(cc, dims, as_points), crs = st_crs(x$refsys), n_empty = 0L)
	else
		st_sfc(xy2sfc(cc, dims, as_points), crs = x$refsys)
}

#' @export
st_as_sfc.stars = function(x, ..., as_points = st_dimensions(x)$x$point) {
	st_as_sfc(structure(st_dimensions(x)[c("x", "y")], class = "dimensions"),
		..., as_points = as_points)
}

#' replace x y raster dimensions with simple feature geometry list (points or polygons)
#' @param x object of class \code{stars}
#' @param as_points logical; if \code{TRUE}, generate points at cell centers, else generate polygons
#' @param ... arguments passed on to \code{st_as_sfc}
#' @return object of class \code{stars} with x and y raster dimensions replace by sfc geometry list
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

	sfc = st_dimensions(x)$sfc$values
	# may choose units method -> is broken; drop units TODO: if fixed:
	dfs = lapply(x, function(y) as.data.frame(unclass(y))) 
	nc = sapply(dfs, ncol)
	df = do.call(cbind, dfs)
	names(df) = if (length(x) > 1)
			make.names(rep(names(x), nc), unique = TRUE)
		else
			colnames(dfs[[1]])
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
st_stars.sf = function(x, ..., times = colnames(data[[1]])) {
	geom = st_geometry(x)
	dots = list(...)
	data = if (length(dots)) {
			if (length(dots) == 1 && is.list(dots[[1]]))
				dots[[1]]
			else
				dots
		} else
			structure(list(as.matrix(st_set_geometry(x, NULL))), names = deparse(substitute(x)))
	dimensions = list(
		sfc = create_dimension(1, length(geom), refsys = st_crs(geom)$proj4string, values = geom),
		time = create_dimension(from = 1, to = ncol(data[[1]]), values = times))
	class(dimensions) = "dimensions"
	st_stars(data, dimensions = dimensions)
}
