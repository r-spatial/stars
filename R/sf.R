# sf conversion things

# convert x/y gdal dimensions into a list of points, or a list of square polygons
#' @export
st_as_sfc.dimensions = function(x, ..., as_points = TRUE) {

	stopifnot(identical(names(x), c("x", "y")))
	form_polys = function(cc, dm) { # form square polygons from a long matrix with corner points
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

	y = x$y
	x = x$x
	stopifnot(identical(x$geotransform, y$geotransform))
	xy = if (as_points) # grid cell centres:
			expand.grid(x = seq(x$from, x$to) - 0.5, y = seq(y$from, y$to) - 0.5)
		else # grid corners: from 0 to n
			expand.grid(x = seq(x$from - 1, x$to), y = seq(y$from - 1, y$to))
	cc = xy_from_colrow(as.matrix(xy), x$geotransform)
	lst = if (as_points)
			unlist(apply(cc, 1, function(x) list(sf::st_point(x))), recursive = FALSE)
		else
			form_polys(cc, c(x$to, y$to) + 1)
	st_sfc(lst, crs = x$refsys)
}

#' @export
st_as_sfc.stars = function(x, ..., as_points = TRUE) {
	st_as_sfc(structure(st_dimensions(x)[c("x", "y")], class = "dimensions"),
		..., as_points = as_points)
}

#' replace x y raster dimensions with simple feature geometry list (points or polygons)
#' @param x object of class \code{stars}
#' @param as_points logical; if \code{TRUE}, generate points at cell centers, else generate polygons
#' @return object of class \code{stars} with x and y raster dimensions replace by sfc geometry list
#' @export
st_xy2sfc = function(x, as_points = TRUE) {

	d = st_dimensions(x)

	if (!all(c("x", "y") %in% names(d)))
		stop("x and/or y not among dimensions")

	stopifnot(identical(which(names(d) %in% c("x", "y")), 1:2))

	sfc = st_as_sfc(x, as_points = as_points)
	# overwrite x:
	d[["x"]] = create_dimension(from = 1, to = length(sfc), values = sfc)
	# rename:
	names(d)[names(d) == "x"] = "sfc"
	# remove y:
	d[["y"]] = NULL
	newdim = sapply(d, function(x) x$to)
	newdim = c(length(sfc), prod(newdim[-1]))
	for (i in seq_along(x))
		dim(x[[i]]) = newdim
	structure(x, dimensions = d, class = "stars")
}

#' @export
st_as_sf.stars = function(x, ..., as_points = TRUE) {
	x = st_xy2sfc(x, as_points = as_points)
	sfc = st_dimensions(x)$sfc$values
	dfs = lapply(x, as.data.frame)
	nc = sapply(dfs, ncol)
	df = do.call(cbind, dfs)
	names(df) = make.names(rep(names(x), nc), unique = TRUE)
	st_sf(df, geom = sfc)
}
