
#' Extract cell values at point locations
#'
#' Extract cell values at point locations, possibly using interpolation
#' @name st_extract
#' @export
#' @returns if \code{x} has more dimensions than only x and y (raster), an 
#' object of class \code{stars} with POINT geometries replacing x and y raster
#' dimensions; otherwise an object of \code{sf}.
#' @details works on the first attribute only
st_extract = function(x, ...) UseMethod("st_extract")

#' @export
#' @name st_extract
#' @param ... passed on to next method
st_extract.stars = function(x, ...) {
	st_extract(st_as_stars_proxy(x), ...)
}

#' @param x object of class \code{stars} or \code{stars_proxy}
#' @param pts object of class \code{sf} or \code{sfc} with POINT geometries
#' @param method interpolation method, see \link{st_warp}
#' @param cellsize numeric; cellsize chosen for the sampling cell.
#' @name st_extract
#' @export
st_extract.stars_proxy = function(x, pts, ..., method = 'near', cellsize = 1e-7) {
	stopifnot(inherits(pts, c("sf", "sfc")))
	stopifnot(all(st_dimension(pts) == 0))
	
	pts = st_geometry(pts)
	if (st_crs(pts) != st_crs(x))
		pts = st_transform(pts, st_crs(x))
	lst = vector("list", length(pts))
	tmp = tempfile(fileext = ".tif")
	on.exit(unlink(tmp))
	nz = ifelse(length(dim(x)) == 2, 1, prod(dim(x)[-(1:2)])) # FIXME:? assumes x/y = 1&2
	halfcellsize = cellsize / 2
	for (i in seq_along(pts)) {
		# write pt
		pt = pts[[i]]
		bb = st_bbox(setNames(c(pt[1] - halfcellsize, pt[2] - halfcellsize, 
			pt[1] + halfcellsize, pt[2] + halfcellsize), c("xmin", "ymin", "xmax", "ymax")))
		s = st_as_stars(bb, nx = 1, ny = 1, nz = nz)
		write_stars(s, tmp)
		# warp x to pt
		sf::gdal_utils("warper", x[[1]], tmp, method)
		# read result, add:
		lst[[i]] = read_stars(tmp)
	}
	m = t(sapply(lst, function(x) x[[1]]))
	if (inherits(x[[1]], "factor"))
		m = structure(m, levels = levels(x[[1]]), colors = attr(x[[1]], "colors"), class = "factor")
	if (nz == 1) # single band:
		st_set_geometry(setNames(as.data.frame(t(m)), names(x)), pts)
	else { # multi-dimensional: return stars
		dim(m) = c(length(pts), dim(x)[-(1:2)])
		d = create_dimensions(append(list(sfc = create_dimension(values = pts)),
			st_dimensions(x)[-(1:2)]))
		st_as_stars(setNames(list(m), names(x)), dimensions = d)
	}
}
