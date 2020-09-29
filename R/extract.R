
#' Extract cell values at point locations
#'
#' Extract cell values at point locations, possibly using interpolation
#' @name st_extract
#' @export
#' @returns if \code{x} has more dimensions than only x and y (raster), an 
#' object of class \code{stars} with POINT geometries replacing x and y raster
#' dimensions; otherwise an object of \code{sf} with extracted values.
st_extract = function(x, ...) UseMethod("st_extract")

#' @export
#' @name st_extract
#' @param ... passed on to next method
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' r = read_stars(tif)
#' pnt = st_sample(st_as_sfc(st_bbox(r)), 10)
#' st_extract(r, pnt)
#' st_extract(r, pnt) %>% st_as_sf()
#' st_extract(r[,,,1], pnt)
st_extract.stars = function(x, pts, ...) {
	stopifnot(inherits(pts, c("sf", "sfc")))
	sf_column = attr(pts, "sf_column") %||% "geometry"
	stopifnot(all(st_dimension(pts) == 0))
	if (length(x) > 1)
		x = merge(x)
	#st_extract(st_as_stars_proxy(x), ...)
	attr_name = names(x)
	x = st_upfront(x)
	ar = array(x[[1]], dim = c(prod(dim(x)[1:2]), prod(dim(x)[-(1:2)])))
	cr = colrow_from_xy(st_coordinates(pts), x)
	i = (cr[,2] - 1) * dim(x)[1] + cr[,1]
	res = ar[i, , drop=FALSE]
	if (ncol(res) > 1) {
		res = array(res, c(nrow(res), dim(x)[-(1:2)]))
		d = structure(st_dimensions(x),
			raster = get_raster(dimensions = rep(NA_character_,2)))
		d[[1]] = create_dimension(values = st_geometry(pts))
		d[[2]] = NULL
		names(d)[1] = sf_column
		setNames(st_as_stars(res, d), attr_name)
	} else {
		df = setNames(data.frame(as.vector(res)), attr_name)
		df[[sf_column]] = st_geometry(pts)
		st_as_sf(df)
	}
}

#' @param x object of class \code{stars} or \code{stars_proxy}
#' @param pts object of class \code{sf} or \code{sfc} with POINT geometries
#' @param method interpolation method, see \link{st_warp}
#' @param cellsize numeric; cellsize chosen for the sampling cell.
#' @param debug logical; if \code{TRUE}, do not remove the destination grid file and print its name;
#' @name st_extract
#' @export
st_extract.stars_proxy = function(x, pts, ..., method = 'near', cellsize = 1e-7, debug = FALSE) {
	stopifnot(inherits(pts, c("sf", "sfc")))
	stopifnot(all(st_dimension(pts) == 0))
	
	pts = st_geometry(pts)
	if (st_crs(pts) != st_crs(x))
		pts = st_transform(pts, st_crs(x))
	lst = vector("list", length(pts))
	tmp = tempfile(fileext = ".tif")
	if (debug)
		print(tmp)
	else
		on.exit(unlink(tmp))

	# prepare input imagery:
	if (length(x) > 1) # merge:
		x = merge(x)
	if (length(x[[1]]) > 1) { # merge into a single file:
		out_file = tempfile(fileext = ".vrt")
		gdal_utils("buildvrt", x[[1]], out_file, options = "-separate")
		x[[1]] = out_file
	}

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
