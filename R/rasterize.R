#' rasterize simple feature geometries
#' 
#' rasterize simple feature geometries
#' @export
#' @param sf object of class \code{sf}
#' @param template stars object with desired target geometry 
#' @param file temporary file name
#' @param driver driver for temporary file
#' @param options character; options vector for \code{GDALRasterize}
#' @param ... arguments passed on to \link{st_as_stars}
#' @examples
#' demo(nc, echo = FALSE, ask = FALSE)
#' (x = st_rasterize(nc)) # default grid:
#' plot(x, axes = TRUE)
#' # a bit more customized grid:
#' (x = st_rasterize(nc, st_as_stars(st_bbox(nc), nx = 100, ny = 50, values = NA_real_)))
#' plot(x, axes = TRUE)
#' (ls = st_sf(a = 1:2, st_sfc(st_linestring(rbind(c(0.1, 0), c(1.1, 1))),
#'    st_linestring(rbind(c(0, 0.05), c(1, 0.05))))))
#' (grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0, 1.0), ylim = c(0, 1),
#'    values = NA_real_))
#' # Only the left-top corner is part of the grid cell:
#' sf_extSoftVersion()["GDAL"]
#' plot(st_rasterize(ls, grd), axes = TRUE, reset = FALSE) # ALL_TOUCHED=FALSE; 
#' plot(ls, add = TRUE, col = "red")
#' plot(st_rasterize(ls, grd, options = "ALL_TOUCHED=TRUE"), axes = TRUE, reset = FALSE)
#' plot(ls, add = TRUE, col = "red")
#' # add lines to existing 0 values, summing values in case of multiple lines:
#' (grd = st_as_stars(st_bbox(ls), nx = 10, ny = 10, xlim = c(0, 1.0), ylim = c(0, 1), values = 0))
#' r = st_rasterize(ls, grd, options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE"))
#' plot(r, axes = TRUE, reset = FALSE)
#' plot(ls, add = TRUE, col = "red")
st_rasterize = function(sf, template = guess_raster(sf, ...) %||% 
			st_as_stars(st_bbox(sf), values = NA_real_, ...), 
		file = tempfile(), driver = "GTiff", options = character(0), ...) {
	template = st_normalize(template)
	isn = sapply(sf, is.numeric)
	if (!any(isn)) {
		sf$ID = seq_len(nrow(sf))
		sf = sf["ID"]
	} else
		sf = sf[isn]
	sf::gdal_rasterize(sf, template, get_geotransform(template), file, driver, options)
	ret = read_stars(file, driver = driver)
	for (i in seq_along(ret)) {
		ret[[i]][is.nan(ret[[i]])] = NA_real_
		if (inherits(sf[[i]], "units"))
			units(ret[[i]]) = units(sf[[i]])
	}
	setNames(ret, names(sf)[1])
}

guess_raster = function(x, ...) {
	if (length(list(...)))
		return(NULL)
	if (all(st_dimension(x) == 0)) { # POINT
		cc = st_coordinates(x)
		ux = sort(unique(cc[,1]))
		uy = sort(unique(cc[,2]))
		dux = unique(diff(ux))
		if (length(dux) > 1) {
			if (var(dux)/mean(dux) < 1e-8)
				dux = mean(dux)
			else
				return(NULL)
		}
		duy = unique(diff(uy))
		if (length(duy) > 1) {
			if (var(duy)/mean(duy) < 1e-8)
				duy = mean(dux)
			else
				return(NULL)
		}
		if (length(ux) * length(uy) <= 2 * nrow(cc)) {
			bb = st_bbox(x)
			bb = st_bbox(setNames(c(bb["xmin"] - 0.5 * dux, 
				bb["ymin"] - 0.5 * duy, 
				bb["xmax"] + 0.5 * dux, 
				bb["ymax"] + 0.5 * duy), c("xmin", "ymin", "xmax", "ymax")), crs = st_crs(x))
			return(st_as_stars(bb, dx = dux, dy = duy))
		}
	}
	NULL
}


#' @export
#' @param dims the column names or indices that form the cube dimensions
#' @param coords same as dims, for symmetry with \link[sf]{st_as_sf}
#' @param xy the x and y raster dimension names or indices; only takes effect after dims has been specified
#' @param y_decreasing logical; if TRUE, (numeric) y values get a negative delta (decrease with increasing index)
#' @name st_as_stars
#' @examples
#' data(Produc, package = "plm")
#' st_as_stars(Produc, y_decreasing = FALSE)
st_as_stars.data.frame = function(.x, ..., dims = coords, xy = dims[1:2], y_decreasing = TRUE, coords = 1:2) {

	if (missing(dims) && !missing(xy))
		stop("parameter xy only takes effect when the cube dimensions are set with dims")
	if (is.character(xy))
		xy = match(xy, names(.x))
	if (any(is.na(xy)))
		stop("xy coordinates not found in data")

	index = NULL
	dimensions = list()
	if (length(dims) >= 2) {
		this_dim = 1
		for (i in dims) {
			v = .x[[i]]
			if (inherits(v, "sfc")) {
    			if (!requireNamespace("digest", quietly = TRUE))
        			stop("package digest required, please install it first") # nocov
				dig = sapply(v, digest::digest)
				uv = unique(dig) # no need to sort
				ix = match(dig, uv) # but look up "hash collision"
			} else {
				suv = sort(unique(v), decreasing = y_decreasing && i == xy[2])
				ix = match(v, suv)
			}
			index = cbind(index, ix)
			dimensions[[this_dim]] = if (inherits(v, "sfc"))
					create_dimension(values = v[match(uv, dig)])
				else
					create_dimension(values = suv, is_raster = TRUE)
			this_dim = this_dim + 1
		}
		names(dimensions) = names(.x)[dims]
	
		raster_xy = if (length(xy) == 2) names(.x)[xy] else c(NA_character_, NA_character_)
		d = create_dimensions(dimensions, raster = get_raster(dimensions = raster_xy))
		l = lapply(.x[-xy], function(x) {
				m = if (is.factor(x))
						structure(factor(rep(NA_character_, prod(dim(d))), levels = levels(x)),
							dim = dim(d))
					else 
						array(NA, dim = dim(d))
				m[index] = x # match order
				m 
			}
		)
	} else {
		l = lapply(.x, as.array)
		dimensions[[1]] = if (length(dims) == 0 || dims < 1)
				create_dimension(values = row.names(.x))
			else {
				l[[dims]] = NULL
				create_dimension(values = .x[[dims]])
			}
		names(dimensions) = "rows"
		d = create_dimensions(dimensions)
	}
	st_stars(l, d)
}

#' replace POINT simple feature geometry list with an x y raster
#' @param x object of class \code{stars}, or of class \code{sf}
#' @param ... passed on to \link{as.data.frame.stars}
#' @return object of class \code{stars} with a POINT list replaced by x and y raster dimensions. This only works when the points are distributed over a regular or rectilinear grid.
#' @export
st_sfc2xy = function(x, ...) {
	if (inherits(x, "sf"))
		x = st_as_stars(x)
	i = which_sfc(x)
	d = st_dimensions(x)
	if (!inherits(d[[i]]$values, "sfc_POINT"))
		stop("point geometries expected")
	cc = st_coordinates(d[[i]]$values)
	df = as.data.frame(x)
	df$geometry = NULL
	s = st_as_stars(cbind(cc, df))
	st_set_crs(s, st_crs(d))
}
