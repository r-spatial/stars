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
st_rasterize = function(sf, template = st_as_stars(st_bbox(sf), values = NA_real_, ...), 
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


#' @export
#' @param dims the column names or indexes that form the cube dimensions
#' @param xy the x and y raster dimensions
#' @param y_decreasing logical; if TRUE, (numeric) y values get a negative delta (decrease with increasing index)
#' @name st_as_stars
#' @examples
#' data(Produc, package = "plm")
#' st_as_stars(Produc, y_decreasing = FALSE)
st_as_stars.data.frame = function(.x, ..., dims = 1:2, xy = dims[1:2], y_decreasing = TRUE) {
	if (is.character(xy))
		xy = match(names(.x), xy)

	index = NULL
	dimensions = list()
	if (length(dims) >= 2) {
		for (i in dims) {
			v = .x[[i]]
			if (inherits(v, "sfc")) {
    			if (!requireNamespace("digest", quietly = TRUE))
        			stop("package digest required, please install it first") # nocov
				dig = sapply(v, digest::digest)
				uv = unique(dig) # don't sort
				ix = match(dig, uv)
			} else {
				suv = sort(unique(v), decreasing = length(xy) == 2 && i == xy[2])
				ix = match(v, suv)
			}
			index = cbind(index, ix)
			dimensions[[i]] = if (inherits(v, "sfc")) 
					create_dimension(values = v[match(uv, dig)])
				else
					create_dimension(values = suv)
		}
		names(dimensions) = names(.x)[dims]
	
		raster_xy = if (length(xy) == 2) names(.x)[xy] else c(NA_character_, NA_character_)
		d = create_dimensions(dimensions, raster = get_raster(dimensions = raster_xy))
		l = lapply(.x[-dims], function(x) {
				m = array(NA, dim = dim(d))
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
	st_as_stars(as.data.frame(x, ...)) # too simplistic?!
}
