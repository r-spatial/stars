# align bb with the first two dimensions of d
st_align = function(bb, d) {
	up1 = function(x) ifelse(sign(x) < 0, -ceiling(abs(x)), floor(x))
	up2 = function(x) ifelse(sign(x) > 0, ceiling(abs(x)), -floor(abs(x)))
	dx = d[[1]]$delta
	dy = d[[2]]$delta
	ox = d[[1]]$offset
	oy = d[[2]]$offset
	dbbx = diff(bb[c("xmin", "xmax")])
	dbby = diff(bb[c("ymin", "ymax")])
	bb["xmin"] = ox - up2((ox - bb["xmin"])/dx) * dx
	bb["ymin"] = oy - up1((oy - bb["ymin"])/dy) * dy
	bb["xmax"] = ox - up1((ox - bb["xmax"])/dx) * dx
	bb["ymax"] = oy - up2((oy - bb["ymax"])/dy) * dy
	bb
}

#' rasterize simple feature geometries
#' 
#' rasterize simple feature geometries
#' @export
#' @param sf object of class \code{sf}
#' @param template stars object with desired target geometry, or target geometry alignment if \code{align=TRUE}
#' @param file temporary file name
#' @param driver driver for temporary file
#' @param options character; options vector for \code{GDALRasterize}
#' @param align logical; if \code{TRUE}, \code{template} contain the geometry alignment, 
#' informing target resolution and offset only.
#' @param proxy logical; should a proxy object be returned?
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
		file = tempfile(), driver = "GTiff", options = character(0),
		align = FALSE, proxy = FALSE, ...) {

	if (align) {
		if (missing(template))
			stop("align=TRUE requires template to be specified")
		stopifnot(is_regular_grid(template))
		bb = st_align(st_bbox(sf), d <- st_dimensions(st_normalize(template)))
		dx = d[[1]]$delta
		dy = d[[2]]$delta
		template = st_as_stars(bb, values = 0.0, dx = dx, dy = dy,
			nx = round(diff(bb[c("xmin", "xmax")])/dx), 
			ny = round(diff(bb[c("ymin", "ymax")])/dx),
			proxy = proxy)
	} else
		template = st_normalize(template)
	is_numeric = function(x) is.numeric(x) || is.factor(x)
	isn = sapply(sf, is_numeric)
	sf = if (!any(isn)) {
			sf$ID = seq_len(nrow(sf))
			sf["ID"]
		} else
			sf[isn]
	n_attr = length(which(isn))
	if (length(dim(template)) == 2 && n_attr > 1)
		template = merge(do.call(c, lapply(seq_len(n_attr), function(x) template)))
	geoms = which(sapply(sf, inherits, "sfc"))
	attrs = as.data.frame(sf)[-geoms]
	for (i in which(sapply(sf, inherits, "factor"))) # factors:
		sf[[i]] = as.numeric(sf[[i]])
	sf::gdal_rasterize(sf, template, get_geotransform(template), file, driver, options)
	ret = read_stars(file, driver = driver, proxy = proxy)
	if (!proxy) {
		if (length(dim(ret)) > 2)
			ret = split(ret)
		for (i in seq_along(ret)) {
			ret[[i]][is.nan(ret[[i]])] = NA_real_
			if (inherits(sf[[i]], "units"))
				units(ret[[i]]) = units(sf[[i]])
			if (is.factor(attrs[[i]]))
				ret[[i]] = structure(ret[[i]], class = "factor", levels = levels(attrs[[i]]))
		}
	}
	setNames(ret, names(attrs))
}

guess_raster = function(x, ...) {
	if (length(list(...))) # ... hints at other arguments meant at not guessing the raster
		return(NULL)
	if (all(st_dimension(x) == 0)) { # POINT
		cc = st_coordinates(x)
		ux = sort(unique(cc[,1]))
		uy = sort(unique(cc[,2]))
		dux = unique(diff(ux))
		if (length(dux) > 1) {
			if (var(dux)/mean(dux) < 1e-8)
				dux = mean(dux)
			else if (all(dux %% min(dux) == 0))
				dux = min(dux)
			else
				return(NULL)
		}
		duy = unique(diff(uy))
		if (length(duy) > 1) {
			if (var(duy)/mean(duy) < 1e-8)
				duy = mean(dux)
			else if (all(duy %% min(duy) == 0))
				duy = min(duy)
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
	if (is.character(dims))
		dims = match(dims, names(.x))
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
					create_dimension(values = suv, is_raster = i %in% xy)
			this_dim = this_dim + 1
		}
		names(dimensions) = names(.x)[dims]
	
		raster_xy = if (length(xy) == 2) names(.x)[xy] else c(NA_character_, NA_character_)
		d = create_dimensions(dimensions, raster = get_raster(dimensions = raster_xy))
		l = lapply(.x[-dims], function(x) {
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
