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
			ny = round(diff(bb[c("ymin", "ymax")])/dy),
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
	sf::gdal_rasterize(sf, template, st_geotransform(template), file, driver, options)
	ret = read_stars(file, driver = driver, proxy = proxy)
	if (!proxy) {
		if (length(dim(ret)) > 2)
			ret = split(ret)
		for (i in seq_along(ret)) {
			ret[[i]][is.nan(ret[[i]])] = NA_real_
			if (inherits(sf[[i]], "units"))
				units(ret[[i]]) = units(sf[[i]])
			if (is.factor(attrs[[i]]))
				ret[[i]] = structure(ret[[i]], class = class(attrs[[i]]), levels = levels(attrs[[i]]))
		}
	}
	setNames(ret, names(attrs))
}

guess_raster = function(x, ...) {
	if (length(list(...))) # ... hints at other arguments meant at not guessing the raster
		return(NULL)
	get_cell_size = function(x) {
		du = unique(diff(x))
		if (length(du) > 1) {
			if (var(du)/mean(du) < 1e-8) # some fuzz:
				du = mean(du)
			else if (all(du %% min(du) == 0)) # missing cols/rows:
				du = min(du)
			else
				numeric(0) # no regular step size
		} else
			du
	}
	if (all(st_dimension(x) == 0)) { # POINT
		cc = st_coordinates(x)
		ux = sort(unique(cc[,1]))
		if (length(dux <- get_cell_size(ux)) == 0)
			return(NULL);
		uy = sort(unique(cc[,2]))
		if (length(duy <- get_cell_size(uy)) == 0)
			return(NULL);
		bb = st_bbox(x)
		bb = st_bbox(setNames(c(bb["xmin"] - 0.5 * dux, 
			bb["ymin"] - 0.5 * duy, 
			bb["xmax"] + 0.5 * dux, 
			bb["ymax"] + 0.5 * duy), c("xmin", "ymin", "xmax", "ymax")), crs = st_crs(x))
		return(st_as_stars(bb, dx = dux, dy = duy))
	}
	NULL
}


#' @export
#' @param dims the column names or indices that form the cube dimensions
#' @param coords same as dims, for symmetry with \link[sf]{st_as_sf}
#' @param xy the x and y raster dimension names or indices; only takes effect after \code{dims} has been specified, see details
#' @param y_decreasing logical; if TRUE, (numeric) y values get a negative delta (decrease with increasing index)
#' @name st_as_stars
#' @details
#' If \code{xy} is not specified and the first two dimensions in \code{dims} are both numeric,
#' then it is set to these two dimensions.
#' @examples
#' if (require(plm, quietly = TRUE)) {
#'  data(Produc, package = "plm")
#'  st_as_stars(Produc)
#' }
#' if (require(dplyr, quietly = TRUE)) {
#'   # https://stackoverflow.com/questions/77368957/
#' spatial_dim <- st_sf(
#'   ID = 1:3,
#'   geometry = list(
#'     st_polygon(list(
#'       cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
#'     )),
#'     st_polygon(list(
#'       cbind(c(1, 2, 2, 1, 1), c(0, 0, 1, 1, 0))
#'     )),
#'     st_polygon(list(
#'       cbind(c(2, 3, 3, 2, 2), c(0, 0, 1, 1, 0))
#'     ))
#'   )
#' )
#' weekdays_dim <- data.frame(weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#' hours_dim <- data.frame(hours = c("8am", "11am", "4pm", "11pm"))
#' sf_dta <- spatial_dim |>
#'   cross_join(weekdays_dim)|>
#'   cross_join(hours_dim) |>
#'   mutate(population = rnorm(n(), mean = 1000, sd = 200)) |>
#'   select(everything(), geometry)
#' 
#' st_as_stars(sf_dta, dims = c("weekdays", "hours", "geometry"))
#' }
#' demo(nc, echo=FALSE,ask=FALSE)
#' st_as_stars(nc)
#' st_as_stars(st_drop_geometry(nc), dims = "NAME")
#' data.frame(expand.grid(x=1:5, y = 1:5), z = rnorm(25)) |> st_as_stars()
st_as_stars.data.frame = function(.x, ..., dims = coords, xy, y_decreasing = TRUE, coords = 1:2) {

	if (missing(dims) && !missing(xy))
		stop("parameter xy only takes effect when the cube dimensions are set with dims")
	if (is.character(dims))
		dims = match(dims, names(.x))
	if (missing(xy)) {
		if (length(dims) > 1 && all(sapply(.x[dims[1:2]], is.numeric)))
			xy = dims[1:2]
		else 
			xy = c(-1, -1) # cancels y_decreasing
	}
	if (is.character(xy)) {
		xy = match(xy, names(.x))
		if (any(is.na(xy)))
			stop("xy coordinates not found in data")
	}
	stopifnot(length(dims) >= 1, all(dims >= 1), !any(is.na(dims)))

	index = NULL
	dimensions = list()
	this_dim = 1
	for (i in dims) {
		v = .x[[i]]
		if (inherits(v, "sfc")) {
    		if (!requireNamespace("digest", quietly = TRUE))
       			stop("package digest required, please install it first") # nocov
			dig = sapply(st_equals(v), digest::digest)
			uv = unique(dig) # no need to sort
			ix = match(dig, uv) # but look up "hash collision"
		} else {
			suv = if (is.factor(v))
					levels(v)
				else if (is.character(v))
					unique(v)
				else # numeric:
					sort(unique(v), decreasing = y_decreasing && i == xy[2])
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
	
	raster_xy = if (length(xy) == 2 && all(xy > 0)) 
			names(.x)[xy] 
		else 
			c(NA_character_, NA_character_)
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
