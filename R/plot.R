make_label = function(x, i = 1) {
	dims = dim(x)
	if ((length(dims) <= 2 || dims[3] == 1) && inherits(x[[i]], "units"))
		make_unit_label(names(x)[i], units(x[[i]]))
	else
		names(x)[i]
}
	


#' plot stars object, with subplots for each level of first non-spatial dimension
#' 
#' plot stars object, with subplots for each level of first non-spatial dimension, and customization of legend key
#'
#' @name plot
#' @param x object of class \code{stars}
#' @param y ignored
#' @param join_zlim logical; if \code{TRUE}, compute a single, joint zlim (color scale) for all subplots from \code{x}
#' @param main character; subplot title prefix; use \code{""} to get only time, use \code{NULL} to suppress subplot titles
#' @param axes logical; should axes and box be added to the plot?
#' @param downsample logical or numeric; if \code{TRUE} will try to plot not many more pixels than actually are visible, if \code{FALSE}, no downsampling takes place, if numeric, the downsampling rate; see Details.
#' @param nbreaks number of color breaks; should be one more than number of colors. If missing and \code{col} is specified, it is derived from that.
#' @param breaks actual color breaks, or a method name used for \link[classInt]{classIntervals}.
#' @param col colors to use for grid cells
#' @param ... further arguments: for \code{plot}, passed on to \code{image.stars}; for \code{image}, passed on to \code{image.default} or \code{rasterImage}.
#' @param key.pos integer; side to plot a color key: 1 bottom, 2 left, 3 top, 4 right; set to \code{NULL} to omit key. Ignored if multiple columns are plotted in a single function call. Default depends on plot size, map aspect, and, if set, parameter \code{asp}.
#' @param key.width amount of space reserved for width of the key (labels); relative or absolute (using lcm)
#' @param key.length amount of space reserved for length of the key (labels); relative or absolute (using lcm)
#' @param reset logical; if \code{FALSE}, keep the plot in a mode that allows adding further map elements; if \code{TRUE} restore original mode after plotting; see details.
#' @param box_col color for box around sub-plots; use \code{0} to suppress plotting of boxes around sub-plots.
#' @param center_time logical; if \code{TRUE}, sub-plot titles will show the center of time intervals, otherwise their start
#' @param hook NULL or function; hook function that will be called on every sub-plot.
#' @details 
#' Downsampling: a value for \code{downsample} of 0 or 1 causes no downsampling, 2 that every second dimension value is sampled, 3 that every third dimension value is sampled, and so on. 
#' @export
plot.stars = function(x, y, ..., join_zlim = TRUE, main = make_label(x, 1), axes = FALSE, 
		downsample = TRUE, nbreaks = 11, breaks = "quantile", col = grey(1:(nbreaks-1)/nbreaks),
		key.pos = get_key_pos(x, ...), key.width = lcm(1.8), key.length = 0.618, 
		reset = TRUE, box_col = grey(.8), center_time = FALSE, hook = NULL) {

	flatten = function(x, i) { # collapse all non-x/y dims into one, and select "layer" i
		d = st_dimensions(x)
		dxy = attr(d, "raster")$dimensions
		dims = dim(x)
		nms = names(x)
		x = x[[1]]
		aux = setdiff(names(dims), dxy)
		newdims = c(dims[dxy], prod(dims[aux]))
		dim(x) = newdims
		st_as_stars(setNames(list(x[,,i]), nms[1]), dimensions = d[dxy])
	}
	key.pos.missing = missing(key.pos)
	if (missing(nbreaks) && !missing(col))
		nbreaks = length(col) + 1
	opar = par()
	dots = list(...)

	#if (any(dim(x) == 1))
	#	x = adrop(x)

	if (join_zlim && !is.character(x[[1]])) {
		breaks = get_breaks(x, breaks, nbreaks, dots$logz)
		if (length(breaks) > 2)
			breaks = unique(breaks)
		nbreaks = length(breaks) # might be shorter than originally intended!
	}

	if (isTRUE(dots$logz) && !((has_raster(x) && (is_curvilinear(x) || has_rotate_or_shear(x))) || has_sfc(x)))
		x = log10(x) # otherwise, defer log-transforming to sf::plot.sf

	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
		dxy = attr(st_dimensions(x), "raster")$dimensions
		loop = setdiff(names(dim(x)), dxy) # dimension (name) over which we loop, if any
		x = aperm(x, c(dxy, loop))
		zlim = if (join_zlim)
				range(unclass(x[[1]]), na.rm = TRUE)
			else
				rep(NA_real_, 2)
		dims = dim(x)
		x = if (isTRUE(downsample)) {
				n = dims * 0 + 1 # keep names
				n[dxy] = get_downsample(dims)
				st_downsample(x, n)
			} else if (is.numeric(downsample))
				st_downsample(x, downsample)
		dims = dim(x) # may have changed by st_downsample

		if (length(dims) == 2 || dims[3] == 1 || (!is.null(dots$rgb) && is.numeric(dots$rgb))) { ## ONE IMAGE:
			# set up key region
			values = structure(x[[1]], dim = NULL) # array -> vector
			if (! isTRUE(dots$add) && ! is.null(key.pos) && !all(is.na(values)) &&
					(is.factor(values) || length(unique(na.omit(values))) > 1) &&
					length(col) > 1 && is.null(dots$rgb) && !is_curvilinear(x)) { # plot key?
				switch(key.pos,
					layout(matrix(c(2,1), nrow = 2, ncol = 1), widths = 1, heights = c(1, key.width)),  # 1 bottom
					layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(key.width, 1), heights = 1),  # 2 left
					layout(matrix(c(1,2), nrow = 2, ncol = 1), widths = 1, heights = c(key.width, 1)),  # 3 top
					layout(matrix(c(2,1), nrow = 1, ncol = 2), widths = c(1, key.width), heights = 1)   # 4 right
				)
				if (is.factor(values))
					.image_scale_factor(levels(values), col, key.pos = key.pos,
						key.width = key.width, key.length = key.length, axes = axes, ...)
				else
					.image_scale(values, col, breaks = breaks, key.pos = key.pos, 
						key.width = key.width, key.length = key.length, axes = axes, ...) 
			}

			# map panel:
			par(mar = c(axes * 2.1, axes * 2.1, 1 * !is.null(main), 0))

			# plot the map:
			image(x, ..., axes = axes, breaks = breaks, col = col, key.pos = key.pos, 
				key.width = key.width, key.length = key.length, main = NULL)
			if (!is.null(main))
				title(main)

		} else { ## MULTIPLE IMAGES -- now loop over dimensions 3:
			draw.key = !is.null(key.pos) && join_zlim
			if (! draw.key)
				key.pos = NULL
			lt = .get_layout(st_bbox(x), dims[3], par("din"),
				if (join_zlim && key.pos.missing) -1 else key.pos, key.width)
			title_size = if (is.null(main)) 
					0
				else
					1.2
			layout(lt$m, widths = lt$widths, heights = lt$heights, respect = FALSE)
			par(mar = c(axes * 2.1, axes * 2.1, title_size, 0))
			labels = st_get_dimension_values(x, 3, center = center_time)
			for (i in seq_len(dims[3])) {
				im = flatten(x, i)
				if (! join_zlim) {
					zlim = range(im[[1]], na.rm = TRUE)
					br = get_breaks(im, breaks, nbreaks, dots$logz)
				} else
					br = breaks
				image(im, xlab = "", ylab = "", axes = axes, zlim = zlim, breaks = br, col = col, key.pos = NULL, main = NULL, ...)
				if (!is.null(main)) {
					if (length(main) == dims[3])
						title(main[i])
					else if (identical(main, names(x)[1])) # default value: omit on multi
						title(paste(format(labels[i])))
					else # user-defined
						title(paste(main, format(labels[i])))
				}
				if (!is.null(hook) && is.function(hook))
					hook()
				box(col = box_col)
			}
			for (i in seq_len(prod(lt$mfrow) - dims[3])) # empty panels:
				plot.new()
			if (draw.key) {
				values = structure(x[[1]], dim = NULL)
				if (is.factor(values))
					.image_scale_factor(levels(values), col, key.pos = lt$key.pos,
						key.width = key.width, key.length = key.length, axes = axes,...)
				else
					.image_scale(values, col, breaks = breaks, key.pos = lt$key.pos, 
						key.width = key.width, key.length = key.length, axes = axes,...)
			}
		}
	} else if (has_sfc(x)) {
#		if (key.pos.missing)
#			key.pos = -1
		plot(st_as_sf(x), ..., key.pos = key.pos, key.length = key.length, key.width = key.width, 
			reset = reset, axes = axes, main = main)
	} else
		stop("no raster, no features geometries: no default plot method set up yet!")
	if (reset) {
		layout(matrix(1)) # reset
		desel = which(names(opar) %in% c("cin", "cra", "csi", "cxy", "din", "page", "pin"))
		par(opar[-desel])
	}
}

get_breaks = function(x, breaks, nbreaks, logz = NULL) {
	if (is.character(breaks)) { # compute breaks from values in x:
		pdx = prod(dim(x[[1]]))
		## take a regular sample from x[[1]]:
		# values = structure(x[[1]], dim = NULL)[seq(1, pdx, length.out = min(pdx, 10000))]
		values = structure(x[[1]], dim = NULL)
		if (isTRUE(logz))
			values = log10(values)
		if (is.factor(values) || is.logical(values))
			values = as.numeric(values)
		n.unq = length(unique(na.omit(values)))
		if (! all(is.na(values)) && n.unq > 1)
			classInt::classIntervals(na.omit(values), min(nbreaks-1, n.unq), breaks, 
				warnSmallN = FALSE)$brks
		else
			range(values, na.rm = TRUE) # lowest and highest!
	} else # breaks was given:
		breaks
}

#' @name plot
#' @param band integer; which band (dimension) to plot
#' @param attr integer; which attribute to plot
#' @param asp numeric; aspect ratio of image
#' @param rgb integer; specify three bands to form an rgb composite. Experimental: rgb color table; see Details.
#' @param maxColorValue numeric; passed on to \link{rgb}
#' @param xlab character; x axis label
#' @param ylab character; y axis label
#' @param xlim x axis limits
#' @param ylim y axis limits
#' @param text_values logical; print values as text on image?
#' @param interpolate logical; when using \link{rasterImage} (rgb), should pixels be interpolated?
#' @param as_points logical; for curvilinear or sheared grids: parameter passed on to \link{st_as_sf}, determining whether raster cells will be plotted as symbols (fast, approximate) or small polygons (slow, exact)
#' @param logz logical; if \code{TRUE}, use log10-scale for the attribute variable. In that case, \code{breaks} and \code{at} need to be given as log10-values; see examples.
#' @param add.geom object of class \code{sfc}, or list with arguments to \code{plot}, that will be added to an image or sub-image
#' @param border color used for cell borders (only in case \code{x} is a curvilinear or rotated/sheared grid)
#' @param useRaster logical; use the rasterImage capabilities of the graphics device?
#' @details use of an rgb color table is experimental; see https://github.com/r-spatial/mapview/issues/208
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' image(x, col = grey((3:9)/10))
#' image(x, rgb = c(1,3,5)) # rgb composite
image.stars = function(x, ..., band = 1, attr = 1, asp = NULL, rgb = NULL, 
		maxColorValue = ifelse(inherits(rgb, "data.frame"), 255, max(x[[attr]], na.rm = TRUE)),
		xlab = if (!axes) "" else names(d)[1], ylab = if (!axes) "" else names(d)[2],
		xlim = st_bbox(x)$xlim, ylim = st_bbox(x)$ylim, text_values = FALSE, axes = FALSE,
		interpolate = FALSE, as_points = FALSE, key.pos = NULL, logz = FALSE,
		key.width = lcm(1.8), key.length = 0.618, add.geom = NULL, border = NA,
		useRaster = dev.capabilities("rasterImage")$rasterImage == "yes") {

	dots = list(...)

	#stopifnot(!has_rotate_or_shear(x)) # FIXME: use rasterImage() with rotate, if only rotate & no shear

	if (any(dim(x) == 1))
		x = adrop(x)

	force(xlim)
	force(ylim)

	d = st_dimensions(x)

	dimxy = attr(d, "raster")$dimensions
	dimx =  dimxy[1]
	dimy =  dimxy[2]
	dimxn = match(dimx, names(d))
	dimyn = match(dimy, names(d))

	if (has_sfc(x) && length(dim(x)) == 2) { # time series of features:
		ed = lapply(expand_dimensions(x), function(x) if (inherits(x, "sfc")) seq_along(x) else x)
		names(ed) = c("x", "y")
		ed$z = x[[attr]]
		axes = TRUE
		return(image(ed, ..., xlab = xlab, ylab = ylab))
	}

	if (! is_curvilinear(x)) {
		dims = expand_dimensions.stars(x, center = FALSE, max = FALSE)
		d_max = expand_dimensions.stars(x, center = FALSE, max = TRUE)
		if (tail(dims[[dimx]], 1) != tail(d_max[[dimx]], 1))
			dims[[ dimx ]] = c(dims[[dimx]], tail(d_max[[dimx]], 1))
		if (tail(dims[[dimy]], 1) != tail(d_max[[dimy]], 1))
			dims[[ dimy ]] = c(dims[[dimy]], tail(d_max[[dimy]], 1))

		y_is_neg = all(diff(dims[[ dimy ]]) < 0)
		if (y_is_neg)
			dims[[ dimy ]] = rev(dims[[ dimy ]])
	}

	if (is.null(asp))
		asp = if (isTRUE(st_is_longlat(x))) {
				bb = st_bbox(x)
				1 / cos((mean(bb[c(2,4)]) * pi)/180)
			} else
				1.0

	ar = unclass(x[[ attr ]]) # raw data matrix/array

	# rearrange ar:
	others = setdiff(seq_along(dim(ar)), c(dimxn, dimyn))
	ar = aperm(ar, c(dimxn, dimyn, others))
	if (text_values)
		ar_text = ar # keep original order for cell text labels
	
	if (is.null(rgb) && is.character(ar))
		rgb = TRUE

	if (! is.null(rgb)) {
		if (is_curvilinear(x))
			warning("when using rgb, curvilinear grid is plotted as regular grid")
		xy = dim(ar)[1:2]
		if (! y_is_neg) # need to flip y?
			ar = ar[ , rev(seq_len(dim(ar)[2])), ]
		if (!useRaster)
			stop("rgb plotting not supported on this device")
		if (! isTRUE(dots$add)) {
			plot.new()
			plot.window(xlim = xlim, ylim = ylim, asp = asp)
		}
		if (is.numeric(rgb) && length(rgb) == 3) {
			ar = structure(ar[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
			nas = apply(ar, 1, function(x) any(is.na(x)))
			ar = grDevices::rgb(ar[!nas,], maxColorValue = maxColorValue)
			mat = rep(NA_character_, prod(xy))
			mat[!nas] = ar
			dim(mat) = xy
		} else {
			stopifnot(isTRUE(rgb) || inherits(rgb, "data.frame"))
			# rgb has col 1: index, col 2: label, col 3-5: R, G, B
			# ar = as.vector(ar[ , , 1]) # flattens x/y to 1-D index vector
			mat = if (isTRUE(rgb))
					ar
				else {
					ar = as.vector(ar) # flattens x/y to 1-D index vector
					rgb = grDevices::rgb(rgb[match(ar, rgb[[1]]), 3:5], maxColorValue = maxColorValue)
					structure(rgb, dim = xy)
				}
		}
		myRasterImage = function(x, xmin, ymin, xmax, ymax, interpolate, ..., breaks, add, zlim) # absorbs breaks, add & zlim
			rasterImage(x, xmin, ymin, xmax, ymax, interpolate = interpolate, ...)
		myRasterImage(t(mat), xlim[1], ylim[1], xlim[2], ylim[2], interpolate = interpolate, ...)
	} else if (is_curvilinear(x) || has_rotate_or_shear(x)) {
		x = st_as_sf(x[1], as_points = as_points)
		mplot = function(x, col, ..., zlim) {
			if (missing(col))
				plot(x, ...)
			else
				plot(x, pal = col, ...) # need to swap arg names: FIXME:?
		}
		mplot(x, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, axes = axes, reset = FALSE, 
			key.pos = key.pos, key.width = key.width, key.length = key.length, logz = logz, 
			border = border, ...)
	} else { # regular & rectilinear grid, no RGB:
		if (y_is_neg) { # need to flip y?
			ar = if (length(dim(ar)) == 2)
					ar[ , rev(seq_len(dim(ar)[2])), drop = FALSE]
				else
					ar[ , rev(seq_len(dim(ar)[2])), band] # FIXME: breaks if more than 3?
		}
		image.default(dims[[ dimx ]], dims[[ dimy ]], ar, asp = asp, xlab = xlab, ylab = ylab, 
			xlim = xlim, ylim = ylim, axes = FALSE, useRaster = useRaster && !is_rectilinear(x), ...)
	}
	if (text_values) {
		dims = expand_dimensions.stars(x, center = TRUE)
		text(do.call(expand.grid, dims[1:2]), labels = as.character(as.vector(ar_text))) # xxx
	}
	if (axes) { # FIXME: see sf::plot.sf for refinements to be ported here?
        if (isTRUE(st_is_longlat(x))) {
            .degAxis(1)
            .degAxis(2)
        } else {
            axis(1)
            axis(2)
		}
	}
	if (!is.null(add.geom)) {
		if (inherits(add.geom, c("sf", "sfc")))
			add.geom = list(add.geom)
		do.call(plot, c(add.geom, add = TRUE))
	}
}

# reduce resolution of x, keeping (most of) extent
st_downsample = function(x, n) {
	stopifnot(all(n >= 0))
	if (! all(n <= 1)) {
		d = dim(x)
		n = rep(n, length.out = length(d))
		args = rep(list(rlang::missing_arg()), length(d)+1)
		for (i in seq_along(d))
			if (n[i] > 1)
				args[[i+1]] = seq(1, d[i], n[i])
		eval(rlang::expr(x[!!!args]))
	} else
		x
}

# compute the degree of downsampling allowed to still have more than 
# one cell per screen/device pixel:
get_downsample = function(dims, px = dev.size("px")) { 
	floor(sqrt(prod(dims) / prod(px)))
}


#' plot contours of a stars object
#'
#' plot contours of a stars object
#' @param x object of class \code{stars}
#' @param ... other parameters passed on to \link[graphics]{contour}
#' @details this uses the R internal contour algorithm, which (by default) plots contours; \link{st_contour} uses the GDAL contour algorithm that returns contours as simple features.
#' @export
#' @examples
#' d = st_dimensions(x = 1:ncol(volcano), y = 1:nrow(volcano))
#' r = st_as_stars(t(volcano))
#' r = st_set_dimensions(r, 1, offset = 0, delta = 1)
#' r = st_set_dimensions(r, 2, offset = 0, delta = -1)
#' plot(r, reset = FALSE)
#' contour(r, add = TRUE)
contour.stars = function(x, ...) {
	if (!(is_regular_grid(x) || is_rectilinear(x)))
		stop("contour only works for regular or rectilinear grids")
	x = st_upfront(adrop(x)) # drop singular dimensions, put x/y first
	dx = dim(x)
	if (length(dx) != 2)
		stop("contour only supported for 2-D arrays") # nocov
	e = expand_dimensions(x)
	contour(z = x[[1]][,rev(seq_len(dx[2]))], x = e[[1]], y = rev(e[[2]]), ...)
}
