make_label = function(x, i = 1) {
	dims = dim(x)
	if ((length(dims) <= 2 || dims[3] == 1) && inherits(x[[i]], "units"))
		make_unit_label(paste0("'", names(x)[i], "'"), units(x[[i]]))
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
#' @param downsample logical or numeric; if \code{TRUE} will try to plot not many more pixels than actually are visible, if \code{FALSE}, no downsampling takes place, if numeric, the number of pixels/lines/bands etc that will be skipped; see Details.
#' @param nbreaks number of color breaks; should be one more than number of colors. If missing and \code{col} is specified, it is derived from that.
#' @param breaks actual color breaks, or a method name used for \link[classInt]{classIntervals}.
#' @param col colors to use for grid cells, or color palette function
#' @param ... further arguments: for \code{plot}, passed on to \code{image.stars}; for \code{image}, passed on to \code{image.default} or \code{rasterImage}.
#' @param key.pos integer; side to plot a color key: 1 bottom, 2 left, 3 top, 4 right; set to \code{NULL} to omit key. Ignored if multiple columns are plotted in a single function call. Default depends on plot size, map aspect, and, if set, parameter \code{asp}.
#' @param key.width amount of space reserved for width of the key (labels); relative or absolute (using lcm)
#' @param key.length amount of space reserved for length of the key (labels); relative or absolute (using lcm)
#' @param reset logical; if \code{FALSE}, keep the plot in a mode that allows adding further map elements; if \code{TRUE} restore original mode after plotting
#' @param box_col color for box around sub-plots; use \code{0} to suppress plotting of boxes around sub-plots.
#' @param center_time logical; if \code{TRUE}, sub-plot titles will show the center of time intervals, otherwise their start
#' @param hook NULL or function; hook function that will be called on every sub-plot; see examples.
#' @param mfrow length-2 integer vector with nrows, ncolumns of a composite plot, to override the default layout
#' @details
#' Downsampling: a value for \code{downsample} of 0: no downsampling, 1: after every dimension value (pixel/line/band), one value is skipped (half of the original resolution), 2: after every dimension value, 2 values are skipped (one third of the original resolution), etc.
#'
#' To remove unused classes in a categorical raster, use the \link[base]{droplevels} function.
#'
#' When bitmaps show visual artefacts (Moiré effects), make sure that device \link{png} is used rather than \code{ragg::agg_png} as the latter uses antialiasing for filled polygons which causes this; see also https://github.com/r-spatial/stars/issues/573 .
#' @export
#' @examples
#' st_bbox(L7_ETMs) |> st_as_sfc() |> st_centroid() |> st_coordinates() -> pt
#' hook1 = function() {
#'     text(pt[,"X"], pt[,"Y"], "foo", col = 'orange', cex = 2)
#' }
#' plot(L7_ETMs, hook = hook1)
#' x = st_set_dimensions(L7_ETMs, 3, paste0("B_", 1:6))
#' hook2 = function(..., row, col, nr, nrow, ncol, value, bbox) {
#'    str = paste0("row ", row, "/", nrow, ", col ", col, "/", ncol, "\nnr: ", nr, " value: ", value)
#'    bbox |> st_as_sfc() |> st_centroid() |> st_coordinates() -> pt
#'    text(pt[,"X"], pt[,"Y"], str, col = 'red', cex = 2)
#' }
#' plot(x, hook = hook2, col = grey(c(.2,.25,.3,.35)))
plot.stars = function(x, y, ..., join_zlim = TRUE, main = make_label(x, 1), axes = FALSE,
		downsample = TRUE, nbreaks = 11, breaks = "quantile", col = grey(1:(nbreaks-1)/nbreaks),
		key.pos = get_key_pos(x, ...), key.width = lcm(1.8), key.length = 0.618,
		reset = TRUE, box_col = grey(.8), center_time = FALSE, hook = NULL, mfrow = NULL) {

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
	x = st_normalize(x)
	if (is.character(x[[1]])) # rgb values
		key.pos = NULL
	if (!missing(col) && is.function(col))
		col = col(nbreaks - 1)
	if (is.factor(x[[1]])) {
		if (missing(col))
			col = attr(x[[1]], "colors") %||% sf.colors(length(levels(x[[1]])), categorical = TRUE)
		else
			attr(x[[1]], "colors") = col
	}
	
	key.pos.missing = missing(key.pos)
	breaks.missing = missing(breaks)
	if (missing(nbreaks) && !missing(col))
		nbreaks = length(col) + 1
	opar = par()
	dots = list(...)

	#if (any(dim(x) == 1))
	#	x = adrop(x)
	if (is.factor(x[[1]]) && any(is.na(levels(x[[1]]))))
		x = droplevels(x) # https://github.com/r-spatial/stars/issues/339

	if (join_zlim && !is.character(x[[1]]) && is.null(dots$rgb)) {
		breaks = if (is.factor(x[[1]]))
					seq(.5, length.out = length(levels(x[[1]])) + 1)
				else
					get_breaks(x, breaks, nbreaks, dots$logz)
		if (!inherits(breaks, c("POSIXt", "Date")))
			breaks = as.numeric(breaks)
		if (length(breaks) > 2)
			breaks = unique(breaks)
		nbreaks = length(breaks) # might be shorter than originally intended!
		if (breaks.missing && nbreaks <= 2) # unlucky default!
			warning('breaks="quantile" leads to a single class; maybe try breaks="equal" instead?')
	}

	if (isTRUE(dots$logz) && !((has_raster(x) && (is_curvilinear(x) || has_rotate_or_shear(x))) || has_sfc(x)))
		x = log10(x) # otherwise, defer log-transforming to sf::plot.sf

	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
		dxy = attr(st_dimensions(x), "raster")$dimensions
		loop = setdiff(names(dim(x)), dxy) # dimension (name) over which we loop, if any
		x = aperm(x, c(dxy, loop))
		zlim = if (join_zlim && is.null(dots$rgb))
				range(unclass(x[[1]]), na.rm = TRUE)
			else
				rep(NA_real_, 2)
		dims = dim(x)
		x = if (isTRUE(downsample)) {
				n = dims * 0 # keep names
				n[dxy] = get_downsample(dims, rgb = is.numeric(dots$rgb))
				st_downsample(x, n)
			} else if (is.numeric(downsample)) {
				st_downsample(x, downsample)
			} else
				x
		dims = dim(x) # may have changed by st_downsample

		if (length(dims) == 2 || dims[3] == 1 || (!is.null(dots$rgb) && is.numeric(dots$rgb))) { ## ONE IMAGE:
			# set up key region
			values = structure(x[[1]], dim = NULL) # array -> vector
			if (! isTRUE(dots$add) && ! is.null(key.pos) && !all(is.na(values)) && is.null(dots$rgb) &&
					(is.factor(values) || length(unique(na.omit(values))) > 1) &&
					length(col) > 1 && !is_curvilinear(x)) { # plot key?
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
			mar = c(axes * 2.1, axes * 2.1, 1 * !is.null(main), 0)
			if (!is.null(key.pos) && key.pos %in% 1:4)
				mar[key.pos] = mar[key.pos] + .5
			par(mar = mar)

			# plot the map:
			image(x, ..., axes = axes, breaks = breaks, col = col, key.pos = key.pos,
				key.width = key.width, key.length = key.length, main = NULL)
			if (!is.null(main))
				title(main)

		} else { ## MULTIPLE IMAGES -- now loop over dimensions 3:
			draw.key = !is.null(key.pos) && join_zlim
			if (! draw.key)
				key.pos = NULL
			lt = sf::.get_layout(st_bbox(x), dims[3], par("din"),
						if (join_zlim && key.pos.missing) -1 else key.pos, key.width, mfrow = mfrow)
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
				if (!is.null(hook) && is.function(hook)) {
					if (is.null(formals(hook)))
						hook()
					else {
						nc = lt$mfrow[[2]] # nr of columns in the plot
						hook(row = ((i - 1) %/% nc) + 1,
							 col = ((i - 1)  %% nc) + 1, 
							 nrow = lt$mfrow[[1]],
							 ncol = nc,
							 nr = i, value = labels[i],
							 bbox = st_bbox(x))
					}
				}
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
		if (! join_zlim)
			key.pos = NULL # omit key
		plot(st_as_sf(x[1]), ..., breaks = breaks, key.pos = key.pos, key.length = key.length,
			key.width = key.width, reset = reset, axes = axes, main = main)
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
#' @param text_color character; color for printed text values
#' @param interpolate logical; when using \link{rasterImage} (rgb), should pixels be interpolated?
#' @param as_points logical; for curvilinear or sheared grids: parameter passed on to \link{st_as_sf}, determining whether raster cells will be plotted as symbols (fast, approximate) or small polygons (slow, exact)
#' @param logz logical; if \code{TRUE}, use log10-scale for the attribute variable. In that case, \code{breaks} and \code{at} need to be given as log10-values; see examples.
#' @param add.geom object of class \code{sfc}, or list with arguments to \code{plot}, that will be added to an image or sub-image
#' @param border color used for cell borders (only in case \code{x} is a curvilinear or rotated/sheared grid)
#' @param useRaster logical; use the rasterImage capabilities of the graphics device?
#' @param extent object which has a \code{st_bbox} method; sets the plotting extent
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
		xlim = st_bbox(extent)$xlim, ylim = st_bbox(extent)$ylim, text_values = FALSE,
		text_color = 'black', axes = FALSE,
		interpolate = FALSE, as_points = FALSE, key.pos = NULL, logz = FALSE,
		key.width = lcm(1.8), key.length = 0.618, add.geom = NULL, border = NA,
		useRaster = isTRUE(dev.capabilities()$rasterImage == "yes"), extent = x) {

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

	# handle color table, if present:
	co = attr(ar, "colors")
	if (! is.null(co)) {
		if (is.null(rgb))
			rgb = TRUE
		interpretation = attr(co, "interpretation")
		if (!is.null(interpretation) && interpretation != 1)
			warning("color interpretation is not RGB, but rgb is used nevertheless: colors will probably be wrong")
	}

	# rearrange ar:
	others = setdiff(seq_along(dim(ar)), c(dimxn, dimyn))
	ar = aperm(ar, c(dimxn, dimyn, others))
	if (text_values)
		ar_text = ar # keep original order for cell text labels

	if (is.null(rgb) && is.character(ar))
		rgb = TRUE

	if (! is.null(rgb)) {
		if (is_curvilinear(x)) {
			x.sf = st_as_sf(x, as_points = as_points)
			plot_sf = function(x, col, ...) plot(x, ...) # absorb col
			if (!is.null(co)) # #456:
				plot_sf(x.sf, pal = co, border = FALSE, ...)
			else
				plot(x.sf, border = FALSE, ...)
			return()
		}
		xy = dim(ar)[1:2]
		if (! y_is_neg) { # need to flip y?
			ar = if (length(dim(ar)) == 3)
					ar[ , rev(seq_len(dim(ar)[2])), ]
				else
					ar[ , rev(seq_len(dim(ar)[2]))]
		}
		if (!useRaster)
			stop("rgb plotting not supported on this device")
		if (! isTRUE(dots$add)) {
			plot.new()
			plot.window(xlim = xlim, ylim = ylim, asp = asp)
		}
		if (is.numeric(rgb) && length(rgb) == 3) {
			ar = structure(ar[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
			#nas = apply(ar, 1, function(x) any(is.na(x))) #503
			nas = !complete.cases(ar)
			ar = grDevices::rgb(ar[!nas,], maxColorValue = maxColorValue)
			mat = rep(NA_character_, prod(xy))
			mat[!nas] = ar
			dim(mat) = xy
		} else {
			stopifnot(isTRUE(rgb) || inherits(rgb, "data.frame"))
			# rgb has col 1: index, col 2: label, col 3-5: R, G, B
			# ar = as.vector(ar[ , , 1]) # flattens x/y to 1-D index vector
			mat = if (isTRUE(rgb)) {
					if (!is.null(co))
						structure(co[as.vector(ar)], dim = dim(ar))
					else
						ar
				} else {
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
		text(do.call(expand.grid, dims[1:2]), labels = as.character(as.vector(ar_text)),
			col = text_color)
	}
	if (axes) { # FIXME: see sf::plot.sf for refinements to be ported here?
        if (isTRUE(st_is_longlat(x))) {
			if (isTRUE(all.equal(st_bbox(x), st_bbox(), tolerance = .1, 
								 check.attributes = FALSE))) {
				.degAxis(1, at = seq(-180, 180, 60))
				.degAxis(2, at = seq(-90, 90, 30))
			} else {
				.degAxis(1)
				.degAxis(2)
			}
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

# compute the degree of downsampling allowed to still have more than
# one cell per screen/device pixel:
get_downsample = function(dims, px = dev.size("px"), rgb = FALSE) {
	if (rgb)
		dims = dims[1:2]
	if (n <- max(0L, floor(sqrt(prod(dims) / prod(px))) - 1.0))
		message(paste0("downsample set to ", n))
	n
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
	if (inherits(x[[1]], "units"))
		x[[1]] = units::drop_units(x[[1]])
	x = st_upfront(adrop(x)) # drop singular dimensions, put x/y first
	dx = dim(x)
	if (length(dx) != 2)
		stop("contour only supported for 2-D arrays") # nocov
	e = expand_dimensions(x)
	contour(z = x[[1]][,rev(seq_len(dx[2]))], x = e[[1]], y = rev(e[[2]]), ...)
}

#' reduce dimension to rgb (alpha) hex values
#'
#' @export
#' @param x object of class \code{stars}
#' @param dimension dimension name or number to reduce
#' @param use_alpha logical; if TRUE, the fourth band will be used as alpha values
#' @param maxColorValue integer; maximum value for colors
#' @param stretch logical or character; if \code{TRUE} or \code{"percent"},
#' each band is stretched to 0 ... maxColorValue by "percent clip" method using
#' probs values. If \code{"histogram"}, a "histogram equalization" is performed
#' (\code{probs} values are ignored). If stretch is \code{NULL} or \code{FALSE}, no stretching
#' is performed. Other character values are interpreted as "percent" and a message
#' will be printed.
#' @param probs probability values for quantiles used for stretching by "percent".
#' @seealso \link{st_apply}, \link[grDevices]{rgb}
#' @details the dimension's bands are mapped to red, green, blue, alpha; if a different
#' ordering is wanted, use \link{[.stars} to reorder a dimension, see examples
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' st_rgb(x[,,,3:1])
#' r = st_rgb(x[,,,c(6,5,4,3)], 3, use_alpha=TRUE) # now R=6,G=5,B=4,alpha=3
#' if (require(ggplot2)) {
#'  ggplot() + geom_stars(data = r) + scale_fill_identity()
#' }
#' r = st_rgb(x[,,,3:1],
#' 		   probs = c(0.01, 0.99),
#' 		   stretch = "percent")
#' plot(r)
#' r = st_rgb(x[,,,3:1],
#' 		   probs = c(0.01, 0.99),
#' 		   stretch = "histogram")
#' plot(r)
st_rgb <- function (x,
					dimension = 3,
					use_alpha = dim(x)[dimension] == 4,
					maxColorValue = 255L,
					probs = c(0, 1),
					stretch = NULL) {

	if (inherits(x, "stars_proxy"))
		return(collect(x, match.call(), "st_rgb", 
					   c("x", "dimension", "use_alpha", "maxColorValue", "probs", "stretch"), env = environment())) # RETURNS!!

	# if not stars_proxy:
	if (is.character(dimension))
		dimension = match(dimension, names(dim(x)))
	stopifnot(is.numeric(dimension), length(dimension) == 1)
	if (!dim(x)[dimension] %in% c(3, 4))
		stop(paste("number of bands along dimension", dimension,
				   "should be 3 or 4"))
	dims = setdiff(seq_along(dim(x)), dimension)
	cutoff = function(x, probs, stretch.method = "percent") {
		if(stretch.method == "percent"){
			qs = if (all(probs == c(0, 1)))
				range(x)
			else quantile(x, probs, na.rm = TRUE)
			x = (x - qs[1])/(qs[2] - qs[1])
			x[x > 1] = 1
			x[x < 0] = 0
			x * maxColorValue
		} else if(stretch.method == "histogram"){
			x = stats::ecdf(x)(x)
			x * maxColorValue
		} else {
			qs = range(x)
			(x - qs[1])/(qs[2] - qs[1]) * maxColorValue
		}
	}

	if(is.null(stretch)) {
		stretch.method = "none"
		stretch = FALSE
	}

	if(is.logical(stretch)) {
		if(stretch){
			stretch.method = "percent"
		} else {
			maxColorValue = max(maxColorValue, max(x[[1]], na.rm = TRUE))
		}
	}

	if(is.character(stretch)) {
		if(!stretch %in% c("percent", "histogram")){
			stretch.method = "percent"
		} else {
			stretch.method = stretch
		}
		stretch = TRUE
	}

	if(stretch)
		x = st_apply(x, dimension, cutoff, probs = probs, stretch.method = stretch.method)

	if (anyNA(x[[1]])) {
		rgb4 = function(r, g, b, a) {
			r = cbind(as.vector(r), as.vector(g), as.vector(b),
					  as.vector(a))
			sel = !apply(r, 1, anyNA)
			ret = rep(NA_character_, nrow(r))
			ret[sel] = rgb(r[sel, 1:3], alpha = a[sel], maxColorValue = maxColorValue)
			structure(ret, dim = dim(g))
		}
		rgb3 = function(r, g, b) {
			r = cbind(as.vector(r), as.vector(g), as.vector(b))
			sel = !apply(r, 1, anyNA)
			ret = rep(NA_character_, nrow(r))
			ret[sel] = rgb(r[sel, 1:3], maxColorValue = maxColorValue)
			structure(ret, dim = dim(g))
		}
	} else {
		rgb4 = function(r, g, b, a)
			structure(rgb(r, g, b, a,maxColorValue = maxColorValue), dim = dim(r))
		rgb3 = function(r, g, b)
			structure(rgb(r, g, b, maxColorValue = maxColorValue), dim = dim(r))
	}
	st_apply(x, dims, if (use_alpha) rgb4 else rgb3)
}

#' @export
hist.stars = function(x, ..., main = names(x)[1]) {
	hist(x[[1]], ..., main = main)
}

#' @export
hist.stars_proxy = function(x, ..., downsample = 0) {
	x = st_as_stars(x, downsample = downsample)
	NextMethod()
}
