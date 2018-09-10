#' plot stars object, with subplots for each level of first non-spatial dimension
#'
#' @name plot.stars
#' @param x object of class \code{stars}
#' @param y ignored
#' @param join_zlim logical; if \code{TRUE}, compute a single, joint zlim (color scale) for all subplots from \code{x}
#' @param main character; subplot title prefix; use \code{""} to get only time, use \code{NULL} to suppress subplot titles
#' @param axes logical; should axes and box be added to the plot?
#' @param downsample logical; if \code{TRUE} will try to plot not many more pixels than actually are visibule.
#' @param nbreaks number of color breaks; should be one more than number of colors. If missing and \code{col} is specified, it is derived from that.
#' @param breaks actual color breaks, or a method name used for \link[classInt]{classIntervals}.
#' @param col colors to use for grid cells
#' @param ... further arguments: for \code{plot}, passed on to \code{image.stars}; for \code{image}, passed on to \code{image.default} or \code{rasterImage}.
#' @param key.pos integer; side to plot a color key: 1 bottom, 2 left, 3 top, 4 right; set to \code{NULL} to omit key. Ignored if multiple columns are plotted in a single function call. Default depends on plot size, map aspect, and, if set, parameter \code{asp}.
#' @param key.width amount of space reserved for width of the key (labels); relative or absolute (using lcm)
#' @param key.length amount of space reserved for length of the key (labels); relative or absolute (using lcm)
#' @param reset logical; if \code{FALSE}, keep the plot in a mode that allows adding further map elements; if \code{TRUE} restore original mode after plotting; see details.
#' @param box_col color for box around sub-plots; use \code{0} to suppress plotting of boxes around sub-plots.
#' @export
plot.stars = function(x, y, ..., join_zlim = TRUE, main = names(x)[1], axes = FALSE, 
		downsample = TRUE, nbreaks = 11, breaks = "quantile", col = grey(1:(nbreaks-1)/nbreaks),
		key.pos = get_key_pos(x, ...), key.width = lcm(1.8), key.length = 0.618, 
		reset = TRUE, box_col = grey(.8)) {

	flatten = function(x, i) { # collapse all non-x/y dims into one, and select "layer" i
		d = st_dimensions(x)
		dims = dim(x)
		x = x[[1]]
		aux = setdiff(names(dims), c("x", "y"))
		newdims = c(dims[c("x", "y")], prod(dims[aux]))
		dim(x) = newdims
		st_as_stars(list(x[,,i]), dimensions = d[c("x", "y")])
	}
	key.pos.missing = missing(key.pos)
	if (missing(nbreaks) && !missing(col))
		nbreaks = length(col) + 1
	opar = par()
	dots = list(...)

	#if (any(dim(x) == 1))
	#	x = adrop(x)

	if (join_zlim) {
		breaks = get_breaks(x, breaks, nbreaks)
		nbreaks = length(breaks) # might be shorter than originally intended!
	}

	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
		xy = attr(st_dimensions(x), "raster")$dimensions
		loop = setdiff(names(dim(x)), xy) # dimension (name) over which we loop, if any
		x = aperm(x, c(xy, loop))
		zlim = if (join_zlim)
				range(unclass(x[[1]]), na.rm = TRUE)
			else
				rep(NA_real_, 2)
		dims = dim(x)
		if (downsample) {
			n = dims * 0 + 1 # keep names
			n[c("x", "y")] = get_downsample(dims)
			x = st_downsample(x, n)
		}
		if (length(dims) == 2 || dims[3] == 1 || !is.null(dots$rgb)) { ## ONE IMAGE:
			# set up key region
			values = as.vector(x[[1]])
			if (! isTRUE(dots$add) && ! is.null(key.pos) && !all(is.na(values)) &&
					(is.factor(values) || length(unique(na.omit(values))) > 1) &&
					length(col) > 1 && is.null(dots$rgb)) { # plot key?
				switch(key.pos,
					layout(matrix(c(2,1), nrow = 2, ncol = 1), widths = 1, heights = c(1, key.width)),  # 1 bottom
					layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(key.width, 1), heights = 1),  # 2 left
					layout(matrix(c(1,2), nrow = 2, ncol = 1), widths = 1, heights = c(key.width, 1)),  # 3 top
					layout(matrix(c(2,1), nrow = 1, ncol = 2), widths = c(1, key.width), heights = 1)   # 4 right
				)
				if (is.factor(values)) {
					.image_scale_factor(levels(values), col, key.pos = key.pos,
						key.width = key.width, key.length = key.length, axes = axes, ...)
				} else
					.image_scale(values, col, breaks = breaks, key.pos = key.pos, 
						key.width = key.width, key.length = key.length, axes = axes, ...) 
			}

			# map panel:
			par(mar = c(axes * 2.1, axes * 2.1, 1 * !is.null(main), 0))

			# plot the map:
			image(x, ..., axes = axes, breaks = breaks, col = col)
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
			labels = expand_dimensions(st_dimensions(x))[[3]]
			for (i in seq_len(dims[3])) {
				im = flatten(x, i)
				if (! join_zlim) {
					zlim = range(im[[1]], na.rm = TRUE)
					br = get_breaks(im, breaks, nbreaks)
				} else
					br = breaks
				image(im, xlab = "", ylab = "", axes = axes, zlim = zlim, breaks = br, col = col, ...)
				if (!is.null(main)) {
					if (length(main) == dims[3])
						title(main[i])
					else
						title(paste(main, format(labels[i])))
				}
				box(col = box_col)
			}
			for (i in seq_len(prod(lt$mfrow) - dims[3])) # empty panels:
				plot.new()
			if (draw.key) {
				values = as.vector(x[[1]])
				if (is.factor(values))
					.image_scale_factor(levels(values), col, key.pos = lt$key.pos,
						key.width = key.width, key.length = key.length, axes = axes,...)
				else
					.image_scale(values, col, breaks = breaks, key.pos = lt$key.pos, 
						key.width = key.width, key.length = key.length, axes = axes,...)
			}
		}
	} else if (has_sfc(x)) {
		plot(st_as_sf(x), ..., axes = axes)
	} else
		stop("no raster, no features geometries: no default plot method set up yet!")
	if (reset) {
		layout(matrix(1)) # reset
		desel = which(names(opar) %in% c("cin", "cra", "csi", "cxy", "din", "page", "pin"))
		par(opar[-desel])
	}
}

get_breaks = function(x, breaks, nbreaks) {
	if (is.character(breaks)) { # compute breaks from values in x:
		pdx = prod(dim(x[[1]]))
		# take a regular sample from x[[1]]:
		values = as.numeric(as.vector(x[[1]])[seq(1, pdx, length.out = min(pdx, 10000))])
		n.unq = length(unique(na.omit(values)))
		if (! all(is.na(values)) && n.unq > 1)
			classInt::classIntervals(na.omit(values), min(nbreaks-1, n.unq), breaks, warnSmallN = FALSE)$brks
		else
			range(values, na.rm = TRUE) # lowest and highest!
	} else # breaks was given:
		breaks
}

#' @name plot.stars
#' @param band integer; which band (dimension) to plot
#' @param attr integer; which attribute to plot
#' @param asp numeric; aspect ratio of image
#' @param rgb integer; specify three bands to form an rgb composite
#' @param maxColorValue numeric; passed on to \link{rgb}
#' @param xlab character; x axis label
#' @param ylab character; y axis label
#' @param xlim x axis limits
#' @param ylim y axis limits
#' @param text_values logical; print values as text on image?
#' @param interpolate logical; when using \link{rasterImage} (rgb), should pixels be interpolated?
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' image(x, col = grey((3:9)/10))
#' image(x, rgb = c(1,3,5)) # rgb composite
image.stars = function(x, ..., band = 1, attr = 1, asp = NULL, rgb = NULL, maxColorValue = max(x[[attr]]),
		xlab = if (!axes) "" else names(dims)[1], ylab = if (!axes) "" else names(dims)[2],
		xlim = st_bbox(x)$xlim, ylim = st_bbox(x)$ylim, text_values = FALSE, axes = FALSE,
		interpolate = FALSE) {

	dots = list(...)
	stopifnot(!has_rotate_or_shear(x)) # FIXME: use rasterImage() with rotate, if only rotate & no shear

	if (any(dim(x) == 1))
		x = adrop(x)

	force(xlim)
	force(ylim)
	dims = expand_dimensions(x)

	dimxy = attr(st_dimensions(x), "raster")$dimensions
	dimx =  dimxy[1]
	dimy =  dimxy[2]
	dimxn = which(dimx == names(dims))
	dimyn = which(dimy == names(dims))

	y_is_neg = all(diff(dims[[ dimy ]]) < 0)
	if (y_is_neg)
		dims[[ dimy ]] = rev(dims[[ dimy ]])

	if (is.null(asp)) {
		bb = st_bbox(x)
		asp <- ifelse(isTRUE(st_is_longlat(x)), 1/cos((mean(bb[c(2,4)]) * pi)/180), 1.0)
	}

	ar = unclass(x[[ attr ]]) # raw data matrix/array

	# rearrange ar:
	third = setdiff(1:3, c(dimxn, dimyn))[1]
	ar = if (length(dim(x)) == 3) # FIXME: deal with more than 3 dims here?
			aperm(ar, c(dimxn, dimyn, third))
		else
			aperm(ar, c(dimxn, dimyn))

	if (! is.null(rgb)) {
		if (is_rectilinear(x))
			warning("rectilinear rgb grid is plotted as regular grid")
		xy = dim(ar)[1:2]
		ar = structure(ar[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
		nas = apply(ar, 1, function(x) any(is.na(x)))
		ar = rgb(ar[!nas,], maxColorValue = maxColorValue)
		mat = rep(NA_character_, prod(xy))
		mat[!nas] = ar
		dim(mat) = xy
		if (dev.capabilities("rasterImage")$rasterImage != "yes")
			stop("rgb plotting not supported on this device")
		if (! isTRUE(dots$add)) {
			plot.new()
			plot.window(xlim = xlim, ylim = ylim, asp = asp)
		}
		myRasterImage = function(x, xmin, ymin, xmax, ymax, interpolate, ..., breaks, add) # absorbs breaks & add
			rasterImage(x, xmin, ymin, xmax, ymax, interpolate = interpolate, ...)
		myRasterImage(t(mat), xlim[1], ylim[1], xlim[2], ylim[2], interpolate = interpolate, ...)
	} else { 
		if (y_is_neg) { # flip y?
			ar = if (length(dim(x)) == 3) # FIXME: deal with more than 3 dims here?
				ar[ , rev(seq_len(dim(ar)[2])), band]
			else
				ar[ , rev(seq_len(dim(ar)[2]))]
		}
		image.default(dims[[ dimx ]], dims[[ dimy ]], ar, asp = asp, xlab = xlab, ylab = ylab, 
			xlim = xlim, ylim = ylim, axes = FALSE, ...)
	}
	if (text_values)
		text(do.call(expand.grid, dims[1:2]), labels = as.character(as.vector(ar))) # xxx
	if (axes) {
        if (isTRUE(st_is_longlat(x))) {
            .degAxis(1)
            .degAxis(2)
        } else {
            axis(1)
            axis(2)
		}
	}
}

# reduce resolution of x, keeping (most of) extent
st_downsample = function(x, n) {
	stopifnot(all(n >= 0))
	if (! all(n == 0)) {
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
