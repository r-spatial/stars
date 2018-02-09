#' plot stars object, with subplots for each level of first non-spatial dimension
#'
#' @name plot.stars
#' @param x object of class \code{stars}
#' @param y ignored
#' @param join_zlim logical; if \code{TRUE}, compute a single zlim for all subplots from array range
#' @param main character; subplot title prefix; use \code{""} to get only time, use \code{NULL} to suppress subplot titles
#' @param axes logical; should axes be added to the plot?
#' @param downsample logical; if \code{TRUE} will try to plot not many more pixels than actually are visibule.
#' @param nbreaks number of color breaks; should be one more than number of colors. If missing and \code{col} is specified, it is derived from that.
#' @param breaks actual color breaks, or a method name used for \link[classInt]{classIntervals}.
#' @param col colors to use for grid cells
#' @export
plot.stars = function(x, y, ..., join_zlim = TRUE, main = names(x)[1], axes = FALSE, 
		downsample = TRUE, nbreaks = 11, breaks = "quantile", col = grey(1:(nbreaks-1)/nbreaks)) {
	flatten = function(x, i) { # collapse all non-x/y dims into one
		d = st_dimensions(x)
		dims = dim(x)
		x = x[[1]]
		aux = setdiff(names(dims), c("x", "y"))
		newdims = c(dims[c("x", "y")], prod(dims[aux]))
		dim(x) = newdims
		st_stars(list(x[,,i]), dimensions = d[c("x", "y")])
	}
	if (missing(nbreaks) && !missing(col))
		nbreaks = length(col) + 1

	dots = list(...)

	if (is.character(breaks)) { # compute breaks from values:
		pdx = prod(dim(x[[1]]))
		# take a regular sample from x[[1]]:
		values = as.numeric(as.vector(x[[1]])[seq(1, pdx, length.out = min(pdx, 10000))])
		n.unq = length(unique(na.omit(values)))
		breaks = if (! all(is.na(values)) && n.unq > 1) {
			if (utils::packageVersion("classInt") > "0.2-1")
				classInt::classIntervals(na.omit(values), min(nbreaks-1, n.unq), breaks, warnSmallN = FALSE)$brks
			else
				classInt::classIntervals(na.omit(values), min(nbreaks-1, n.unq), breaks)$brks
		} else
			range(values, na.rm = TRUE) # lowest and highest!
	}

	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
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
		if (length(dims) == 2 || !is.null(dots$rgb)) {
			image(x, ..., axes = axes, breaks = breaks, col = col)
			if (!is.null(main))
				title(main)
		} else { # simply loop over dimensions 3:
			mfrow = get_mfrow(st_bbox(x), dims[3], par("din"))
			title_size = if (is.null(main)) 
					0
				else
					1.1
			opar = par(mfrow = mfrow, mar = c(axes * 2.1, axes * 2.1, title_size, 0))
			on.exit(par(opar))
			labels = expand_dimensions(st_dimensions(x))[[3]]
			for (i in seq_len(dims[3])) {
				if (join_zlim)
					image(flatten(x, i), xlab = "", ylab = "", axes = axes, zlim = zlim, breaks = breaks, col = col, ...)
				else
					image(flatten(x, i), xlab = "", ylab = "", axes = axes, breaks = breaks, col = col, ...)
				if (!is.null(main)) {
					if (length(main) == dims[3])
						title(main[i])
					else
						title(paste(main, format(labels[i])))
				}
				box()
			}
		}
	} else if (has_sfc(x)) {
		plot(st_as_sf(x), ..., axes = axes)
	} else
		stop("no raster, no features geometries: no default plot method set up yet!")
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
#' @param ... passed on to \code{image.default}
#' @param text_values logical; print values as text on image?
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' image(x, col = grey((3:9)/10))
#' image(x, rgb = c(1,3,5)) # rgb composite
image.stars = function(x, ..., band = 1, attr = 1, asp = 1, rgb = NULL, maxColorValue = max(x[[attr]]),
		xlab = if (!axes) "" else names(dims)[1], ylab = if (!axes) "" else names(dims)[2],
		xlim = st_bbox(x)$xlim, ylim = st_bbox(x)$ylim, text_values = FALSE, axes = FALSE) {

	dots = list(...)
	stopifnot(!has_rotate_or_shear(x)) # FIXME: use rasterImage() with rotate, if only rotate & no shear

	if (any(dim(x) == 1))
		x = adrop(x)

	force(xlim)
	force(ylim)
	dims = expand_dimensions(x)

	y_is_neg = all(diff(dims$y) < 0)
	if (y_is_neg)
		dims[[2]] = rev(dims[[2]])

	ar = unclass(x[[ attr ]]) # raw data matrix/array
	if (!is.null(rgb)) {
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
		rasterImage(t(mat), xlim[1], ylim[1], xlim[2], ylim[2], interpolate = FALSE, ...)
	} else { 
		if (y_is_neg) {
			ar = if (length(dim(x)) == 3)
				ar[ , rev(seq_len(dim(ar)[2])), band]
			else
				ar[ , rev(seq_len(dim(ar)[2]))]
		}
		image.default(dims[[1]], dims[[2]], unclass(ar), asp = asp, xlab = xlab, ylab = ylab, 
			xlim = xlim, ylim = ylim, axes = axes,...)
	}
	if (text_values)
		text(do.call(expand.grid, dims[1:2]), labels = as.character(as.vector(ar))) # xxx
	if (axes) { # FIXME: deal with long/lat axes the way sf does
		axis(1)
		axis(2)
	}
}

#### copied from sf/R/plot.R -- remove on merge
get_mfrow = function(bb, n, total_size = c(1,1)) {
	asp = diff(bb[c(1,3)])/diff(bb[c(2,4)])
	size = function(nrow, n, asp) {
		ncol = ceiling(n / nrow)
		xsize = total_size[1] / ncol
		ysize = xsize  / asp
		if (xsize * ysize * n > prod(total_size)) {
			ysize = total_size[2] / nrow
			xsize = ysize * asp
		}
		xsize * ysize
	}
	sz = vapply(1:n, function(x) size(x, n, asp), 0.0)
	nrow = which.max(sz)
	ncol = ceiling(n / nrow)
	structure(c(nrow, ncol), names = c("nrow", "ncol"))
}

st_downsample = function(x, n) {
	stopifnot(all(n >= 0))
	d = dim(x)
	n = rep(n, length.out = length(d))
	args = rep(list(rlang::missing_arg()), length(d)+1)
	for (i in seq_along(d))
		if (n[i] > 1)
			args[[i+1]] = seq(1, d[i], n[i])
	eval(rlang::expr(x[!!!args]))
}

get_downsample = function(dims, px = dev.size("px")) { 
	floor(sqrt(prod(dims) / prod(px)))
}
