#' plot stars object, with subplots for each level of first non-spatial dimension
#'
#' @name plot.stars
#' @param x object of class \code{stars}
#' @param y ignored
#' @param one_zlim logical; if \code{TRUE}, compute a single zlim for all subplots from array range
#' @param main character; subplot title prefix; use \code{""} to get only time, use \code{NULL} to suppress subplot titles
#' @export
plot.stars = function(x, y, ..., one_zlim = TRUE, main = names(x)[1]) {
	flatten = function(x, i) {
		d = st_dimensions(x)
		dims = dim(x)
		x = x[[1]]
		aux = setdiff(names(dims), c("x", "y"))
		newdims = c(dims[c("x", "y")], prod(dims[aux]))
		dim(x) = newdims
		st_stars(list(x[,,i]), dimensions = d[c("x", "y")])
	}
	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
		zlim = if (one_zlim)
				range(unclass(x[[1]]), na.rm = TRUE)
			else
				rep(NA_real_, 2)
		dims = dim(x)
		if (length(dims) == 2) {
			image(x, ...)
			if (!is.null(main))
				title(main)
		} else { # simply loop over dimensions 3:
			mfrow = get_mfrow(st_bbox(x), dims[3], par("din"))
			title_sz = if (is.null(main)) 
					0
				else
					1.1
			par(mfrow = mfrow, mar = c(0, 0, title_sz, 0))
			labels = expand_dimensions(st_dimensions(x))[[3]]
			for (i in seq_len(dims[3])) {
				if (one_zlim)
					image(flatten(x, i), xlab = "", ylab = "", axes = FALSE, zlim = zlim,...)
				else
					image(flatten(x, i), xlab = "", ylab = "", axes = FALSE, ...)
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
		plot(st_as_sf(x), ...)
	} else
		stop("no raster, no features geometries: don't know how to plot!")
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
#' @param useRaster logical; see \link{image.default}
#' @param ... passed on to \code{image.default}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' image(x, col = grey((3:9)/10))
image.stars = function(x, ..., band = 1, attr = 1, asp = 1, rgb = NULL, maxColorValue = 1,
		xlab = names(dims)[1], ylab = names(dims)[2], xlim = st_bbox(x)$xlim,
		ylim = st_bbox(x)$ylim, useRaster = TRUE) {

	stopifnot(!has_affine(x)) # FIXME: use rasterImage() with rotate, if only rotate & no shear

	if (any(dim(x) == 1))
		x = adrop(x)

	force(xlim)
	force(ylim)
	dims = expand_dimensions(x)
	ar = unclass(x[[ attr ]])
	ar = if (length(dim(x)) == 3) {
			if (is.null(rgb))
				ar[ , rev(seq_len(dim(ar)[2])), band]
			else {
				stop("not yet supported")
				xy = dim(ar)[1:2]
				ar = structure(ar[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
				ar = rgb(ar, maxColorValue = maxColorValue) # FIXME: deal with NAs
				dim(ar) = xy
				#return(rasterImage(x[ , rev(seq_len(dim(x)[2]))], 0, 0, 1, 1, interpolate = FALSE))
			}
		} else
			ar[ , rev(seq_len(dim(ar)[2]))]
	image.default(dims[[1]], rev(dims[[2]]), unclass(ar), asp = asp, xlab = xlab, ylab = ylab, 
		xlim = xlim, ylim = ylim, useRaster = useRaster, ...)
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
