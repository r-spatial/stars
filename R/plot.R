#' @export
plot.stars = function(x, y, ..., mfrow = c(10,10), names = names(x)[1]) {
	flatten = function(x, i) {
		d = st_dimensions(x)
		x = x[[1]]
		dims = dim(x)
		aux = setdiff(names(dims), c("x", "y"))
		newdims = c(dims[c("x", "y")], prod(dims[aux]))
		dim(x) = newdims
		st_stars(list(x[,,i]), dimensions = d[c("x", "y")])
	}
	if (!missing(y))
		stop("y argument should be missing")
	if (has_raster(x)) {
		dims = dim(x)
		if (length(dims) == 2)
			image(x, ...)
		else {
			n = ceiling(sqrt(dims[3]))
			par(mfrow = c(n,n), mar = c(0,0, if (is.null(names)) 0 else 1.1,0))
			for (i in seq_len(dims[3])) {
				image(flatten(x, i), xlab = "", ylab = "", axes = FALSE, ...)
				if (!is.null(names))
					title(paste0(names, ": ", i))
				box()
			}
		}
	} else if (has_sfc(x)) {
		plot(st_as_sf(x, ...), border = NA)
	} else
		stop("no raster, no features: don't know how to plot!")
}

#' @name st_stars
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
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' image(x, col = grey((3:9)/10))
image.stars = function(x, ..., band = 1, attr = 1, asp = 1, rgb = NULL, maxColorValue = 1,
		xlab = names(dims)[1], ylab = names(dims)[2], xlim = range(dims$x), ylim = range(dims$y)) {

	stopifnot(!has_affine(x))

	if (any(dim(x) == 1))
		x = adrop(x)

	dims = expand_dimensions(x)
	x = unclass(x[[ attr ]])
	x = if (length(dim(x)) == 3) {
			if (is.null(rgb))
				x[ , rev(seq_len(dim(x)[2])), band]
			else {
				stop("not yet supported")
				xy = dim(x)[1:2]
				x = structure(x[ , , rgb], dim = c(prod(xy), 3)) # flattens x/y
				x = rgb(x, maxColorValue = maxColorValue) # FIXME: deal with NAs
				dim(x) = xy
				#return(rasterImage(x[ , rev(seq_len(dim(x)[2]))], 0, 0, 1, 1, interpolate = FALSE))
			}
		} else
			x[ , rev(seq_len(dim(x)[2]))]
	image.default(dims[[1]], rev(dims[[2]]), unclass(x), asp = asp, xlab = xlab, ylab = ylab, 
		xlim = xlim, ylim = ylim, ...)
}
