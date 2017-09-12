## read raster/array dataset from file or connection

bind = function(lst, units) {
	ns = names(lst)
	if (length(lst)) {
		pr = lst[[1]]
		for (i in 1:length(lst)) { # includes 1 <--> 1, to drop attributes
			if (any(attr(pr, "cols") != attr(lst[[i]], "cols")))
				stop("cols don't match")
			if (any(attr(pr, "rows") != attr(lst[[i]], "rows")))
				stop("rows don't match")
			if (any(attr(pr, "geotransform") != attr(lst[[i]], "geotransform")))
				stop("geotransform parameters don't match")
			attributes(lst[[i]]) = NULL
			attr(lst[[i]], "dim") = dim(pr)
		}
	}
	lst = unlist(lst, recursive = FALSE)
	if (any(!is.na(units)))
		for (i in 1:length(lst))
			if (!is.na(units[i]))
				lst[[i]] = set_units(lst[[i]], make_unit(units[i]))
	attributes(lst) = attributes(pr)
	names(lst) = ns
	lst
}

create_dimensions = function(dims, properties) {
	if (!is.null(properties$properties)) # messy!
		properties = properties$properties
	df = data.frame(row.names = names(dims))
	df$from = 1
	df$from[1:2] = c(properties$cols[1], properties$rows[1])
	df$to = dims
	df$to[1:2] = c(properties$cols[2], properties$rows[2])
	df$offset = NA_real_
	df$delta = NA_real_
	df$geotransform = vector("list", nrow(df))
	if (! is.null(properties$geotransform)) {
		df[[1, "geotransform"]] = properties$geotransform
		df[[2, "geotransform"]] = properties$geotransform
		df$offset[1:2] = properties$geotransform[c(1,4)]
		df$delta[1:2] = properties$geotransform[c(2,6)]
	}
	df$refsys = NA_character_
	if (! is.null(properties$proj4string)) {
		df[[1, "refsys"]] = properties$proj4string
		df[[2, "refsys"]] = properties$proj4string
	}
	df
}

read_units = function(sub_ds, names) {
	units = rep(NA_character_, length(sub_ds))
	get_u = function(ds, nm) {
		md = st_get_metadata(ds)
		i = grep(paste0(nm, "#units"), md)
		if (length(i))
			strsplit(md[i], "=")[[1]][2]
		else
			NA_character_
	}
	for (i in 1:length(sub_ds))
		units[i] = get_u(sub_ds[[i]], names[i])
	print(units)
	units
}

#' read raster/array dataset from file or connection
#' @param file character; file name to read
#' @param options character; opening options
#' @param driver character; driver to use for opening file
#' @param keep_meta logical; should metadata be kept in attribute \code{properties}?
#' @return object of class \code{stars}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
st_stars = function(file, options = character(0), driver = character(0), keep_meta = FALSE) {
	properties = CPL_read_gdal(file, options, driver, TRUE)
	ret = if (properties$bands[2] == 0) { # read sub-datasets:
		sub_ds = st_get_subdatasets(file, options)
		ret = lapply(sub_ds, st_stars, options = options, driver = properties$driver[1], keep_meta = TRUE)
		names(ret) = sapply(strsplit(unlist(sub_ds), ":"), tail, 1)
		units = read_units(sub_ds, names(ret))
		bind(ret, units)
	} else  {
		structure(list(attr(properties, "data")),
			names = file,
			class = "stars",
			properties = structure(properties, data = NULL))
	}
	dimensions = create_dimensions(dim(ret[[1]]), attr(ret, "properties"))
	structure(ret, dimensions = dimensions, properties = if (keep_meta) properties else NULL)
}

#' @name st_stars
#' @param x stars object to plot
#' @param band integer; which band (dimension) to plot
#' @param attr integer; which attribute to plot
#' @param ... passed on to \code{image.default}
#' @export
#' @examples
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = st_stars(tif)
#' image(x)
image.stars = function(x, ..., band = 1, attr = 1) {
	x = x[[attr]]
	x = if (length(dim(x)) == 3)
			x[ , rev(seq_len(dim(x)[2])), band]
		else
			x[ , rev(seq_len(dim(x)[2]))]
	image.default(unclass(x), ...)
}

## @param x two-column matrix with columns and rows, as understood by GDAL; 0.5 refers to the first cell's center; 
xy_from_colrow = function(x, geotransform) {
# http://www.gdal.org/classGDALDataset.html , search for geotransform:
# 0-based indices:
# Xp = geotransform[0] + P*geotransform[1] + L*geotransform[2];
# Yp = geotransform[3] + P*geotransform[4] + L*geotransform[5];
	stopifnot(ncol(x) == 2)
	matrix(geotransform[c(1, 4)], nrow(x), 2, byrow = TRUE) + 
		x %*% matrix(geotransform[c(2, 3, 5, 6)], nrow = 2, ncol = 2)
}

#' @export
as.data.frame.stars = function(x, ...) {
	meta = attr(x, "properties")
	xc = xy_from_colrow(cbind(do.call(seq, as.list(meta$cols - 1)) + .5, 0), meta$geotransform)[,1]
	yc = xy_from_colrow(cbind(0, do.call(seq, as.list(meta$rows - 1)) + .5), meta$geotransform)[,2]
	band = do.call(seq, as.list(meta$bands))
	coords = expand.grid(x = xc, y = yc, band = band)
	data.frame(coords, z = c(x))
}

#' @export
print.stars = function(x, ...) {
	cat("stars object with", length(dim(x[[1]])), "dimensions and", length(x), if (length(x) > 1) "attributes\n" else "attribute\n")
	cat("attribute(s):\n")
	print(summary(data.frame(lapply(x, as.vector))))
	cat("dimension(s):\n")
	if (any(do.call(rbind, attr(x, "dimensions")$geotransform)[,c(3,5)] != 0))
		print(attr(x, "dimensions"))
	else
		print(attr(x, "dimensions")[,-5])
}
