## $values interface

# if the offset and delta fields are not set, dimension values need
# to be in the $values field of a dimension.
# this can be in two forms:
#   1. curvilinear grid (raster, 2d space only): here, a matrix keeps the values for all x (y) 
#      coordinates for the full set of coordinates
#   2. rectilinear grid (space, but also time or sth else): here, a data.frame with values
#      that specify the start, and that specify the end of the dimensions values

values_get_start = function(x) {
	stopifnot(inherits(x, "dimension"))
	v = x$values
	stopifnot(inherits(v, "intervals") && !is.null(v$start))
	v$start
}

values_get_end = function(x) {
	stopifnot(inherits(x, "dimension"))
	v = x$values
	stopifnot(inherits(v, "intervals") && !is.null(v$end))
	v$end
}

values_get_where = function(x, where) {
	if (where == 0.0)
		values_get_start(x)
	else if (where == 1.0)
		values_get_end(x)
	else {
		stopifnot(inherits(x, "dimension"))
		v = x$values
		stopifnot(inherits(v, "intervals"))
		v$start + where * (v$end - v$start)
	}
}

# x is an object of class dimension
# where specifies, in case of linear mapping, where in [0,1] the value is to be returned: 
#    0 begin, 1 end, 0.5 center
# geotransform: the geotransform (for rotated or sheared raster)
# what: "x" or "y" if geotransform is given
get_dimension_values = function(x, where = 0.0, geotransform, what = NA_character_) {
	stopifnot(inherits(x, "dimension"))
	stopifnot(where >= 0.0 && where <= 1.0)
	if (!is.null(x$values)) {
		if (is.atomic(x$values) || inherits(x$values, "sfc"))
			x$values
		else
			values_get_where(x, where)
	} else {
		if (! any(is.na(geotransform)))
			switch(what,
				x = xy_from_colrow(cbind(seq(x$from, x$to) - 1 + where, 0), geotransform)[,1],
				y = xy_from_colrow(cbind(0, seq(x$from, x$to) - 1 + where), geotransform)[,2],
				stop("argument what needs to be x or y")
			)
		else if (!any(c(is.na(x$offset), is.na(x$delta)))) {
			if (inherits(x$offset, "PCICt") && inherits(x$delta, "difftime")) # FIXME: why does this generate a warning, where POSIXct & difftime doesn't?
				suppressWarnings(seq(from = x$offset + (x$from - 1 + where) * x$delta, by = x$delta, length.out = x$to - x$from + 1))
			else
				seq(from = x$offset + (x$from - 1 + where) * x$delta, by = x$delta, length.out = x$to - x$from + 1)
		} else if (!is.na(x$offset) && x$from == 1 && x$to == 1)
			x$offset
		else
			seq(x$from, x$to)
	}
}

values_range = function(x) {
	stopifnot(inherits(x, "dimension"))
	range(as.vector(as.matrix(x$values)))
}

# sets the values (start, end) field of a dimension, from centers, from start, or from start & end.
# if end is not given, the last two intervals are assumed to be equal sized
# if centers is given, the first two, and the last two intervals are assumed to be equal sized.
set_dimension_values = function(..., start = NULL, end = NULL, centers = NULL) {
	if (!is.null(centers) && (!is.null(start) || !is.null(end)))
		stop("if centers is given, start and end should not be given")
	if (!is.null(centers)) {
		l = length(centers)
		if (l <= 1)
			stop("cannot derive cell boundaries from a single center: specify start and end")
		d = diff(centers)
		start = c(centers - 0.5 * c(d[1], d))
		end = c(start[-1], centers[l] + 0.5 * d[l-1])
	} else if (missing(end)) {
		l = length(start)
		end = if (l == 1)
				NA_real_ #stop("cannot infer end values from a single start value")
			else
				c(start[-1], start[l] + diff(c(start[l-1], start[l])))
	}
	make_intervals(start = start, end = end)
}

range.dimension = function(..., na.rm = FALSE) {
	dots = list(...)
	stopifnot(length(dots) == 1)
	x = dots[[1]]
	range(get_dimension_values(x, 0.0, NA, NA_character_), 
		get_dimension_values(x, 1.0, NA, NA_character_))
}
