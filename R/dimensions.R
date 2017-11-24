#' get dimensions from stars object
#' @export
#' @param .x object to retrieve dimensions information from 
#' @param ... further arguments
#' @return the \code{dimensions} attribute of \code{x}, of class \code{dimensions}
st_dimensions = function(.x, ...) UseMethod("st_dimensions")

#' @export
#' @name st_dimensions
st_dimensions.stars = function(.x, ...) attr(.x, "dimensions")

#' @export
#' @name st_dimensions
st_dimensions.default = function(.x, ...) {
	d = list(...)
	if (! missing(.x))
		d = append(list(.x), d)
	ret = structure(lapply(d, function(y) create_dimension(values = y)), class = "dimensions")
	if (is.null(names(ret)) || any(names(ret) == ""))
		names(ret) = make.names(seq_along(ret))
	ret
}

create_dimension = function(from = 1, to, offset = NA_real_, delta = NA_real_, 
		geotransform = rep(NA_real_, 6), refsys = NA_character_, point = NA, values = NULL) {
	if (! is.null(values)) {
		from = 1
		to = length(values)
		if (is.character(values) || is.factor(values))
			values = as.character(values)
		else if (is.atomic(values) && length(ud <- unique(diff(values))) == 1) {
			offset = values[1]
			delta = values[2] - values[1]
			values = NULL
			if (inherits(offset, "POSIXct"))
				refsys = "POSIXct"
			if (inherits(offset, "Date"))
				refsys = "Date"
		}
		if (inherits(values, "sfc_POINT"))
			point = TRUE
		if (inherits(values, "sfc") && !is.na(st_crs(values)) && is.na(refsys))
			refsys = st_crs(values)$proj4string
	}
	structure(list(from = from, to = to, offset = offset, delta = delta, 
		geotransform = geotransform, refsys = refsys, point = point, values = values),
		class = "dimension")
}

create_dimensions = function(dims, pr = NULL) {
	if (!is.null(pr) && !is.null(pr$properties)) # messy!
		pr = pr$properties
	lst = vector("list", length(dims))
	names(lst) = names(dims)
	if (!is.null(pr)) {
		for (i in names(lst)) {
			lst[[i]] = switch(i,
				x = create_dimension(from = pr$cols[1], to = pr$cols[2], 
					offset = pr$geotransform[1], 
					delta = pr$geotransform[2], geotransform = pr$geotransform, 
					point = pr$point,
					refsys = if (is.null(pr$proj4string)) NA_character_ 
						else pr$proj4string),
				y = create_dimension(from = pr$rows[1], to = pr$rows[2], 
					offset = pr$geotransform[4],
					delta = pr$geotransform[6], geotransform = pr$geotransform, 
					point = pr$point,
					refsys = if (is.null(pr$proj4string)) NA_character_ 
						else pr$proj4string),
				create_dimension(from = 1, to = dims[i]) # time? depth+units?
			)
		}
	} else {
		for (i in names(lst))
			lst[[i]] = create_dimension(from = 1, to = dims[i])
	}
	if (! is.null(pr$dim_extra)) {
		for (d in names(pr$dim_extra)) {
			refsys = if (inherits(pr$dim_extra[[d]], "POSIXct")) "POSIXct" else NA_character_
			de = pr$dim_extra[[d]]
			diff.de = diff(de)
			lst[[d]] = if (length(unique(diff.de)) <= 1) {
					delta = if (length(diff.de)) diff.de[1] else NA_real_
					create_dimension(from = 1, to = length(de), offset = de[1], delta = delta, refsys = refsys)
				} else
					create_dimension(from = 1, to = length(de), values = de, refsys = refsys)
		}
		lst[["band"]] = NULL
	}
	structure(lst, class = "dimensions")
}

get_val = function(pattern, meta) {
	i = grep(pattern, meta)
	if (length(i))
		strsplit(meta[i], "=")[[1]][2]
	else
		NA_character_
}

parse_netcdf_meta = function(pr, name) {
	meta = pr$meta
	name = tail(strsplit(name, ":")[[1]], 1)
	# unit:
	pr$units = get_val(paste0(name, "#units"), meta)
	# extra dims: NETCDF_DIM_EXTRA={time,zlev}
	val = get_val("NETCDF_DIM_EXTRA", meta)
	if (! is.na(val)) {
		val = substr(val, 2, nchar(val)-1) # e.g. "{time,depth}" removes { }
		val = strsplit(val, ",")[[1]]
		if (length(val)) {
			pr$dim_extra = vector("list", length(val))
			names(pr$dim_extra) = val
			for (v in val) {
				rhs = get_val(paste0("NETCDF_DIM_", v, "_VALUES"), meta)
				if (!is.na(rhs)) {
					rhs = strsplit(gsub("[\\{\\}]" , "", rhs), ",")
					pr$dim_extra[[v]] = as.numeric(rhs[[1]])
				} else {
					rhs = get_val(paste0("NETCDF_DIM_", v), meta)
					pr$dim_extra[[v]] = as.numeric(rhs)
				}
				pr$dim_extra[[v]] = set_units(pr$dim_extra[[v]], 
					get_val(paste0(v, "#units"), meta))
				if (v == "time")
					pr$dim_extra[[v]] = as.POSIXct(pr$dim_extra[[v]])
			}
		}
	}
	pr
}

parse_meta = function(properties) {
	point = get_val("AREA_OR_POINT", properties$meta)
	properties$point = switch(point,
	  Area=FALSE,
	  Point=TRUE,
	  NA)
	properties
}

expand_dimensions = function(x, ...) UseMethod("expand_dimensions")

expand_dimensions.stars = function(x) {
	expand_dimensions(st_dimensions(x))
}

expand_dimensions.dimensions = function(x) {
	dimensions = x
	lst = vector("list", length(dimensions))
	names(lst) = names(dimensions)
	if ("x" %in% names(lst)) {
		x = dimensions[["x"]]
		gt = x$geotransform
		if (! all(is.na(gt)))
			lst[["x"]] = xy_from_colrow(cbind(seq(x$from, x$to) - .5, 0), gt)[,1]
		else
			stop("cannot determine x and y coordinates without geotransform")
	}
	if ("y" %in% names(lst)) {
		y = dimensions[["y"]]
		gt = y$geotransform
		if (! all(is.na(gt)))
			lst[["y"]] = xy_from_colrow(cbind(0, seq(y$from, y$to) - .5), gt)[,2]
		else
			stop("cannot determine x and y coordinates without geotransform")
	}
	for (nm in setdiff(names(lst), c("x", "y"))) {
		dm = dimensions[[nm]]
		lst[[nm]] = if (!is.null(dm$values))
				dm$values 
			else if (is.na(dm$offset) || is.na(dm$delta))
				seq(dm$from, dm$to)
			else
				seq(from = dm$offset, by = dm$delta, length.out = dm$to - dm$from + 1)
	}
	lst
}

#' @export
dim.dimensions = function(x) lengths(expand_dimensions(x))

#' @export
print.dimensions = function(x, ..., digits = 6) {
	lst = lapply(x, function(y) {
			aff = y$geotransform[c(3,5)]
			y$geotransform = if (any(!is.na(aff)) && any(aff != 0))
				paste(signif(y$geotransform, digits = digits), collapse = ", ")
			else
				NULL
			if (length(y$values) > 2)
				y$values = paste0(format(y$values[1]), ", ..., ", 
					format(tail(y$values, 1)))
			y
		}
	)
	lst = lapply(lst, function(x) lapply(x, format, digits = digits))
	ret = data.frame(do.call(rbind, lst), stringsAsFactors = FALSE)
	names(ret) = names(lst[[1]])
	print(ret)
}

check_equal_dimensions = function(lst) {
	if (length(lst) > 1) {
		for (i in 2:length(lst))
			if (!identical(attr(lst[[1]], "dimensions"), attr(lst[[i]], "dimensions")))
				stop(paste("object 1 and", i, "have different dimensions"))
	}
	TRUE
}

combine_dimensions = function(dots, along) {
	dims = attr(dots[[1]], "dimensions")
	offset = lapply(dots, function(x) attr(x, "dimensions")[[along]]$offset)
	offset = structure(do.call(c, offset), tzone = attr(offset[[1]], "tzone")) # preserve TZ
	if (length(unique(diff(offset))) == 1) { # regular & sorted
		dims[[along]]$offset = min(offset)
		dims[[along]]$delta = diff(offset)[1]
	} else {
		dims[[along]]$values = offset
		dims[[along]]$delta = NA_real_
	}
	dims[[along]]$from = 1
	dims[[along]]$to = length(offset)
	dims
}

#' @export
seq.dimension = function(from, ...) { # does what expand_dimensions also does, for single dimension
	if (!is.null(from$values))
		from$values
	else
		from$offset + (seq(from$from, from$to) - 1) * from$delta
}
