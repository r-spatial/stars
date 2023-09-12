as.xts.stars <- function(x,...) { 

	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov
	if (length(x) > 1)
		message("only the first attribute will used; maybe use merge() first?")

	ed = expand_dimensions.stars(x)
	time = which(sapply(ed, inherits, c("Date", "POSIXt")))
	if (length(time) < 1)
		stop("no time dimension present in object")
	if (length(time) > 1) {
		message("more than one time dimension present in object; taking the first")
		time = time[1]
	}
	x = adrop(st_upfront(x[1], time), drop_xy = TRUE)
	if (length(dim(x)) > 2) {
		dims = setNames(c(dim(x)[1], prod(dim(x)[-1])), c(names(ed)[time], "other"))
		x = st_redimension(x, dims)
	} else { # set colnames
		d = st_dimensions(x)
		if (length(dim(x)) == 2 && length(d[[2]]$values) == dim(x)[2] && is.character(d[[2]]$values))
			colnames(x[[1]]) = d[[2]]$values
	}
	xts::xts(x[[1]], ed[[time]])
}

#' @name st_as_stars
#' @param name character; attribute name for array from an \code{xts} object
#' @details for the \code{xts} methods, if \code{dimensions} are provided, time has to be the first dimension.
#' @export
st_as_stars.xts = function(.x, ..., dimensions, name = "attr") {
	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov
	if (!requireNamespace("zoo", quietly = TRUE))
		stop("zoo required: install that first") # nocov
	time = zoo::index(.x)
	cn = colnames(.x)
	if (!missing(dimensions)) {
		.x = as.matrix(.x)
		dim(.x) = dim(dimensions)
		st_stars(setNames(list(.x), name), dimensions)
	} else {
		.x = st_as_stars(setNames(list(as.matrix(.x)), name))
		.x = st_set_dimensions(.x, 1, time)
		if (!is.null(cn))
			.x = st_set_dimensions(.x, 2, cn)
		st_set_dimensions(.x, names = c("time", "others"))
	}
}
