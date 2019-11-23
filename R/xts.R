as.xts.stars <- function(x,...) { 

	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov
	if (length(x) > 1)
		warning("all but first attribute will be ignored")

	ed = expand_dimensions.stars(x)
	time = which(sapply(ed, inherits, c("Date", "POSIXt")))
	if (length(time) < 1)
		stop("no time dimension present in object")
	if (length(time) > 1) {
		message("more than one time dimension present in object; taking the first")
		time = time[1]
	}
	x = st_upfront(x[1], time)
	if (length(dim(x)) > 2)
		x = st_redimension(x, c(dim(x)[1], prod(dim(x)[-1])))
	
	xts::xts(x[[1]], ed[[time]])
}

#' @name st_as_stars
#' @details for the \code{xts} methods, if \code{dimensions} are provided, time has to be the first dimension.
#' @export
st_as_stars.xts = function(.x, ..., dimensions) {
	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov
	if (!requireNamespace("zoo", quietly = TRUE))
		stop("zoo required: install that first") # nocov
	time = zoo::index(x)
	cn = colnames(x)
	if (!missing(dimensions)) {
		x = as.matrix(x)
		dim(x) = dim(dimensions)
		st_stars(list(x), dimensions)
	} else {
		x = st_as_stars(list(as.matrix(x)))
		x = st_set_dimensions(x, 1, time)
		if (!is.null(cn))
			x = st_set_dimensions(x, 2, cn)
		st_set_dimensions(x, names = c("time", "others"))
	}
}
