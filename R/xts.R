as.xts.stars <- function(x,...) { 

	stopifnot(length(dim(x)) == 2)
	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov
	if (length(x) > 1)
		warning("all but first attribute will be ignored")

	ed = expand_dimensions.stars(x)
	time = which(sapply(ed, inherits, c("Date", "POSIXt")))
	xts::xts(st_upfront(x[1], time)[[1]], ed[[time]])
}
