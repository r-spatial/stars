as.xts.stars <- function(x,...) { 
	stopifnot(length(dim(x)) == 2)
	if (length(x) > 1)
		warning("all but first attribute will be ignored")
	ed = expand_dimensions.stars(x)
	time = which(sapply(ed, inherits, c("Date", "POSIXt")))

	new_perm = c(time, setdiff(seq_along(ed), time))

	if (!requireNamespace("xts", quietly = TRUE))
		stop("xts required: install that first") # nocov

	xts::xts(aperm(x[1], new_perm)[[1]], ed[[time]])
}
