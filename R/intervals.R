#' create an intervals object
#' 
#' create an intervals object, assuming left-closed and right-open intervals
#' @param start vector with start values, or 2-column matrix with start and end values in column 1 and 2, respectively
#' @param end vector with end values
#' @export
make_intervals = function(start, end) {
	if (missing(end) && is.matrix(start) && ncol(start) == 2) {
		end = start[,2]
		start = start[,1]
	}
	stopifnot(length(start) > 0, length(start) == length(end))
	structure(list(start = start, end = end), class = "intervals")
}

as_intervals = function(x, add_last = FALSE) {
	stopifnot(is.atomic(x))
	if (add_last)
		x = c(x, tail(x, 1) + diff(tail(x, 2)))
	make_intervals(start = head(x, -1), end = tail(x, -1))
}

#' @export
length.intervals = function(x) length(x$start)

#' @export
head.intervals = function(x, n, ...) make_intervals(head(x$start, n, ...), head(x$end, n, ...))

#' @export
tail.intervals = function(x, n, ...) make_intervals(tail(x$start, n, ...), tail(x$end, n, ...))

#' @export
c.intervals = function(...) {
	dots = list(...)
	start = do.call(c, lapply(dots, function(x) x$start))
	end = do.call(c, lapply(dots, function(x) x$end))
	make_intervals(start, end)
}

#' @export
`[.intervals` = function(x, i, ...) {
	make_intervals(x$start[i], x$end[i])
}

#' @export
#' @importMethodsFrom CFtime range
format.intervals = function(x, digits = getOption("digits"), ...) {
	if (inherits(x$start, "units") && inherits(x$end, "units")) {
		stopifnot(units(x$start) == units(x$end))
		paste0("[", format(as.numeric(x$start), ...), ",", format(as.numeric(x$end), ...), ") ",
			"[", as.character(units(x$start)), "]")
	} else if (methods::is(x, "CFtime")) {
		rng = range(x)
		paste0("[", rng[1], ",", rng[2], ")")
	} else {
		paste0("[", format(x$start, digits = digits, ...), ",", format(x$end, digits = digits, ...), ")")
	}
}

find_interval = function(x, intervals) {
	if (inherits(intervals$start, "Date") && inherits(x, "POSIXct"))
		x = as.Date(x)
	if (inherits(x, "Date") && inherits(intervals$start, "POSIXct"))
		x = as.POSIXct(x)
	if (all(intervals$start > intervals$end)) { # decreasing intervals
		start = intervals$end
		intervals$end = intervals$start
		intervals$start = start
	}
	if (getRversion() < "4.1.0")
		stop("R >= 4.1.0 required for handling intervals")
	w = apply(outer(x, intervals$start, ">=") & outer(x, intervals$end, "<"), 1, which, simplify = FALSE)
	w[lengths(w) == 0] = NA_integer_
	unlist(w)
}

#' @export
as.list.intervals = function(x, ...) {
	structure(mapply(make_intervals, x$start, x$end, SIMPLIFY = FALSE),
		class = "intervals_list")
}

#' @export
format.intervals_list = function(x, ...) {
	sapply(x, format, ...)
}

#' @export
`[.intervals_list` = function(x, i, ...) {
	ret = NextMethod()
	is_null = sapply(ret, is.null)
	ret[is_null] = list(make_intervals(NA_real_, NA_real_))
	structure(ret, class = "intervals_list")
}
