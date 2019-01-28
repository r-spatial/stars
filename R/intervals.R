make_intervals = function(start, end) {
	stopifnot(length(start) > 0, length(start) == length(end))
	structure(list(start = start, end = end), class = "intervals")
}

as_intervals = function(x, add_last = FALSE) {
	stopifnot(is.atomic(x))
	if (add_last)
		x = c(x, tail(x, 1) + diff(tail(x, 2)))
	make_intervals(start = head(x, -1), end = tail(x, -1))
}

length.intervals = function(x) length(x$start)

head.intervals = function(x, n) make_intervals(head(x$start, n), head(x$end, n))

tail.intervals = function(x, n) make_intervals(tail(x$start, n), tail(x$end, n))

as.data.frame.intervals = function(x) data.frame(start = x$start, end = x$end)

`[.intervals` = function(x, i, ...) {
	make_intervals(x$start[i], x$end[i])
}

format.intervals = function(x, ...) {
	paste0("[", format(x$start, ...), ",", format(x$end, ...), ")")
}
