#' @name st_as_stars
#' @export
#' @param check_times logical; should we check that the time stamps of all time series are identical?
st_as_stars.cubble_df = function(.x, ..., check_times = FALSE) {
    if (!requireNamespace("tsibble", quietly = TRUE))
        stop("package cubble required, please install it first") #nocov
    if (!requireNamespace("cubble", quietly = TRUE))
        stop("package cubble required, please install it first") #nocov
    if (!requireNamespace("dplyr", quietly = TRUE))
        stop("package dplyr required, please install it first") #nocov
	# time_column = cubble::index(.x)
	nr = sapply(.x$ts, nrow)
	stopifnot(length(unique(nr)) == 1)
	ts1 = .x$ts[[1]]
	dt = which(sapply(ts1, inherits, c("Date", "POSIXct", "units", "factor")))
	if (length(dt) > 1) {
		message("using only first time column for time index")
		dt = dt[1]
	}
	if (length(dt) == 0)
		stop("no time column found")
	times = ts1[[dt]]
	if (check_times)
		stopifnot(all(sapply(.x$ts, function(ts) identical(ts[[dt]], times))))
	else
		message("assuming times are identical for all elements in in the ts list column")
	m = do.call(rbind, lapply(.x$ts, function(df) df[-dt][[1]]))
	if (inherits(.x, "sf")) { # vector data cube
		sfc = st_geometry(.x)
		d = create_dimensions(list(
			   geometry = create_dimension(values = sfc),
			   time = create_dimension(values = times))
		)
		st_stars(list(values = m), d)
	} else { # raster:
		kv = tsibble::key_vars(.x)
		.x = dplyr::as_tibble(.x)
		for (k in kv)
			.x[[k]] = NULL
		.x$ts = NULL # remove time series payload
		.x = cbind(.x, m)
		merge(st_as_stars(.x))
	}
}
