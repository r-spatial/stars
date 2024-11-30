# https://github.com/r-spatial/stars/issues/295
suppressPackageStartupMessages(library(stars))
raw <- read_stars(system.file("nc/bcsd_obs_1999.nc", package = "stars"))
foo <- function(x, idx) stats::lowess(idx, x)$y

timeline <- st_get_dimension_values(raw, "time")
offsets <- CFtime::offsets(timeline)

smooth = st_apply(raw,
	MARGIN = c("x", "y"),
	FUN = foo,
	idx = offsets
)

st_set_dimensions(smooth,
	which = "foo",
	values = timeline,
	names = "time"
)

raw %>%
  st_apply(MARGIN = c("x", "y"), FUN = foo, idx = offsets) %>%
  st_set_dimensions("foo", timeline, names = "time")
