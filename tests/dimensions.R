# https://github.com/r-spatial/stars/issues/295
suppressPackageStartupMessages(library(stars))
raw <- read_stars(system.file("nc/bcsd_obs_1999.nc", package = "stars"))
foo <- function(x, idx) stats::lowess(idx, x)$y

timeline <- st_get_dimension_values(raw, "time")

smooth = st_apply(raw,
	MARGIN = c("x", "y"),
	FUN = foo,
	idx = st_get_dimension_values(raw, "time")
)

st_set_dimensions(smooth,
	which = "foo",
	values = st_get_dimension_values(raw, "time"),
	names = "time"
)

raw %>%
  st_apply(MARGIN = c("x", "y"), FUN = foo, idx = timeline) %>%
  st_set_dimensions("foo", st_dimensions(raw)["time"])
