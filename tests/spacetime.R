suppressPackageStartupMessages(library(stars))
library(spacetime)
set.seed(1331)
example(STFDF)

x = st_as_stars(stfdf)
xx = as(x, "STFDF")
