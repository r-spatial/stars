suppressPackageStartupMessages(library(stars))
library(spacetime)
set.seed(1331)
Sys.setenv(TZ="UTC")
example(STFDF)

x = st_as_stars(stfdf)
xx = as(x, "STFDF")

library(sp)
pts = SpatialPoints(rbind(c(0,0), c(0,1), c(1,0), c(1,1)))
gridded(pts) = TRUE

library(xts)
tm = xts(1:10, as.Date("2010-01-01")+0:9)
stfdf = STFDF(pts, tm, data.frame(foo = seq_len(length(pts) * length(tm))))
x = st_as_stars(stfdf)
xx = as(x, "STFDF")
