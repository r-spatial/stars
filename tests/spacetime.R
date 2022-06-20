suppressPackageStartupMessages(library(stars))
if (require(spacetime, quietly = TRUE)) {
set.seed(1331)
Sys.setenv(TZ="UTC")
example(STFDF)

x = st_as_stars(stfdf)
xx = as(x, "STFDF")

# as.xts.stars:
library(xts)
xts = as.xts(x)
st_as_stars(xts)
st_as_stars(xts, dimensions = st_dimensions(x))

library(sp)
pts = SpatialPoints(rbind(c(0,0), c(0,1), c(1,0), c(1,1)))
gridded(pts) = TRUE

tm = xts(1:10, as.Date("2010-01-01")+0:9)
stfdf = STFDF(pts, tm, data.frame(foo = seq_len(length(pts) * length(tm))))
x = st_as_stars(stfdf)
xx = as(x, "STFDF")

x = st_as_stars(as(stfdf, "STSDF"))

}
