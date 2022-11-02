# show that extract with exact=TRUE equals st_interpolate_aw(), only much faster
options(digits=15)
library(stars)
L = st_as_stars(L7_ETMs)[,,,1]
set.seed(123432)
st_bbox(L) |> 
  st_as_sfc() |> 
  st_centroid() |> 
  st_buffer(300) |>
  st_sample(10) |> 
  st_combine() |> 
  st_convex_hull() -> ch
if (interactive()) {
 plot(L, reset = FALSE)
 plot(ch, add = TRUE, border = 'red', col = NA)
}
e = st_extract(L, ch)[[1]]
a = aggregate(L, ch, mean)[[1]]
identical(e, a) # same code path
ee = aggregate(L, ch, mean, exact = TRUE)[[1]]
all.equal(e, ee)
aw = suppressWarnings(st_interpolate_aw(L, ch, extensive = FALSE)$V1)
all.equal(as.vector(ee), aw)
ee0 = st_extract(L, ch, exact = TRUE)[[1]]
all.equal(ee0, ee) # ??
