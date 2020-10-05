# Create 'stars' object
set.seed(1331)
library(stars)
volcano = rbind(volcano, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # add NA rows
d = st_dimensions(x = 1:ncol(volcano), y = 1:nrow(volcano))
(r = st_as_stars(t(volcano)))
r = st_set_dimensions(r, 1, offset = 0, delta = 1)
r = st_set_dimensions(r, 2, offset = nrow(volcano), delta = -1)

# Create points
pnt = st_sample(st_as_sfc(st_bbox(r)), 100)
pnt = st_as_sf(pnt)

# Extract - 'st_join'
x = st_join(pnt, st_as_sf(r))

# Extract - 'st_extract'
y = st_extract(r, pnt)

# check there are NA's:
any(is.na(x))
# Compare
all.equal(x$A1, y[[1]])

## tic: segfaults
# check equal results with stars_proxy:
#x = st_extract(stars:::st_as_stars_proxy(r), pnt)
#all.equal(x$A1, y[[1]])
#all.equal(x, y)

r = c(r, 2*r, 10*r)
x = st_join(pnt, st_as_sf(r))
y = st_as_sf(st_extract(r, pnt))
all.equal(x, y)

## tic: segfaults
#x = st_extract(stars:::st_as_stars_proxy(merge(r)), pnt)
#all.equal(st_as_sf(x), y)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
xp = read_stars(tif, proxy = TRUE)
xm = read_stars(tif, proxy = FALSE)
pts = st_sample(st_as_sfc(st_bbox(xp)), 10)
pts = c(pts, st_as_sfc("POINT(0 0)"), pts)
em = st_extract(xm, pts)
if (utils::packageVersion("sf") >= "0.9-7") {
	ep = st_extract(xp, pts)
	print(all.equal(ep, em, check.attributes = FALSE))
}
