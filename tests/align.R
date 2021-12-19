suppressPackageStartupMessages(library(stars))
set.seed(1331)
st_sample(st_as_sfc(st_bbox(L7_ETMs)), 1)
b = st_buffer(st_sample(st_as_sfc(st_bbox(L7_ETMs)), 1), 335)
bba = stars:::st_align(st_bbox(b), st_dimensions(L7_ETMs))

plot(L7_ETMs[,,,1], extent = st_buffer(b, 20), reset = FALSE, key.pos = NULL)
plot(b, add = TRUE, col = NA, border = 'red')
plot(st_as_sfc(st_bbox(b)), col = NA, border = 'red', add = TRUE)
plot(st_as_sfc(bba), col = NA, border = 'blue', add = TRUE)
st_bbox(b)
bba

plot(L7_ETMs[,,,1], reset = FALSE, key.pos = NULL)
plot(b, col = 'red', add = TRUE)
plot(st_as_sfc(bba), col = NA, border = 'blue', add = TRUE)

b0 = st_buffer(b, -100)
L7 = st_as_stars(L7_ETMs)[b0][,,,1] |> st_normalize()
bbb = stars:::st_align(st_bbox(b), st_dimensions(L7))
plot(L7, extent = st_buffer(b, 120), reset = FALSE, key.pos = NULL)
plot(st_as_sfc(st_bbox(L7)), col = NA, border = 'black', add = TRUE)
plot(st_as_sfc(bbb), col = NA, border = 'blue', add = TRUE)
plot(b, col = NA, border = 'red', add = TRUE)

all.equal(bbb, bba)
