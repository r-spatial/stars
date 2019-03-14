suppressPackageStartupMessages(library(stars))
# predict:
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))
model = lm(x~L7_ETMs.tif, head(as.data.frame(x), 50))
x = predict(x, model)

(y = read_stars(tif, proxy = TRUE))
(yp = predict(y, model))
all.equal(x, st_as_stars(yp))

yy = st_set_dimensions(y, 3, values = paste0("band", 1:6))
(y_spl = split(yy, 3))
as.data.frame(st_as_stars(split(yy, 3)))[1:3,]
