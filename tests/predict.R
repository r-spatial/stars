suppressPackageStartupMessages(library(stars))
# predict:
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))
model = lm(x~L7_ETMs.tif, head(as.data.frame(x), 50))
x = predict(x, model)

(y = read_stars(tif, proxy = TRUE))
(y = predict(y, model))
all.equal(x, st_as_stars(y))
