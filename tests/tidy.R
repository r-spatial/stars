library(stars)
suppressPackageStartupMessages(library(dplyr))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))

(y <- x %>% select(L7_ETMs.tif))
(y <- x %>% filter(band > 2))
