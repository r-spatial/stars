library(stars)
suppressPackageStartupMessages(library(dplyr))
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))

(y <- x %>% select(L7_ETMs.tif))
(y <- x %>% filter(band > 2))
(z <- x %>% mutate(foo = 2* L7_ETMs.tif))
x %>% pull(L7_ETMs.tif) %>% head
