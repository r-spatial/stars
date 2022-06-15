suppressPackageStartupMessages(library(stars))

if (require(dplyr, quietly = TRUE)) {
tif = system.file("tif/L7_ETMs.tif", package = "stars")
(x = read_stars(tif))

(y <- x %>% select(L7_ETMs.tif))
(y <- x %>% filter(band > 2))
(z <- x %>% mutate(foo = 2* L7_ETMs.tif))
x %>% pull(L7_ETMs.tif) %>% class()
x %>% filter(x > 289900)
x %>% slice("x", 50:100)

suppressPackageStartupMessages(library(ggplot2))
ggplot() + geom_stars(data = x) + coord_equal() + facet_wrap(~band)
ggplot() + geom_stars(data = x[,c(1:50,52:100),c(1:45, 50:60)]) +
	coord_equal()
ggplot() + geom_stars(data = x[,1:10,1:10,1:2], sf = TRUE) + facet_wrap(~band)


# see https://github.com/r-spatial/stars/issues/539 :
x <- read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
y <- transmute(x, L7_ETMs.tif = units::set_units(L7_ETMs.tif, degree_C)) # same as x but with units

x - slice(x, 'band', 1) # runs as expected, recycling the second object
try(y - slice(x, 'band', 1)) # fails as expected because x is unitless
try(x - slice(y, 'band', 1)) # fails as expected 
y - slice(y, 'band', 1) # runs as expected
}
