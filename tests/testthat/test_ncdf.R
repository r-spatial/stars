context("read ncdf")
f <- system.file("nc/reduced.nc", package = "stars")

test_that("basic reduced comes back as expected", {
  expect_warning(nc <- read_ncdf(f), 
                 paste("No variables with a grid mapping found.\n",
                       "Defaulting to WGS84 Lon/Lat"))
  expect_equal(names(nc), c("sst", "anom", "err", "ice"))
  st_dim <- st_dimensions(nc)
  expect_equal(names(st_dim), c("lon", "lat", "zlev", "time"))
  
  expect_equal(st_dim$lon$delta, 2)
  expect_equal(length(st_dim$zlev$values), 1)
  expect_equal(length(st_dim$time$values), 1)
  
  expect(sf::st_crs(nc) == sf::st_crs("+proj=longlat +ellps=WGS84 +no_defs"))
})

test_that("variable subsetting", {
  expect_warning(  nc <- read_ncdf(f, var = c("anom")), 
                 paste("No variables with a grid mapping found.\n",
                       "Defaulting to WGS84 Lon/Lat"))
  expect_equal(names(nc), "anom")
})

test_that("domain subsetting", {
  expect_warning(nc <- read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), count = c(10, 12, 1, 1))), 
                   paste("No variables with a grid mapping found.\n",
                         "Defaulting to WGS84 Lon/Lat"))
  st_dim <- st_dimensions(nc)
  expect_equal(st_dim$lon$to - st_dim$lon$from, c("lon" = 9))
  expect_equal(st_dim$lat$to - st_dim$lat$from, c("lat" = 11))
})

test_that("crs from grid_mapping", {
  expect_warning(nc <- read_ncdf(system.file("extdata/daymet_sample.nc", package = "ncmeta")),
                 "missing coordinate variables names in coordinates attribute trying to find non-auxiliary coordinate variables.")
  
  expect_equal(sf::st_crs(nc), sf::st_crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
})
