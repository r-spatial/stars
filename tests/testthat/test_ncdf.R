context("read ncdf")
f <- system.file("nc/reduced.nc", package = "stars")

test_that("basic reduced comes back as expected", {
  nc <- read_ncdf(f)
  expect_equal(names(nc), c("sst", "anom", "err", "ice"))
  st_dim <- st_dimensions(nc)
  expect_equal(names(st_dim), c("lon", "lat", "zlev", "time"))
  
  expect_equal(st_dim$lon$delta, 2)
  expect_equal(length(st_dim$zlev$values), 1)
  expect_equal(length(st_dim$time$values), 1)
})

test_that("variable subsetting", {
  nc <- read_ncdf(f, var = c("anom"))
  expect_equal(names(nc), "anom")
})

test_that("domain subsetting", {
  nc <- read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), 
                                   count = c(10, 12, 1, 1)))
  st_dim <- st_dimensions(nc)
  expect_equal(st_dim$lon$to - st_dim$lon$from, c("lon" = 9))
  expect_equal(st_dim$lat$to - st_dim$lat$from, c("lat" = 11))
  
  expect_error(nc <- read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), 
                                   count = c(200, 12, 1, 1))),
               "start or count out of bounds")
  
  expect_error(nc <- read_ncdf(f, ncsub = cbind(start = c(1, 1, 1), 
                                                count = c(200, 12, 1))),
               "input ncsub doesn't match available dims")
  
  
  
  
  # Leaving this here -- NA or -1 counts should return all but this causes other errors.
  # nc <- read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), 
  #                                  count = c(NA, NA, 1, 1)))
  # st_dim <- st_dimensions(nc)
  # expect_equal(st_dim$lon$to - st_dim$lon$from, c("lon" = 179))
  # expect_equal(st_dim$lat$to - st_dim$lat$from, c("lat" = 89))
})

test_that("normal bcsd", {
  nc <- read_ncdf(system.file("nc/bcsd_obs_1999.nc", package = "stars"))
  expect_equal(names(nc), c("pr", "tas"))
  st_dim <- st_dimensions(nc)
  expect_equal(names(st_dim), c("longitude", "latitude", "time"))
  expect_equal(st_dim$longitude$delta, 0.125)
})

test_that("non canonical axis order is handled right", {
  expect_warning(nc <- read_ncdf(system.file("nc/3B42_Daily.19991231.7.test.nc", 
                                             package = "stars")),
                 "Non-canonical axis order found, attempting to correct.")
  expect_equal(names(st_dimensions(nc)), c("lon", "lat"))
  expect_equal(st_dimensions(nc)[[1]]$to, c(lon = 4L))
  expect_equal(st_dimensions(nc)[[2]]$to, c(lat = 5L))
})

# 
# test_that("euro cordex extra dimvars", {
#   f <- system.file("nc/EURO-CORDEX_81_DOMAIN000.nc", package = "stars")
#   
#   suppressWarnings(out <- read_ncdf(f))
#   
#   expect_equal(names(out), c("topo", "xlat", "xlon"))
#   
#   expect(sf::st_crs(out) == sf::st_crs("+proj=lcc +lat_1=30 +lat_2=65 +lat_0=48 +lon_0=9.75 +x_0=-6000 +y_0=-6000 +a=6371229 +b=6371229 +units=m +no_defs"))
# })

test_that("curvilinear", {
  f <- system.file("nc/test_stageiv_xyt.nc", package = "stars")
  
  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat")))
  
  expect_match(warn[1], "bounds for time seem to be reversed; reverting them")
  
  st_dim <- st_dimensions(out)
  
  expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))
  
  expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))
  
  expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))

  # Should also find the curvilinear grid.  
  suppressWarnings(out <-read_ncdf(f, var = "Total_precipitation_surface_1_Hour_Accumulation"))
  
  expect_true(attr(st_dimensions(out), "raster")$curvilinear)
  
})

test_that("curvilinear broked", {
  f <- system.file("nc/test_stageiv_xyt_borked.nc", package = "stars")
  
  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat")))
  
  expect_match(warn[1], "Non-canonical axis order found, attempting to correct.")

  st_dim <- st_dimensions(out)
  
  expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))
  
  expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))
  
  expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))
})
