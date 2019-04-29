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

# Leaving these here but commented in case there is interest in more robust handling of odd files.
# see dblodgett-usgs/stars for sample files or PR #87 in r-spatial/stars.
# test_that("broken bcsd", {
#   expect_warning(nc <- read_ncdf(system.file("nc/bcsd_obs_1999_borked.nc", package = "stars")),
#                  "Found non-canonical axis order in NetCDF unexpected bahavior may result.")
# })
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
  
  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c("lon", "lat")))
  
  expect_match(warn[1], "Could not parse expression:.*Returning as a single symbolic unit()")
  expect_match(warn[2], "ignoring unrecognized unit: kg m\\^-2")
  expect_match(warn[3], "bounds for time seem to be reversed; reverting them")
  
  st_dim <- st_dimensions(out)
  
  expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))
  
  expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))
  
  expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))
})
