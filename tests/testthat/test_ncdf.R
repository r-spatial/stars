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
  nc <- read_ncdf(f, ncsub = cbind(start = c(20, 1, 1, 1),
                                   count = c(10, 12, 1, 1)))
  st_dim <- st_dimensions(nc)
  expect_equal(st_dim$lon$to - st_dim$lon$from, 9)
  expect_equal(st_dim$lat$to - st_dim$lat$from, 11)
  expect_equal(st_dim$lon$offset, 37)

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
  expect_equal(st_dimensions(nc)[[1]]$to, 4L)
  expect_equal(st_dimensions(nc)[[2]]$to, 5L)
})


test_that("euro cordex extra dimvars", {
  f <- unzip(zipfile = system.file("nc/EURO-CORDEX_81_DOMAIN000_mask_int.nc.zip", package = "stars"), exdir = tempdir())

  expect_warning(out <- read_ncdf(f, var = "mask"),
                 "Didn't find a longitude of prime meridian for datum, assuming 0.")

  expect_equal(names(out), c("mask"))

  #expect_true(sf::st_crs(out) ==
  #sf::st_crs("+proj=lcc +lat_1=30 +lat_2=65 +lat_0=48 +lon_0=9.75 +x_0=-6000 +y_0=-6000 +a=6371229 +b=6371229 +units=m +no_defs"))

  expect_error(out <- suppressWarnings(read_ncdf(f, var = "mask", curvilinear = c("X", "Y"))),
                 "Curvilinear variables not found in file.")

  out1 <- suppressWarnings(read_ncdf(f, var = "mask", curvilinear = c("xlat", "xlon")))
  out2 <- suppressWarnings(read_ncdf(f, var = "mask", curvilinear = c("xlon", "xlat")))

  expect_true(attr(st_dimensions(out1), "raster")$curvilinear)
  expect_true(attr(st_dimensions(out2), "raster")$curvilinear)

  expect_equal(st_dimensions(out1), st_dimensions(out2))
})

test_that("curvilinear", {
  f <- system.file("nc/test_stageiv_xyt.nc", package = "stars")

  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat")))

  expect_match(warn[1], "bounds for time seem to be reversed; reverting them")

  st_dim <- st_dimensions(out)

  expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))

  expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))

  expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))

  # Should also find the curvilinear grid.
  suppressWarnings(out <- read_ncdf(f, var = "Total_precipitation_surface_1_Hour_Accumulation"))

  expect_true(attr(st_dimensions(out), "raster")$curvilinear)

})

test_that("curvilinear broked", {
  f <- system.file("nc/test_stageiv_xyt_borked.nc", package = "stars")

  expect_error(suppressMessages(read_ncdf(f, curvilinear = c("lon", "lat", "time"))),
               "Curvilinear input must be a length two character vector.")

  expect_error(read_ncdf(f, curvilinear = c("time", "time_bounds")),
               "Specified curvilinear coordinates are not 2-dimensional.")

  expect_warning(suppressMessages(read_ncdf(f, curvilinear = c("lon", "time_bounds"))),
               "Specified curvilinear coordinate variables not found as X/Y coordinate variables.")

  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat")))

  expect_match(warn[1], "Non-canonical axis order found, attempting to correct.")

  st_dim <- st_dimensions(out)

  expect_true(all(st_dim$y$values < -74 & st_dim$y$values > -81))

  expect_true(all(st_dim$x$values < 38 & st_dim$x$values > 32))

  expect_equal(dim(st_dim$y$values), setNames(c(118, 87), c("y", "x")))
})

test_that("high-dim from rasterwise", {
  f <- system.file("nc/test-1.nc", package = "stars")
  out <- suppressWarnings(read_ncdf(f, var = "a"))

  expect_equal(names(st_dimensions(out)), c("x", "y", "c3", "c4", "c5"))
})

test_that("timeseries.nc", {
  skip_on_os("solaris")

  f <- system.file("nc/timeseries.nc", package = "stars")
  nc <- read_ncdf(f)
  dims <- st_dimensions(nc)
  expect_s3_class(nc, "stars")
  expect_equal(names(nc), "pr")
  expect_equal(names(dims), c("time", "points"))

  poly <- sf::st_buffer(st_transform(st_sf(id = seq_along(dims$points$values),
                              geom = dims$points$values, crs = 4326), 3857),
                        dist = 100, nQuadSegs = 1)

  temp_f <- tempfile()
  file.copy(f, temp_f)

  suppressWarnings(temp_f <- ncdfgeom::write_geometry(temp_f, poly, "station", "pr"))
  nc <- read_ncdf(temp_f)

  dims <- st_dimensions(nc)
  expect_s3_class(nc, "stars")
  expect_equal(names(nc), "pr")
  expect_equal(names(dims), c("time", "points", "geometry"))
})

test_that("curvilinear 2", {
  f <- system.file("nc/c201923412.out1_4.nc", package = "stars")
  nc <- read_ncdf(f)

  expect_equal(names(st_dimensions(nc)), c("nx", "ny", "time"))

  expect_true(attributes(st_dimensions(nc))$raster$curvilinear)
  expect_equal(attributes(st_dimensions(nc))$raster$dimensions, c("nx", "ny"))

})

test_that("lon cross 360", {
  f <- system.file("nc/test_adaptor.cams_regional_fc.nc", package = "stars")

  suppressWarnings(nc <- read_ncdf(f))

  expect_true(head(st_dimensions(nc)$longitude$values, 1) < 0 &
                tail(st_dimensions(nc)$longitude$values, 1) > 0)
})

test_that("4d not 4d", {
  f <- system.file("nc/sub.nc", package = "stars")

  nc <- read_ncdf(f)

  dim <- st_dimensions(nc)

  expect_equal(dim$time$to, 10)

  expect_equal(dim$level$to, 2)

})
