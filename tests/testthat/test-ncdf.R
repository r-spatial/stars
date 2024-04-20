skip_on_cran()
context("read ncdf")
f <- system.file("nc/reduced.nc", package = "stars")

test_that("basic reduced comes back as expected", {
  skip_if_not_installed("ncmeta")
  nc <- read_ncdf(f)
  expect_equal(names(nc), c("sst", "anom", "err", "ice"))
  st_dim <- st_dimensions(nc)
  expect_equal(names(st_dim), c("lon", "lat", "zlev", "time"))

  expect_equal(st_dim$lon$delta, 2)
  expect_equal(length(st_dim$zlev$values), 1)
  expect_equal(length(st_dim$time$values), 1)
})

test_that("variable subsetting", {
  skip_if_not_installed("ncmeta")
  nc <- read_ncdf(f, var = c("anom"))
  expect_equal(names(nc), "anom")
})

test_that("domain subsetting", {
  skip_if_not_installed("ncmeta")
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
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/bcsd_obs_1999.nc", package = "stars")
  nc <- read_ncdf(f, package = "stars")
  expect_equal(names(nc), c("pr", "tas"))
  st_dim <- st_dimensions(nc)
  expect_equal(names(st_dim), c("longitude", "latitude", "time"))
  expect_equal(st_dim$longitude$delta, 0.125)
  
  file.copy(f, (tf <- tempfile()))
  
  nc_tf <- RNetCDF::open.nc(tf, write = TRUE)
  
  RNetCDF::var.put.nc(nc_tf, "longitude", 
  					(360 + stars::st_get_dimension_values(nc, "longitude")))
  
  RNetCDF::close.nc(nc_tf)
  
  nc <- read_ncdf(tf, package = "stars")
  
  expect_equal(stars::st_get_dimension_values(nc, "longitude")[1], -84.9375)
})

test_that("non canonical axis order is handled right", {
  skip_if_not_installed("ncmeta")
  expect_warning(nc <- read_ncdf(system.file("nc/3B42_Daily.19991231.7.test.nc",
                                             package = "stars")),
                 "Non-canonical axis order found, attempting to correct.")
  expect_equal(names(st_dimensions(nc)), c("lon", "lat"))
  expect_equal(st_dimensions(nc)[[1]]$to, 4L)
  expect_equal(st_dimensions(nc)[[2]]$to, 5L)
})


test_that("euro cordex extra dimvars", {
  skip_if_not_installed("ncmeta")
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

  expect_equal(sf::st_crs(out1), sf::st_crs(NULL))
  
  expect_true(attr(st_dimensions(out1), "raster")$curvilinear)
  expect_true(attr(st_dimensions(out2), "raster")$curvilinear)

  expect_equal(st_dimensions(out1), st_dimensions(out2))
})

test_that("curvilinear", {
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/test_stageiv_xyt.nc", package = "stars")

  out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat"))

  # expect_match(warn[1], "bounds for time seem to be reversed; reverting them")

  st_dim <- st_dimensions(out)

  expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))

  expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))

  expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))
  
  nc <- RNetCDF::open.nc(f)
  
  expect_equal(st_get_dimension_values(st_dim, "time"), 
  			 RNetCDF::utcal.nc(RNetCDF::att.get.nc(nc, "time", "units"),
  			 				  RNetCDF::var.get.nc(nc, "time"), type = "c"))
  RNetCDF::close.nc(nc)
  
  # Should also find the curvilinear grid.
  suppressWarnings(out <- read_ncdf(f, var = "Total_precipitation_surface_1_Hour_Accumulation"))

  expect_true(attr(st_dimensions(out), "raster")$curvilinear)

})

test_that("curvilinear broked", {
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/test_stageiv_xyt_borked.nc", package = "stars")

  expect_error(suppressMessages(read_ncdf(f, curvilinear = c("lon", "lat", "time"))),
               "Curvilinear input must be a length two character vector.")

  expect_error(read_ncdf(f, curvilinear = c("time", "time_bounds")),
               "Specified curvilinear coordinates are not 2-dimensional.")

  expect_error(suppressMessages(read_ncdf(f, curvilinear = c("lon", "time_bounds"))),
               "Specified curvilinear coordinate variables not found as X/Y coordinate variables.")

  warn <- capture_warnings(out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat")))

  expect_match(warn[1], "Non-canonical axis order found, attempting to correct.")

  st_dim <- st_dimensions(out)

  expect_true(all(st_dim$y$values < -74 & st_dim$y$values > -81))

  expect_true(all(st_dim$x$values < 38 & st_dim$x$values > 32))

  expect_equal(dim(st_dim$y$values), setNames(c(118, 87), c("y", "x")))
  
  expect_equal(as.character(st_dim$time$values), "2018-09-14 05:00:00")
  
})

test_that("high-dim from rasterwise", {
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/test-1.nc", package = "stars")
  out <- suppressWarnings(read_ncdf(f, var = "a"))

  expect_equal(names(st_dimensions(out)), c("x", "y", "c3", "c4", "c5"))
})

test_that("timeseries.nc", {
  skip_if_not_installed("ncmeta")
  skip_if_not_installed("ncdfgeom")
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
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/c201923412.out1_4.nc", package = "stars")
  nc <- read_ncdf(f)

  expect_equal(names(st_dimensions(nc)), c("nx", "ny", "time"))

  expect_true(attributes(st_dimensions(nc))$raster$curvilinear)
  expect_equal(attributes(st_dimensions(nc))$raster$dimensions, c("nx", "ny"))

})

test_that("lon cross 360", {
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/test_adaptor.cams_regional_fc.nc", package = "stars")

  suppressWarnings(nc <- read_ncdf(f))

  expect_true(head(st_dimensions(nc)$longitude$values, 1) < 0 &
                tail(st_dimensions(nc)$longitude$values, 1) > 0)
})

test_that("4d not 4d", {
  skip_if_not_installed("ncmeta")
  f <- system.file("nc/sub.nc", package = "stars")

  nc <- read_ncdf(f)

  dim <- st_dimensions(nc)

  expect_equal(dim$time$to, 10)

  expect_equal(dim$level$to, 2)

})

test_that("units are right with lcc km", {
	skip_if_not_installed("ncmeta")
	f <- system.file("nc/lcc_km.nc", package = "stars")
	
	nc <- expect_warning(read_ncdf(f), "prime meridian")
	
	expect_equal(units(sf::st_crs(nc)$ud_unit)$numerator, "km")
})

test_that("axis attribute order -- see #680", {
	
	# Example with two netCDFs
	tas <- array(
		data = rowSums(
			expand.grid(seq_len(9), 10 * (seq_len(2) - 1), 100 * (seq_len(3) - 1))
		),
		dim = c(9, 2, 3)
	)
	
	# File1 has no "axis" attributes
	file1 <- tempfile("tas_example_", fileext = ".nc")
	nc <- RNetCDF::create.nc(file1)
	
	id_lat <- RNetCDF::dim.def.nc(nc, "lat", 3)
	iv_lat <- RNetCDF::var.def.nc(nc, "lat", "NC_FLOAT", id_lat)
	RNetCDF::var.put.nc(nc, "lat", c(40, 45, 50))
	
	id_lon <- RNetCDF::dim.def.nc(nc, "lon", 2)
	iv_lon <- RNetCDF::var.def.nc(nc, "lon", "NC_FLOAT", id_lon)
	RNetCDF::var.put.nc(nc, "lon", c(-100, -95))
	
	id_bnds <- RNetCDF::dim.def.nc(nc, "bnds", 2)
	
	id_time <- RNetCDF::dim.def.nc(nc, "time", 9)
	iv_time <- RNetCDF::var.def.nc(nc, "time", "NC_INT", id_time)
	RNetCDF::var.put.nc(nc, "time", 1:9)
	
	iv_tas <- RNetCDF::var.def.nc(nc, "temperature", "NC_FLOAT", c(id_time, id_lon, id_lat))
	RNetCDF::var.put.nc(nc, "temperature", tas)
	
	RNetCDF::close.nc(nc)
	
	# File2 has X, Y, T axis attributes
	file2 <- tempfile("tas_example_", fileext = ".nc")
	file.copy(from = file1, to = file2)
	#> [1] TRUE
	nc <- RNetCDF::open.nc(file2, write = TRUE)
	
	RNetCDF::att.put.nc(nc, "lon", "axis", "NC_CHAR", "X")
	RNetCDF::att.put.nc(nc, "lat", "axis", "NC_CHAR", "Y")
	RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
	
	RNetCDF::close.nc(nc)
	
	file3 <- tempfile("tas_example_", fileext = ".nc")
	file.copy(from = file1, to = file3)
	#> [1] TRUE
	nc <- RNetCDF::open.nc(file3, write = TRUE)
	
	RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
	RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latidude")
	RNetCDF::att.put.nc(nc, "time", "standard_name", "NC_CHAR", "time")
	RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees")
	RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees")
	RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", "days since 1900-01-01")
	
	RNetCDF::close.nc(nc)

	s1 <- suppressWarnings(stars::read_ncdf(file3))
		
	expect_equal(names(stars::st_dimensions(s1)), c("time", "lon", "lat"))
	
	s2 <- suppressWarnings(stars::read_ncdf(file2))

	expect_equal(names(stars::st_dimensions(s2)), c("lon", "lat", "time"))

	s3 <- suppressWarnings(stars::read_ncdf(file3))
	
	expect_equal(names(stars::st_dimensions(s3)), c("time", "lon", "lat"))
})
