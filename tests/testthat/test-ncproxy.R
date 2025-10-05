skip_on_cran()
f <- system.file("nc/reduced.nc", package = "stars")

test_that("proxy", {
	skip_if_not_installed("ncmeta")
	nc <- read_ncdf(f, proxy = TRUE)
	expect_equal(as.character(nc[[1]]), f)
	expect_s3_class(nc, "stars_proxy")

	output <- capture_output(print(nc))

	expect_true(grepl("^netcdf source", output))

	old_opts <- options("stars.n_proxy" = 100)
	nc <- read_ncdf(f)
	expect_equal(as.character(nc[[1]]), f)

	expect_warning(nc <- read_ncdf(f, proxy = FALSE))
	expect_s3_class(nc, "stars")

	options(old_opts)
})

test_that("st_as_stars.nc_proxy", {
    skip_if_not_installed("ncmeta")

	nc <- read_ncdf(f, proxy = TRUE)

	nc2 <- nc[1, 25:35, 25:35, 1, 1]
	
	nc2 <- st_as_stars(nc2)

	expect_s3_class(nc2, 'stars', exact = TRUE)
	expect_equal(names(nc2), "sst")

	dim <- st_dimensions(nc2)

	expect_equal(dim$lon$to, 11)
	expect_equal(dim$lon$offset, 47)

	nc2 <- st_as_stars(nc, var = "sst",
					   proxy = TRUE)

	expect_s3_class(nc2, c("nc_proxy", "stars_proxy", "stars"), exact = TRUE)

	dim <- st_dimensions(nc2)

	expect_equal(dim$lon$to, 180)
	expect_equal(dim$lon$offset, -1)

})

test_that("basics", {
    skip_if_not_installed("ncmeta")
	nc <- read_ncdf(f, proxy = TRUE)
	
	x <- nc["sst"]

	expect_s3_class(x, "nc_proxy")
	
	expect_equal(length(x), 1)
	
	expect_equal(names(x), "sst")
	
	nc[[5]] <- nc[[4]]
	
	expect_equal(length(nc), 5)
	
	expect_equal(names(nc), c("sst", "anom", "err", "ice", ""))
	
	xx <- c(nc["sst"], nc["ice"])
	
	expect_equal(length(xx), 2)
	
	expect_equal(names(xx), c("sst", "ice"))
	
	expect_error(st_mosaic(nc))
	
	expect_error(st_redimension(nc))
})

test_that("subset", {
	skip_if_not_installed("ncmeta")
	nc <- read_ncdf(f, proxy = TRUE)
	
	nc2 <- nc[1, 80:100, 45:55, , ]
	
	nc3 <- nc["sst", 80:100, 45:55, , ]
	
	expect_equal(nc2, nc3)
	
	expect_equal(dim(nc2), 
				 setNames(c(21, 11, 1, 1), 
				 		 c("lon", "lat", "zlev", "time")))
	
	nc3 <- st_as_stars(nc2)
	
	expect_s3_class(nc3, "stars")
	
	expect_equal(as.numeric(dim(nc3)), dim(nc3$sst))
	
	nc <- read_ncdf(system.file("nc/bcsd_obs_1999.nc", package = "stars"),
					proxy = TRUE)
	
	nc2 <- nc[ , , , 5]
	expect_equal(st_dimensions(nc2)$time$from, 5)
	expect_equal(st_dimensions(nc2)$time$values, 
				 structure(928108800, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
	
	nc3 <- st_as_stars(nc2)
	
	expect_equal(dim(nc3$pr), as.numeric(dim(nc2)))
	
	nc_sf <- sf::st_transform(
		read_sf(system.file("gpkg/nc.gpkg", package="sf")),
		sf::st_crs(nc))
	
	nc2 <- st_crop(nc, nc_sf[10, ])
	nc2 <- nc2["pr", , , 1]
	nc2 <- st_as_stars(nc2)

	nc3 <- nc["pr", , , 1]	
	nc3 <- st_crop(nc3, nc_sf[10, ])
	nc3 <- st_as_stars(nc3)
	
	expect_equal(nc2, nc3)
})

test_that("curvilinear", {
	skip_if_not_installed("ncmeta")
	f <- system.file("nc/test_stageiv_xyt.nc", package = "stars")
	
	out <-read_ncdf(f, curvilinear = c(X = "lon", Y = "lat"), 
					proxy = TRUE)
	
	out <- st_as_stars(out)
	
	st_dim <- st_dimensions(out)
	
	expect_true(all(st_dim$x$values < -74 & st_dim$x$values > -81))
	
	expect_true(all(st_dim$y$values < 38 & st_dim$y$values > 32))
	
	expect_equal(dim(st_dim$y$values), setNames(c(87, 118), c("x", "y")))
	
	# Should also find the curvilinear grid.
	suppressWarnings(
		out <- read_ncdf(f, var = "Total_precipitation_surface_1_Hour_Accumulation",
						 proxy = TRUE))
	
	expect_true(attr(st_dimensions(out), "raster")$curvilinear)
	
})
