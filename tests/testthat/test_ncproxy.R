f <- system.file("nc/reduced.nc", package = "stars")

test_that("proxy", {
	nc <- read_ncdf(f, proxy = TRUE)
	expect_equal(as.character(nc[[1]]), f)
	expect_s3_class(nc, "stars_proxy")

	output <- capture_output(print(nc))

	expect_true(grepl("^netcdf source", output))
	expect_true(grepl("nc_request:", output))

	old_opts <- options("stars.n_proxy" = 100)
	nc <- read_ncdf(f)
	expect_equal(as.character(nc[[1]]), f)

	expect_warning(nc <- read_ncdf(f, proxy = FALSE))
	expect_s3_class(nc, "stars")

	options(old_opts)
})

test_that("st_as_stars.nc_proxy", {

	nc <- read_ncdf(f, proxy = TRUE)

	nc_request <- stars:::get_nc_request(nc)
	nc_request$sst$start <- c(25, 25, 1, 1)
	nc_request$sst$count <- c(10, 10, 1, 1)
	nc_request$sst$size <- prod(nc_request$sst$count)

	nc2 <- nc

	attr(nc2$sst, "nc_request") <- nc_request["sst"]

	nc2 <- st_as_stars(nc2)

	expect_s3_class(nc2, 'stars', exact = TRUE)
	expect_equal(names(nc2), "sst")

	dim <- st_dimensions(nc2)

	expect_equal(dim$lon$to, 10)
	expect_equal(dim$lon$offset, 47)

	nc2 <- st_as_stars(nc, var = "sst",
					   ncsub = cbind(start = c(25, 25, 1, 1),
					   			  count = c(10, 10, 1, 1)),
					   proxy = TRUE)

	expect_s3_class(nc2, c("nc_proxy", "stars_proxy", "stars"), exact = TRUE)

	expect_equal(names(attr(nc2, "nc_request")), "sst")

	dim <- st_dimensions(nc2)

	expect_equal(dim$lon$to, 10)
	expect_equal(dim$lon$offset, 47)

})

test_that("basics", {
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
