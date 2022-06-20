require(dplyr, quietly = TRUE)

test_that("slice", {
  skip_if_not_installed("dplyr")

  f <- system.file("nc/reduced.nc", package = "stars")
  nc <- read_ncdf(f, proxy = TRUE)
  expect_s3_class(nc, "stars_proxy")

  # Slicing after other lazy operations should be lazy
  nc2 <- adrop(nc)
  nc3 <- slice(nc2, "lon", 90)
  expect_equal(length(attr(nc3, "call_list")), 2)

  # Slicing dimension that are part of the file should be lazy
  nc4 <- slice(nc, "lon", 90)
  expect_equal(length(attr(nc4, "call_list")), 1)

  # Trailing singleton dimensions in files
  nc5 <- slice(nc, "time", 1)
  expect_null(attr(nc5, "call_list"))
  expect_equal(st_dimensions(nc), st_dimensions(nc5))

  # concatenate to simulate an extra dimension
  nc6 <- c(nc, nc, nc, nc, along = "dummy")
  expect_equal(dim(nc6)["dummy"], c(dummy = 4))
  expect_s3_class(nc6, "stars_proxy")

  nc7 <- slice(nc6, "dummy", 1)
  expect_s3_class(nc7, "stars_proxy")
  expect_equal(dim(nc7)["dummy"], c(dummy = 1))
  expect_null(attr(nc7, "call_list"))
  expect_equal(names(nc7), names(nc6))

  nc8 <- slice(nc6, "dummy", 2:3)
  expect_s3_class(nc8, "stars_proxy")
  expect_equal(dim(nc8)["dummy"], c(dummy = 2))
  expect_null(attr(nc8, "call_list"))
  expect_equal(names(nc8), names(nc6))
})
