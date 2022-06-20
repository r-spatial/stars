context("st_as_stars tests")

options(rgdal_show_exportToProj4_warnings = "none")

test_that("basic st_as_stars", {
  skip_if_not_installed("ncdfgeom")
  skip_on_os("solaris")

  f <- system.file("nc/timeseries.nc", package = "stars")

  test_list <- ncdfgeom::read_timeseries_dsg(f)

  stars_obj <- st_as_stars(test_list)

  expect_true("XYZ" %in% class(st_dimensions(stars_obj)$points$values[[1]]))

  expect_s3_class(stars_obj, "stars")

  dim <- stars::st_dimensions(stars_obj)
  #expect_true(sf::st_crs(dim$points$refsys) == sf::st_crs(4326))
  expect_equal(dim$time$refsys, "POSIXct")

  expect_s3_class(dim$points$values, "sfc_POINT")

  expect_true(dim$points$point)

  geom_point <- sf::st_as_sf(data.frame(id = names(test_list$data_frames[[1]]),
                                        lon = test_list$lons,
                                        lat = test_list$lats, stringsAsFactors = FALSE),
                             coords = c("lon", "lat"), crs = 4326)

  geom_poly <- sf::st_buffer(st_transform(geom_point, 3857), dist = 100)

  stars_obj <- st_as_stars(test_list, sf_geometry = geom_poly)

  dim <- stars::st_dimensions(stars_obj)
  expect_equal(sf::st_crs(dim$geometry$refsys)$proj4string,
               sf::st_crs(geom_poly)$proj4string)

  expect_s3_class(dim$geometry$values, "sfc_POLYGON")
  expect_false(dim$geometry$point)

  stars_obj <- st_as_stars(test_list, sf_geometry = geom_point)
  dim <- stars::st_dimensions(stars_obj)

  expect_equal(sf::st_crs(dim$geometry$refsys)$proj4string,
               sf::st_crs(geom_point)$proj4string)
  expect_true(dim$geometry$point)
  expect_s3_class(dim$geometry$values, "sfc_POINT")

  test_list$alts <- numeric(0)

  stars_obj <- st_as_stars(test_list)

  expect_true("XY" %in% class(st_dimensions(stars_obj)$points$values[[1]]))
})

test_that("st_as_stars.Raster", {
  skip_if_not_installed("raster")
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  x = raster::raster(tif)
  stars_obj <- st_as_stars(x)
  expect_s3_class(stars_obj, "stars")
  expect_equal(names(stars_obj), names(x))

  # change the name of the attribute
  names(x) <- "newname"
  stars_obj2 <- st_as_stars(x)
  expect_equal(names(stars_obj2), names(x))
})
