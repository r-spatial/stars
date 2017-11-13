library(testthat)
library(stars)
context("gdal utils")


test_that('st_gdal_utils work', {
  skip_on_appveyor()

  fname = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
  info = st_gdal_utils("info", fname, quiet = TRUE)
  sd2 = st_get_subdatasets(fname)[[2]]
  info = st_gdal_utils("info", sd2, quiet = TRUE)
  tf = tempfile()
  tf2 = tempfile()
  tf3 = tempfile()
  #tf = "foo"
  #st_gdal_utils("rasterize", points, tif) -> need a good example
  #st_gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
  #expect_true(st_gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84")))
  #st_gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
  #expect_true(st_gdal_utils("warp", sd2, tf))
  #expect_true(st_gdal_utils("rasterize", sd2, tf))
  expect_true(st_gdal_utils("translate", sd2, tf))
  expect_true(st_gdal_utils("vectortranslate", sd2, tf2))
  expect_warning(st_gdal_utils("nearblack", sd2, tf))
  # create point geom:
  points = system.file("gpkg/nc.gpkg", package="sf")
  expect_true(st_gdal_utils("grid", points, tf))
  expect_true(st_gdal_utils("buildvrt", sd2, tf3))
  expect_true(st_gdal_utils("demprocessing", sd2, tf, processing = "hillshade"))
})

# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite NETCDF:avhrr-only-v2.19810901.nc:anom utm11.tif
# becomes:
# st_gdalwarp("NETCDF:avhrr-only-v2.19810901.nc:anom", "utm11.tif", c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
