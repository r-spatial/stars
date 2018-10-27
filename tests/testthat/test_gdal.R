library(testthat)
library(stars)
context("gdal utils")


test_that('gdal_utils work', {
  skip_on_appveyor()

  fname = system.file("nc/tos_O1_2001-2002.nc", package = "stars")
  info = gdal_utils("info", fname, quiet = TRUE)
  sd2 = gdal_subdatasets(fname)[[4]]
  info = gdal_utils("info", sd2, quiet = TRUE)
  tf = tempfile()
  tf2 = tempfile()
  tf3 = tempfile()
  #tf = "foo"
  #gdal_utils("rasterize", points, tif) -> need a good example
  expect_true(gdal_utils("warp", sd2, tf, c("-t_srs", "+proj=utm +zone=11 +datum=WGS84")))
#  expect_true(gdal_utils("rasterize", sd2, tf)) # breaks on fedora-clang
  expect_true(gdal_utils("translate", sd2, tf))
  expect_true(gdal_utils("vectortranslate", sd2, tf2))
  expect_warning(gdal_utils("nearblack", sd2, tf))
  # create point geom:
  points = system.file("gpkg/nc.gpkg", package="sf")
  expect_true(gdal_utils("grid", points, tf))
  expect_true(gdal_utils("buildvrt", sd2, tf3))
#  expect_true(gdal_utils("demprocessing", sd2, tf, processing = "hillshade")) # breaks on fedora-clang
})

# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' -overwrite NETCDF:avhrr-only-v2.19810901.nc:anom utm11.tif
# becomes:
# gdal_utils("warp" "NETCDF:avhrr-only-v2.19810901.nc:anom", "utm11.tif", c("-t_srs", "+proj=utm +zone=11 +datum=WGS84"))
