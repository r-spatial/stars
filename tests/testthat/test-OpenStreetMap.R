test_that("st_as_stars.OpenStreetMap works", {
  skip_on_cran()
  skip_if_not_installed("OpenStreetMap")
  m = OpenStreetMap::openmap(c(52,7.3),c(52.1,7.15), type = "osm")
  s = st_as_stars(m)
  expect_s3_class(s, "stars")
})
