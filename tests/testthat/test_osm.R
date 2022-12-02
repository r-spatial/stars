context("st_as_stars.OpenStreetMap")

test_that("st_as_stars.OpenStreetMap works", {
  skip_on_cran()
  skip_if_not(require(OpenStreetMap))
  m = openmap(c(52,7.3),c(52.1,7.15), type = "osm")
  s = st_as_stars(m)
  expect_true(inherits(s, "stars"))
})
