context("rectilinear matrices")

m <- matrix(1:12, nrow = 4, ncol = 3)

x = c(0, 1, 2, 3)
y = c(2, 1, 0)

r_cell_edge = st_as_stars(list(m = m),
                dimensions = st_dimensions(x = x, y = y,
                                           .raster = c("x", "y"),
                                           cell_midpoints = FALSE))
sf::st_crs(r_cell_edge) <- sf::st_crs(5070)
r_ce_dim <- st_dimensions(r_cell_edge)

r_cell_mid = st_as_stars(list(m = m),
                dimensions = st_dimensions(x = x, y = y,
                                           .raster = c("x", "y"),
                                           cell_midpoints = TRUE))
sf::st_crs(r_cell_mid) <- sf::st_crs(5070)
r_cm_dim <- st_dimensions(r_cell_mid)

x = c(0, 1, 2, 3, 4)
y = c(2, 1, 0, -1)

r_cell_edges = st_as_stars(list(m = m),
                dimensions = st_dimensions(x = x, y = y,
                                           .raster = c("x", "y"),
                                           cell_midpoints = FALSE))
sf::st_crs(r_cell_edges) <- sf::st_crs(5070)
r_ces_dim <- st_dimensions(r_cell_edges)

r_cell_mids = st_as_stars(list(m = m),
                dimensions = st_dimensions(x = x, y = y,
                                           .raster = c("x", "y"),
                                           cell_midpoints = TRUE))
sf::st_crs(r_cell_mids) <- sf::st_crs(5070)
r_cms_dim <- st_dimensions(r_cell_edges)

test_that("same number of values as rows/cols", {

  expect_s3_class(r_cell_edge, "stars")

  expect_equal(names(dim(r_cell_edge)), names(r_ce_dim))

  expect_equal(names(r_ce_dim), c("x", "y"))

  #expect_true(st_crs(r_ce_dim) == sf::st_crs(5070))

  expect_equal(r_ce_dim$x$from, 1)
  expect_equal(r_ce_dim$x$to, 4)
  expect_equal(r_ce_dim$x$offset, 0)
  expect_equal(r_ce_dim$x$delta, 1)

  expect_equal(r_ce_dim$y$from, 1)
  expect_equal(r_ce_dim$y$to, 3)
  expect_equal(r_ce_dim$y$offset, 2)
  expect_equal(r_ce_dim$y$delta, -1)

  expect_s3_class(r_cell_mid, "stars")

  expect_equal(names(dim(r_cell_mid)), names(r_cm_dim))

  expect_equal(names(r_cm_dim), c("x", "y"))

  #expect_true(st_crs(r_cm_dim) == sf::st_crs(5070))

  expect_equal(r_cm_dim$x$from, 1)
  expect_equal(r_cm_dim$x$to, 4)
  expect_equal(r_cm_dim$x$offset, -0.5)
  expect_equal(r_cm_dim$x$delta, 1)

  expect_equal(r_cm_dim$y$from, 1)
  expect_equal(r_cm_dim$y$to, 3)
  expect_equal(r_cm_dim$y$offset, 2.5)
  expect_equal(r_cm_dim$y$delta, -1)
})

test_that("one more cell coordinate than matrix cells", {

  expect_s3_class(r_cell_edges, "stars")

  expect_equal(r_ces_dim, r_ce_dim)

  # Make sure cell_midpoint argument is ignored here.
  expect_equal(r_cms_dim, r_ces_dim)
})

test_that("st_as_sf works for these", {
  skip_on_cran() # too costly?
  sf_cell_edge <- sf::st_as_sf(r_cell_edge)

  expect_s3_class(sf_cell_edge, "sf")
  expect_s3_class(sf_cell_edge$geometry, "sfc_POLYGON")

  sf_cell_mid <- sf::st_as_sf(r_cell_mid)

  expect_s3_class(sf_cell_mid, "sf")
  expect_s3_class(sf_cell_mid$geometry, "sfc_POLYGON")

  sf_cell_edges <- sf::st_as_sf(r_cell_edges)

  expect_equal(sf_cell_edge, sf_cell_edges)

  sf_cell_mids <- sf::st_as_sf(r_cell_mids)

  expect_equal(sf_cell_mid, sf_cell_mids)
})

