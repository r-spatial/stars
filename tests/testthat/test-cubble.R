test_that("cubble", {
  skip_if_not_installed("cubble")
  skip_if_not_installed("dplyr")
  library(dplyr)
  library(cubble)
  skip_if(packageVersion("cubble") == "0.3.1") # https://github.com/huizezhang-sherry/cubble/issues/30
  library(units)
  tm = set_units(1:6, "days since 1985-01-01")
  # l7 = st_as_stars(L7_ETMs) |> st_set_dimensions(3, values = Sys.time() + 1:6)
  l7 = st_as_stars(L7_ETMs) |> st_set_dimensions(3, values = tm)

  l7sub = l7[,1:30,1:30]
  cu = as_cubble(l7sub)
  s2 = st_as_stars(cu)
  # expect_equal(l7sub, s2)
  expect_true(all.equal(l7sub, s2, check.attributes=FALSE))

  prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
  #(prec = read_stars(gdal_subdatasets(prec_file)[[1]]))
  (prec = read_ncdf(prec_file))
  sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
    st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
  nc_outline = st_union(st_geometry(nc))
  plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)
  prec %>%
    slice(index = 1:12, along = "time") %>%
    plot(downsample = c(3, 3, 1), hook = plot_hook)
  a = aggregate(prec, by = nc, FUN = max)
  a.cb = as_cubble(a, key = id, index = time)
  a2 = st_as_stars(a.cb)
  # expect_equal(a, a2)
  expect_true(all.equal(drop_units(a),a2,check.attributes=FALSE))
})

