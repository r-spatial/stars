test_that("default geotransform works", {
  f <- file.path(R.home(), "Tcl/lib/tk8.6/images/logo100.gif")
  st <- read_stars(f)

  if (packageVersion("sf") >= "0.9-0") {
    expect_true(all(!is.na(unlist(unclass(st_dimensions(st)$x)[c("offset", "delta")]))))

  } else {
    expect_equal(unlist(unclass(st_dimensions(st)$x)[c("offset", "delta")]),
                 c(offset = NA_real_, delta = NA_real_))

  }
})
