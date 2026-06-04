context("aggregate.stars with weights and transform")

make_test_data = function() {
	skip_if_not_installed("exactextractr")
	skip_if_not_installed("terra")

	tif = system.file("tif/L7_ETMs.tif", package = "stars")
	x = read_stars(tif)[, 1:30, 1:30, 1]

	bb = sf::st_bbox(x)
	midx = mean(c(bb["xmin"], bb["xmax"]))
	midy = mean(c(bb["ymin"], bb["ymax"]))
	p1 = sf::st_polygon(list(rbind(
		c(bb["xmin"], bb["ymin"]), c(midx, bb["ymin"]),
		c(midx, midy), c(bb["xmin"], midy),
		c(bb["xmin"], bb["ymin"]))))
	p2 = sf::st_polygon(list(rbind(
		c(midx, midy), c(bb["xmax"], midy),
		c(bb["xmax"], bb["ymax"]), c(midx, bb["ymax"]),
		c(midx, midy))))
	polys = sf::st_sfc(p1, p2, crs = sf::st_crs(x))

	list(x = x, polys = polys)
}

test_that("exact = TRUE without weights/transform is backwards compatible", {
	d = make_test_data()

	a_mean = aggregate(d$x, d$polys, mean, exact = TRUE)
	a_sum = aggregate(d$x, d$polys, sum, exact = TRUE)

	expect_s3_class(a_mean, "stars")
	expect_s3_class(a_sum, "stars")
	expect_true(all(is.finite(c(a_mean[[1]]))))
	expect_true(all(is.finite(c(a_sum[[1]]))))
})

test_that("transform one-to-one captures Jensen's inequality", {
	d = make_test_data()

	a_linear = aggregate(d$x, d$polys, mean, exact = TRUE)
	a_squared = aggregate(d$x, d$polys, mean, exact = TRUE, transform = ~ .x^2)

	expect_true(all(c(a_squared[[1]]) >= c(a_linear[[1]])^2 - 1e-6))
	expect_true(any(c(a_squared[[1]]) > c(a_linear[[1]])^2 + 1))
})

test_that("transform accepts function and formula equivalently", {
	d = make_test_data()

	a_form = aggregate(d$x, d$polys, mean, exact = TRUE, transform = ~ .x^2)
	a_func = aggregate(d$x, d$polys, mean, exact = TRUE, transform = function(z) z^2)

	expect_equal(c(a_form[[1]]), c(a_func[[1]]))
})

test_that("one-to-many transform appends a term dimension with given names", {
	d = make_test_data()

	a = aggregate(d$x, d$polys, mean, exact = TRUE,
		transform = ~ cbind(lin = .x, sq = .x^2, cu = .x^3))

	expect_s3_class(a, "stars")
	expect_true("term" %in% names(st_dimensions(a)))
	expect_equal(dim(a)[["term"]], 3L)
	expect_equal(st_get_dimension_values(a, "term"), c("lin", "sq", "cu"))
})

test_that("one-to-many transform falls back to t1, t2, ... when colnames are NULL", {
	d = make_test_data()

	a = aggregate(d$x, d$polys, mean, exact = TRUE,
		transform = ~ cbind(.x, .x^2, .x^3))

	expect_equal(st_get_dimension_values(a, "term"), c("t1", "t2", "t3"))
})

test_that("one-to-many transform with partial colnames defaults all terms to t1, t2, ...", {
	d = make_test_data()

	# a single unnamed column makes every term fall back to t1..tk, not just the unnamed one
	a = aggregate(d$x, d$polys, mean, exact = TRUE,
		transform = ~ cbind(a = .x, .x^2))

	expect_equal(st_get_dimension_values(a, "term"), c("t1", "t2"))
})

test_that("weights produces a different result from unweighted", {
	d = make_test_data()

	w_raster = methods::as(d$x, "SpatRaster")
	terra::values(w_raster) = seq_len(terra::ncell(w_raster))

	a_unweighted = aggregate(d$x, d$polys, mean, exact = TRUE)
	a_weighted = aggregate(d$x, d$polys, mean, exact = TRUE, weights = w_raster)

	expect_false(isTRUE(all.equal(c(a_unweighted[[1]]), c(a_weighted[[1]]))))
})

test_that("weighted mean matches hand calculation", {
	skip_if_not_installed("exactextractr")
	skip_if_not_installed("terra")

	tif = system.file("tif/L7_ETMs.tif", package = "stars")
	x = read_stars(tif)[, 1:4, 1:4, 1]
	x[[1]][] = as.numeric(1:16)

	bb = sf::st_bbox(x)
	pad = (bb["xmax"] - bb["xmin"]) / 100
	p = sf::st_polygon(list(rbind(
		c(bb["xmin"] - pad, bb["ymin"] - pad),
		c(bb["xmax"] + pad, bb["ymin"] - pad),
		c(bb["xmax"] + pad, bb["ymax"] + pad),
		c(bb["xmin"] - pad, bb["ymax"] + pad),
		c(bb["xmin"] - pad, bb["ymin"] - pad))))
	polys = sf::st_sfc(p, crs = sf::st_crs(x))

	w_raster = methods::as(x, "SpatRaster")

	a = aggregate(x, polys, mean, exact = TRUE, weights = w_raster)

	# polygon strictly encloses every cell, so coverage_fraction = 1 throughout;
	# weights = data per cell, so weighted_mean = sum(data^2) / sum(data) = 1496 / 136 = 11
	expect_equal(as.numeric(a[[1]]), sum((1:16)^2) / sum(1:16), tolerance = 1e-9)
})

test_that("all-zero weights raise an error rather than silently returning NaN", {
	skip_if_not_installed("exactextractr")
	skip_if_not_installed("terra")

	d = make_test_data()
	w0 = methods::as(d$x, "SpatRaster")
	terra::values(w0) = 0

	expect_error(
		aggregate(d$x, d$polys, mean, exact = TRUE, weights = w0),
		"all zero")
})

test_that("weights warns when exact = FALSE", {
	d = make_test_data()
	w_raster = methods::as(d$x, "SpatRaster")

	expect_warning(
		aggregate(d$x, d$polys, mean, weights = w_raster),
		"is ignored when")
})

test_that("transform applies in the non-exact path as well", {
	d = make_test_data()

	a = aggregate(d$x, d$polys, mean, exact = FALSE, transform = ~ .x^2)

	expect_s3_class(a, "stars")
})
