library(testthat)
library(stars)
context("st_apply")


test_that('st_apply retains units', {
		  X<-st_as_stars(set_units(array(1:27, c(3,3,3)),'m'))
		  expect_equal(units(X[[1]]), units(st_apply(X, 1:2, mean)[[1]]))
		  expect_equal(units(X[[1]]), units(st_apply(X, 3, max)[[1]]))
		  expect_equal(units(log(X[[1]])), units(st_apply(X, 3, function(x) log(sum(x)))[[1]]))
		  X[['u']]<-array(1:27, c(3,3,3))
		  expect_equal(units(X[[1]]), units(st_apply(X, 1:2, mean)[[1]]))
		  expect_equal(units(X[[1]]), units(st_apply(X, 3, max)[[1]]))
		  expect_equal(inherits(X[[2]], "units"), inherits(st_apply(X, 1:2, mean)[[2]],'units'))
		  expect_equal(inherits(X[[2]], "units"), inherits(st_apply(X, 3, max)[[2]],'units'))
		  expect_false(inherits(st_apply(X, 1:2, function(x) any(is.na(x)))[[1]],'units'))
		  expect_false(inherits(st_apply(X, 1:2, function(x) any(is.na(x)))[[2]],'units'))
})
