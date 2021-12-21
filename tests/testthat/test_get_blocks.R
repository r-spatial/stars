mat = matrix(0L, 10, 10)

test1 = get_blocks(nrow(mat), ncol(mat), 10, 10)
test_that("one block", {
	expect_equal(nrow(test1), 1)
	expect_setequal(test1, c(1, 1, 10, 10))
	# output matrix should be integer to best performance
	expect_type(test1, "integer")
})

test2 = get_blocks(nrow(mat), ncol(mat), 5, 5)
test_that("equal blocks", {
	expect_equal(nrow(test2), 4)
})

test3 = get_blocks(nrow(mat), ncol(mat), 9, 5)
test_that("different size blocks", {
	expect_equal(nrow(test3), 4)
})

test4 = get_blocks(nrow(mat), ncol(mat), 1, 1)
test_that("many small blocks ", {
	expect_equal(nrow(test4), 100)
	expect_setequal(test4[, "nXOff"], rep(1:10, each = 10))
	expect_setequal(test4[, "nYOff"], rep(1:10, times = 10))
	expect_setequal(test4[, "nXSize"], rep(1, times = 100))
	expect_setequal(test4[, "nYSize"], rep(1, times = 100))
})
