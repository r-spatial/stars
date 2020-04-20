library(stars)
library(xts)
x.orig = read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))
x = x.orig
(x = st_set_dimensions(x, 3, Sys.Date() + 1:6))
(x = st_set_dimensions(x, 3, names = c("x", "y", "time")))
x = as.xts(x)
x[1:3,1:5]
y = st_as_stars(x) # has other dimensions collapsed
y = st_as_stars(x, dimensions = st_dimensions(x.orig)[c("band","x","y")]) 
setNames(y, "L7_ETMs.tif") %>%
	aperm(c(2,3,1)) -> yy
all.equal(yy, x.orig)

# xts -> equal-sized xts (reverse order)
f1 = function(x) {
	xts(as.matrix(x)[rev(seq_len(nrow(x))),], index(x))
}

# xts -> single number (compute time mean)
f2 = function(x) {
	apply(x, 2, mean)
}

# xts -> multiple numbers (return mean and variance)
f3 = function(x) {
	apply(x, 2, function(x) c(mean = mean(x), var = var(x)))
}

# xts -> monthly maxima
f4 = function(x, by = "months") {
	a = aggregate(x, cut(index(x), breaks = by), max)
	xts(as.matrix(a), as.POSIXct(index(a)))
}

# univariate examples:
x = xts(1:5, Sys.Date() + 1:5)
f1(x)
f2(x)
f3(x)
x = xts(rnorm(50), Sys.Date() + 1:50)
f4(x, "weeks")

# multivariate examples, only working on time:
x = xts(cbind(1:5,5:1,runif(5)), Sys.Date() + 1:5)
f1(x)
f2(x)
f3(x)
x = xts(cbind(rnorm(50), runif(50)), Sys.Date() + 1:50)
f4(x)

# running them on a 4-d cube:
# create 4-dimensional cube: x/y/band/time
c(x.orig, 2*x.orig, 3*x.orig, 4*x.orig, along = "time") %>%
  st_set_dimensions(4, Sys.Date()+1:4) -> d4
d4

# apply a time reducer to a 4d cube, either dropping time, or adding a new dimension
reduce_4d_time_xts = function(x, f, time = "time", ...) {
	out = f(as.xts(x), ...)
	t = which(names(dim(x)) == time)
	if (!is.null(dim(out))) {
		out = t(out)
		dim(out) = c(dim(x)[-t], new = prod(dim(out))/prod(dim(x)[-t]))
		st_as_stars(list(x = out)) # but this looses all the dimension metadata
	} else { # f() returns a vector, not a matrix
		dim(out) = dim(x)[-t]
		st_as_stars(list(x = out), dimensions = st_dimensions(x)[-t]) 
	}
}

reduce_4d_time_xts(d4, f2)
reduce_4d_time_xts(d4, f1)
reduce_4d_time_xts(d4, f3)
# reduce_4d_time_xts(d4[,1:20,1:20], f4, by = "days") -->> takes ages!!
