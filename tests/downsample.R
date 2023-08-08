library(stars)
m = matrix(1:121, 11, 11)
m
s = st_as_stars(m)
st_downsample(s, 1)
st_downsample(s, 1)[[1]]
st_downsample(s, 1, offset = 1)
st_downsample(s, 1, offset = 1)[[1]]
st_downsample(s, 1, FUN = mean)
st_downsample(s, 1, FUN = mean)[[1]]
st_downsample(s, 1, offset = 1, FUN = mean)
st_downsample(s, 1, offset = c(0,1), FUN = mean)[[1]]
st_downsample(s, 1, offset = c(0,1))
st_downsample(s, 1, offset = c(0,1))[[1]]
