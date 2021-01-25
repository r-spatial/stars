library(stars)

s1 = st_as_stars(matrix(1:16, 4))
s2 = st_as_stars(matrix(1:16, 4))
s3 = st_as_stars(matrix(1:16, 4))
attr(s1, "dimensions")$X2$delta = -1
attr(s2, "dimensions")$X1$delta =  2
attr(s2, "dimensions")$X2$delta = -2
attr(s3, "dimensions")$X1$delta =  3
attr(s3, "dimensions")$X2$delta = -3
plot(s1, axes = TRUE, text_values = TRUE, text_color = 'orange')
plot(s2, axes = TRUE, text_values = TRUE, text_color = 'orange')
plot(s3, axes = TRUE, text_values = TRUE, text_color = 'orange')

# chunk2 
fn1 = paste0(tempdir(), .Platform$file.sep, "img1.tif")
fn2 = paste0(tempdir(), .Platform$file.sep, "img2.tif")
fn3 = paste0(tempdir(), .Platform$file.sep, "img3.tif")
write_stars(s1, fn1)
write_stars(s2, fn2)
write_stars(s3, fn3)
gdal_utils("info", fn1)
gdal_utils("info", fn2)
gdal_utils("info", fn3)
(r1 = read_stars(c(fn1, fn2, fn3), proxy = TRUE))

# chunk3
st_as_stars(r1) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)

# chunk4
st_as_stars(r1[,2:4,2:4]) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)

# chunk5
s4 = st_as_stars(matrix(1: 16, 4))
s5 = st_as_stars(matrix(1: 64, 8))
s6 = st_as_stars(matrix(1:144,12))
attr(s4, "dimensions")$X2$delta = -1
attr(s5, "dimensions")$X1$delta =  1/2
attr(s5, "dimensions")$X2$delta = -1/2
attr(s6, "dimensions")$X1$delta =  1/3
attr(s6, "dimensions")$X2$delta = -1/3
plot(s4, axes = TRUE, text_values = TRUE, text_color = 'orange')
plot(s5, axes = TRUE, text_values = TRUE, text_color = 'orange')
plot(s6, axes = TRUE, text_values = TRUE, text_color = 'orange')

# chunk6
fn4 = paste0(tempdir(), .Platform$file.sep, "img4.tif")
fn5 = paste0(tempdir(), .Platform$file.sep, "img5.tif")
fn6 = paste0(tempdir(), .Platform$file.sep, "img6.tif")
write_stars(s4, fn4)
write_stars(s5, fn5)
write_stars(s6, fn6) 
(r2 = read_stars(c(fn4, fn5, fn6), proxy = TRUE))

st_as_stars(r2) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)
st_as_stars(r2[,2:4,2:4]) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)

# chunk7
(r3 = read_stars(c(fn6, fn5, fn4), proxy = TRUE))

st_as_stars(r3) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)
st_as_stars(r3[,2:6,3:6]) %>%
  merge() %>%
  plot(breaks = "equal", text_values = TRUE, text_color = 'orange', axes = TRUE)
