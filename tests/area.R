library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
(a = st_area(x))

m = matrix(1:20, nrow = 5, ncol = 4)
x = c(0, 0.5, 1, 2, 4, 5)  # 6 numbers: boundaries!
y = c(0.3, 0.5, 1, 2, 2.2) # 5 numbers: boundaries!
(r = st_as_stars(list(m = m), dimensions = st_dimensions(x = x, y = y)))
# image(r, axes = TRUE, col = grey((1:20)/20))
(a = st_area(r))
plot(a, axes = TRUE)

(s5p = system.file("sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc", package = "starsdata"))
if (s5p != "") {
nit.c = read_stars(s5p, sub = "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column",
	curvilinear = c("//PRODUCT/longitude", "//PRODUCT/latitude"), driver = NULL)
nit.c[[1]][nit.c[[1]] > 9e+36] = NA
st_crs(nit.c) = 4326
print((a <- st_area(nit.c)))
plot(a, axes = TRUE, border = NA)
}
