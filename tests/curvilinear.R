suppressPackageStartupMessages(library(stars))

s5p = system.file("sentinel5p/S5P_NRTI_L2__NO2____20180717T120113_20180717T120613_03932_01_010002_20180717T125231.nc", package = "starsdata")
if (s5p != "") {

lat_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/latitude")
lon_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/longitude")
nit_ds = paste0("HDF5:\"", s5p, "\"://PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column")
lat = read_stars(lat_ds)
lon = read_stars(lon_ds)
nit = read_stars(nit_ds)
nit[[1]][nit[[1]] > 9e+36] = NA

ll = setNames(c(lon, lat), c("x", "y"))
nit.c0 = st_as_stars(nit, curvilinear = ll)

# more direct method:
nit.c = read_stars(s5p, sub = "//PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/nitrogendioxide_summed_total_column",
	curvilinear = c("//PRODUCT/longitude", "//PRODUCT/latitude"), driver = NULL)
nit.c[[1]][nit.c[[1]] > 9e+36] = NA
all.equal(nit.c0, nit.c)
st_crs(nit.c) = 4326
print(nit.c)

if (capabilities()["png"]) {
 png("nit1.png", 800, 800)
 plot(nit.c, breaks = "equal", reset = FALSE, axes = TRUE, as_points = TRUE, pch = 16)
 maps::map('world', add = TRUE, col = 'red')
 dev.off()

 png("nit2.png", 800, 800)
 plot(nit.c, breaks = "equal", reset = FALSE, axes = TRUE, as_points = FALSE, border = NA)
 maps::map('world', add = TRUE, col = 'red')
 dev.off()

 nit.c = stars:::st_downsample(nit.c, 8)
 print(nit.c)
 
 png("nit3.png", 800, 800)
 plot(nit.c, breaks = "equal", reset = FALSE, axes = TRUE, as_points = TRUE, pch = 16)
 maps::map('world', add = TRUE, col = 'red')
 dev.off()

 png("nit4.png", 800, 800)
 plot(nit.c, breaks = "equal", reset = FALSE, axes = TRUE, as_points = FALSE, border = NA)
 maps::map('world', add = TRUE, col = 'red')
 dev.off()
}

}
