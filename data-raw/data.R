library(stars)

# single GeoTIFF:
L7_ETMs = read_stars(
	function() c(L7_ETMs = system.file("tif/L7_ETMs.tif", package = "stars"))
)
d = attr(L7_ETMs, "dimensions")
attr(L7_ETMs, "dimensions")$x$refsys$wkt = gsub("°", "", d$x$refsys$wkt)
attr(L7_ETMs, "dimensions")$y$refsys$wkt = gsub("°", "", d$x$refsys$wkt)
usethis::use_data(L7_ETMs, overwrite = TRUE)

# two attributes in a NetCDF file: use a list of functions giving the sub-datasets:
bcsd_obs = read_stars(
	list(
      function() c(foo = paste0("NETCDF:\"", system.file("nc/bcsd_obs_1999.nc", package = "stars"), "\":pr")),
      function() c(bar = paste0("NETCDF:\"", system.file("nc/bcsd_obs_1999.nc", package = "stars"), "\":tas"))
	)
)
usethis::use_data(bcsd_obs, overwrite = TRUE)

stars_sentinel2 = read_stars(
	function() {
			granule = system.file("sentinel/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.zip", package = "starsdata")
			if (granule == "")
				stop(paste("install starsdata (size: 1 Gb) first, with: ",
					'install.packages("starsdata", repos = "http://gis-bigdata.uni-muenster.de", type = "source")',
					collapse = '\n'))
			paste0("SENTINEL2_L1C:/vsizip/", granule, "/S2A_MSIL1C_20180220T105051_N0206_R051_T32ULE_20180221T134037.SAFE/MTD_MSIL1C.xml:10m:EPSG_32632")
	}
)
usethis::use_data(stars_sentinel2, overwrite = TRUE)
