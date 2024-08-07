#' @importFrom graphics image.default image par plot title box text axis plot.new plot.window rasterImage layout lcm contour hist strwidth
#' @importFrom grDevices dev.capabilities dev.size grey rgb col2rgb cm
#' @importFrom utils head tail setTxtProgressBar txtProgressBar packageVersion methods modifyList
#' @importFrom stats na.omit runif aggregate setNames predict quantile var complete.cases na.pass time prcomp
#' @importFrom tools file_ext
#' @importFrom methods as slotNames new slot
#' @importFrom abind abind adrop asub
#' @importFrom classInt classIntervals
#' @importFrom parallel parApply
#' @importFrom rlang %||%
#' @import sf
#' @import units
NULL

setOldClass("stars")
setOldClass("stars_proxy")

.onLoad = function(libname, pkgname) {
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
}

.onAttach = function(libname, pkgname) {
	if (nrow(sf::st_drivers("raster", "netCDF")) != 1)
		packageStartupMessage("Note: sf is linked to a GDAL version without netCDF driver, some tests or examples may fail")
}

