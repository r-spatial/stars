#' @importFrom graphics image.default image par plot title box text axis plot.new plot.window rasterImage layout lcm
#' @importFrom grDevices dev.capabilities dev.size grey
#' @importFrom utils tail setTxtProgressBar txtProgressBar
#' @importFrom stats na.omit runif aggregate
#' @importFrom tools file_ext
#' @importFrom methods as slotNames new slot
#' @importFrom abind abind adrop asub
#' @importFrom classInt classIntervals
#' @importFrom parallel parApply
#' @import sf
#' @import units
NULL

setOldClass("stars")

.onLoad = function(libname, pkgname) {
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
}
