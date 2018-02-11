#' @importFrom graphics image.default image par plot title box text axis plot.new plot.window rasterImage
#' @importFrom grDevices dev.capabilities dev.size grey
#' @importFrom utils tail
#' @importFrom stats na.omit
#' @importFrom methods as slotNames new slot
#' @importFrom abind abind adrop
#' @importFrom classInt classIntervals
#' @importFrom Rcpp evalCpp
#' @import dplyr
#' @importFrom magrittr %>%
#' @import sf
#' @import units
NULL

setOldClass("stars")

.onLoad = function(libname, pkgname) {
}

.onUnload = function(libname, pkgname) {
}

.onAttach = function(libname, pkgname) {
}
