#' @importFrom graphics image.default image par plot title box text axis plot.new plot.window rasterImage layout lcm
#' @importFrom grDevices dev.capabilities dev.size grey rgb
#' @importFrom utils head tail setTxtProgressBar txtProgressBar
#' @importFrom stats na.omit runif aggregate setNames predict
#' @importFrom tools file_ext
#' @importFrom methods as slotNames new slot
#' @importFrom abind abind adrop asub
#' @importFrom classInt classIntervals
#' @importFrom parallel parApply
#' @importFrom rlang %||%
#' @import sf
#' @import units
NULL

# re-export:
#' @importFrom lwgeom st_transform_proj
#' @export
lwgeom::st_transform_proj

setOldClass("stars")

.onLoad = function(libname, pkgname) {
	register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
}
