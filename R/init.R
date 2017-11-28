#' @importFrom graphics image.default image par plot title
#' @importFrom utils tail
#' @importFrom methods as slotNames new slot
#' @importFrom abind abind adrop
#' @importFrom Rcpp evalCpp
#' @import dplyr
#' @importFrom magrittr %>%
#' @import sf
#' @import units
#' @useDynLib stars
NULL


setOldClass("stars")

.stars_cache <- new.env(FALSE, parent=globalenv())

.onLoad = function(libname, pkgname) {
	CPL_gdal_init()
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
}

.onAttach = function(libname, pkgname) {
	packageStartupMessage(paste0("Linking to GDAL ", CPL_gdal_version()))
}

#' Provide the external dependencies versions of the libraries linked to stars
#' 
#' Provide the external dependencies versions of the libraries linked to stars
#' @export
stars_extSoftVersion = function() {
	structure(CPL_gdal_version(), names = "GDAL")
}

.x = sf_extSoftVersion()
