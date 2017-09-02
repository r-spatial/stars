#' @importFrom sf sf_extSoftVersion
#' @importFrom Rcpp evalCpp
#' @importFrom graphics image.default
#' @useDynLib stars
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.stars_cache <- new.env(FALSE, parent=globalenv())

.onLoad = function(libname, pkgname) {
	if (file.exists(system.file("proj/nad.lst", package = "stars")[1])) {
		# nocov start
  		assign(".stars.PROJ_LIB", Sys.getenv("PROJ_LIB"), envir=.stars_cache)
		prj = system.file("proj", package = "stars")[1]
		Sys.setenv("PROJ_LIB" = prj)
		assign(".stars.GDAL_DATA", Sys.getenv("GDAL_DATA"), envir=.stars_cache)
		gdl = system.file("gdal", package = "stars")[1]
		Sys.setenv("GDAL_DATA" = gdl)
		# nocov end
	}
	CPL_gdal_init()
}

.onUnload = function(libname, pkgname) {
	CPL_gdal_cleanup_all()
	if (file.exists(system.file("proj/nad.lst", package = "stars")[1])) {
		# nocov start
		Sys.setenv("PROJ_LIB"=get(".stars.PROJ_LIB", envir=.stars_cache))
		Sys.setenv("GDAL_DATA"=get(".stars.GDAL_DATA", envir=.stars_cache))
		# nocov end
	}
}

.onAttach = function(libname, pkgname) {
	m = paste0("Linking to GDAL ", CPL_gdal_version(), ", proj.4 ", CPL_proj_version())
	packageStartupMessage(m)
}

#' Provide the external dependencies versions of the libraries linked to stars
#' 
#' Provide the external dependencies versions of the libraries linked to stars
#' @export
stars_extSoftVersion = function() {
	structure(c(CPL_gdal_version(), CPL_proj_version()),
		names = c("GDAL", "proj.4"))
}

.x = sf_extSoftVersion()
