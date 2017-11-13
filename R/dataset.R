#' get metadata of a raster layer
#'
#' get metadata of a raster layer
#' @name get_get_metadata
#' @export
#' @param file file name
#' @param domain_item character vector of length 0, 1 (with domain), or 2 (with domain and item); use \code{""} for the default domain, use \code{NA_character_} to query the domain names.
#' @param options character; character vector with data open options
#' @param parse logical; should metadata be parsed into a named list (\code{TRUE}) or returned as character data?
#' @return named list with metadata items
#' @examples
#' #f = system.file("tif/L7_ETMs.tif", package="stars")
#' f = system.file("nc/avhrr-only-v2.19810901.nc", package = "stars")
#' gdal_metadata(f)
#' gdal_metadata(f, NA_character_)
#' # try(gdal_metadata(f, "wrongDomain"))
#' # gdal_metadata(f, c("", "AREA_OR_POINT"))
gdal_metadata = function(file, domain_item = character(0), options = character(0), parse = TRUE) {
	stopifnot(is.character(file))
	stopifnot(is.character(domain_item))
	stopifnot(length(domain_item) <= 2)
	stopifnot(is.character(options))
	if (length(domain_item) >= 1 && !is.na(domain_item[1]) &&
			!(domain_item[1] %in% CPL_get_metadata(file, NA_character_, options)))
		stop("domain_item[1] not found in available metadata domains")
	p = CPL_get_metadata(file, domain_item, options)
	if (!is.na(domain_item[1]) && parse)
		split_strings(p)
	else
		p
}

split_strings = function(md, split = "=") {
	splt = strsplit(md, split)
	lst = lapply(splt, function(x) if (length(x) <= 1) NA_character_ else x[[2]])
	structure(lst, names = sapply(splt, function(x) x[[1]]))
	structure(lst, class = "gdal_metadata")
}

#' @name get_get_metadata
#' @param name logical; retrieve name of subdataset? If \code{FALSE}, retrieve description
#' @export
#' @return \code{gdal_subdatasets} returns a zero-length list if \code{file} does not have subdatasets, and else a named list with subdatasets.
gdal_subdatasets = function(file, options = character(0), name = TRUE) {
	if (!("SUBDATASETS" %in% CPL_get_metadata(file, NA_character_, options)))
		list()
	else {
		md = gdal_metadata(file, "SUBDATASETS", options, TRUE)
		if (name)
			md[seq(1, length(md), by = 2)]
		else
			md[seq(2, length(md), by = 2)]
	}
}

#' read coordinate reference system from GDAL data set
#' @param file character; file name
#' @param options character; raster layer read options
#' @return object of class \code{crs}, see \link[sf]{st_crs}.
#' @export
gdal_crs = function(file, options = character(0)) {
	ret = CPL_get_crs(file, options)
	ret$crs = sf::st_crs(wkt = ret$crs)
	ret
}
