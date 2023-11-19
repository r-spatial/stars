toRad = function(x) x * pi / 180

toDeg = function(x) x * 180 / pi

rotate = function(lonlat, lon0, lat0, north = TRUE) {
	# https://gis.stackexchange.com/questions/10808/manually-transforming-rotated-lat-lon-to-regular-lat-lon/14445 , by Miha!
	lon0 = toRad(lon0)
	lat0 = toRad(lat0)
	if (north) {
		lat0 = -lat0
		lon0 = pi + lon0
	}
	lonlat = toRad(lonlat)
	lon = lonlat[,1]
	lat = lonlat[,2]
	vartheta = -(pi/2 + lat0)
	varphi = -lon0
	toDeg(cbind(
		atan2(sin(lon), tan(lat) * sin(vartheta) + 
				cos(lon) * cos(vartheta)) - varphi,
		asin(cos(vartheta) * sin(lat) - cos(lon) * sin(vartheta) * cos(lat))
	))
}

#' @export
st_rotate = function(.x, lon0, lat0, north, ...) UseMethod("st_rotate")


#' Transform rotated pole long/lat regular grid to unrotated curvilinear grid
#'
#' Transform rotated long/lat regular grid to unrotated curvilinear grid
#' @param .x object of class \code{stars}
#' @param lon0 longitude of the rotated pole in degrees
#' @param lat0 latitude of the rotated pole in degrees
#' @param north logical; if \code{TRUE} the pole refers to the North pole, otherwise the South pole
#' @param ... ignored
#' @returns curvilinear stars object with coordinates in regular long/lat (North pole at lat=90)
#' @export
#' @name st_rotate
#' @examples
#' if (require("starsdata") && require("maps")) {
#'   # data downloaded from https://esgf-data.dkrz.de/search/cosmo-rea/
#'   nc = "netcdf/ts_EUR-6km_ECMWF-ERAINT_REA6_r1i1p1f1_COSMO_v1_mon_201801-201812.nc"
#'   f = system.file(nc, package = "starsdata")
#'   m = read_mdim(f, "ts")
#'   print(m)
#'   # NOTE this function is obsolete when reading m as
#'   # m = read_mdim(f, "ts", curvilinear = c("longitude", "latitude"))
#'   if (require(RNetCDF)) {
#'      x = open.nc(f)
#'      lon = att.get.nc(x, "rotated_latitude_longitude", "grid_north_pole_longitude")
#'      lat = att.get.nc(x, "rotated_latitude_longitude", "grid_north_pole_latitude")
#'      close.nc(x)
#'      print(c(lon = lon, lat = lat))
#'   } else {
#'      lon = -162
#'      lat = 39.25
#'   } 
#'   m1 = st_rotate(m, lon, lat)
#'   print(m1)
#'   h = function() maps::map(add = TRUE)
#'   plot(m1, downsample = c(10, 10, 5), axes = TRUE, hook = h, mfrow = c(1, 2)) 
#'     # curvilinear grid: downsample for plotting speed
#'   m2 = st_warp(m1, crs = st_crs("OGC:CRS84"), threshold = .1)
#'   plot(m2, hook = h, mfrow = c(3, 4)) # regular grid: plots fast
#' }
st_rotate.stars = function(.x, lon0, lat0, north = TRUE, ...) {
	stopifnot(is.na(st_crs(.x)) || st_is_longlat(.x), 
			  !is_curvilinear(.x), 
			  is.logical(north), 
			  !is.na(north),
			  lat0 <= 90,
			  lat0 >= -90,
			  length(list(...)) == 0)
	if (has_sfc(.x))
		st_set_dimensions(.x, which_sfc(.x), values = rotate(st_geometry(.x), lon0, lat0, north))
	else {
		.x = st_upfront(.x)
		d = dim(.x)
		ccr = rotate(st_coordinates(st_dimensions(.x)[1:2]), lon0, lat0, north)
		st_as_stars(.x, curvilinear = setNames(list(matrix(ccr[,1], d[1], d[2]),
					  matrix(ccr[,2], d[1], d[2])), names(d)[1:2]))
	}
}

#' @name st_rotate
#' @export
st_rotate.sfc = function(.x, lon0, lat0, north = TRUE, ...) {
	r = rapply(.x, rotate, how = "replace", lon0 = lon0, lat0 = lat0, north = north)
	st_set_crs(r, NA_crs_)
}

#' @name st_rotate
#' @export
st_rotate.sf = function(.x, lon0, lat0, north = TRUE, ...) {
	st_set_geometry(.x, st_rotate(st_geometry(.x), lon0, lat0, north))
}

