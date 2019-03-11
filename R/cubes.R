#' convert cube_view object to stars dimensions object
#' 
#' convert cube_view object to stars dimensions object
#' @param v object of class \code{cube_view} (package gdalcubes)
#' @param ... ignored
#' @details wronly interprets time, at the moment, and only for P1M (monthly)
#' @export
#' @examples
#' 
#' if(require(gdalcubes)) {
#'   example(cube_view)
#'   st_cube_view_to_dimensions(vnew)
#'   st_get_dimension_values(st_cube_view_to_dimensions(vnew), 1)
#'   st_get_dimension_values(st_cube_view_to_dimensions(vnew), 2)
#'   st_get_dimension_values(st_cube_view_to_dimensions(vnew), 3)
#' }
#' 
st_cube_view_to_dimensions = function(v, ...) {
	sp = v$space
	x = create_dimension(values = seq(sp$left, sp$right, length.out = sp$nx), refsys = sp$srs, point = FALSE) # might not work if srs is not proj4
	y = create_dimension(values = seq(sp$top, sp$bottom, length.out = sp$ny), refsys = sp$srs, point = FALSE) # might not work if srs is not proj4
	time = create_dimension(values=as.POSIXct(gdalcubes::dimension_values(v, "S")$t, tz = "GMT"), refsys = "POSIXct", point = FALSE) 
	create_dimensions(list(x = x, y = y, time = time))
}



#' Convert an existing cube object to a stars object
#' 
#' Convert an existing cube object to a stars object
#' @name st_as_stars
#' @param proxy logical; if TRUE, create a stars proxy object and delay call to gdalcubes::write_ncdf() (not yet implemented) 
#' @export
#' @examples
#' 
#' if(require(gdalcubes)) {
#'    L8_files <- list.files(system.file("L8NY18", package = "gdalcubes"), ".TIF", recursive = TRUE, full.names = TRUE)
#'    v = gdalcubes::cube_view(extent=list(left=388941.2, right=766552.4, 
#'                             bottom=4345299, top=4744931, t0="2018-01", t1="2018-12"),
#'                             srs="EPSG:32618", nx = 497, ny=526, dt="P1M")
#'    L8.col = gdalcubes::create_image_collection(L8_files, "L8_L1TP") 
#'    st_as_stars(gdalcubes::raster_cube(L8.col, v), proxy=FALSE)
#' }
st_as_stars.cube = function(.x, ..., proxy = TRUE) {
  if (!requireNamespace("gdalcubes", quietly = TRUE))
    stop("package gdalcubes required, please install it first") # nocov
  
  v = gdalcubes::cube_view(.x)
  outnc = tempfile(fileext = ".nc")
  subdatasets = paste0("NETCDF:\"", outnc, "\":", names(.x), sep="", collapse = NULL)
  if (proxy) {
    out = st_stars_proxy(list(subdatasets), st_cube_view_to_dimensions(v))
    create_ncdf = call("write_ncdf", .x, outnc) # assumption here is that the gdalcubes package namespace is loaded, this should be okay due to calling requireNamespace before.
    collect(out, create_ncdf, "write_ncdf") # this, at the moment, does not work with st_as_stars, because fetch tries to read the files before the call list is evaluated
  }
  else {
    gdalcubes::write_ncdf(.x, outnc)
    out = read_stars(subdatasets)
    out = st_set_dimensions(out, "x", point = FALSE)
    out = st_set_dimensions(out, "y", point = FALSE)
    out = st_set_dimensions(out, "time", point = FALSE, values=as.POSIXct(gdalcubes::dimension_values(.x, "S")$t, tz = "GMT"))
    return(out)
  }
}

