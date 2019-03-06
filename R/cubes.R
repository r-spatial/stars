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
	x = create_dimension(values = seq(sp$left, sp$right, length.out = sp$nx), refsys = sp$srs, point = FALSE)
	y = create_dimension(values = seq(sp$top, sp$bottom, length.out = sp$ny), refsys = sp$srs, point = FALSE)
	time = v$time
	t = if (time$dt == "P1M") {
		t0 = as.Date(paste0(time$t0, "-01"))
		t1 = as.Date(paste0(time$t1, "-01"))
		create_dimension(values = seq(from = t0, to = t1, length.out = time$nt), refsys = "Date", point = FALSE) # wrong!!!
	}
	create_dimensions(list(x = x, y = y, t = t))
}
