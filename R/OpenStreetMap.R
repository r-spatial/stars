#' @name st_as_stars
#' @param as_col logical; return rgb numbers (FALSE) or (character) color values (TRUE)?
#' @export
st_as_stars.OpenStreetMap = function(.x, ..., as_col = FALSE) {

	ncols = .x$tiles[[1]]$yres # not sure why OpenStreetMap has swapped y and x...
	nrows = .x$tiles[[1]]$xres
  
	a = if (as_col)
			array(.x$tiles[[1]]$colorData, dim = c(x = ncols, y = nrows))
		else {
			rgb_values = grDevices::col2rgb(.x$tiles[[1]]$colorData)
			array(t(rgb_values), dim = c(x = ncols, y = nrows, band = 3))
		}
	
	bbx <- unname(unlist(.x$bbox))[c(1,3,4,2)]
	names(bbx) <- c("xmin", "xmax", "ymin", "ymax")
  
	crs = st_crs(.x$tiles[[1]]$projection)
  
	dimensions = list(
	x = create_dimension(from = 1, to = ncols, offset = unname(bbx["xmin"]),
		delta = unname((bbx["xmax"] - bbx["xmin"]) / ncols), refsys = crs),
	y = create_dimension(from = 1, to = nrows, offset = unname(bbx["ymax"]),
		delta = unname((bbx["ymin"] - bbx["ymax"]) / nrows), refsys = crs),
	band = create_dimension(values = c("red", "green", "blue")))
    
	if (as_col)
		st_as_stars(a, dimensions = create_dimensions(dimensions[1:2], get_raster()))
	else
		st_as_stars(a, dimensions = create_dimensions(dimensions, get_raster()))
}
