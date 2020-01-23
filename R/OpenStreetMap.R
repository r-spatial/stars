
#' @name st_as_stars
#' @export
st_as_stars.OpenStreetMap = function(.x, ...) {

  ncols = .x$tiles[[1]]$yres # not sure why OpenStreetMap has swapped y and x...
  nrows = .x$tiles[[1]]$xres
  
  rgb_values = grDevices::col2rgb(.x$tiles[[1]]$colorData)
  a = array(t(rgb_values), dim = c(x = ncols, y = nrows, band = 3))
  
  bbx <- unname(unlist(.x$bbox))[c(1,3,4,2)]
  names(bbx) <- c("xmin", "xmax", "ymin", "ymax")
  
  crs = st_crs(.x$tiles[[1]]$projection)
  
  dimensions = list(
    x = create_dimension(from = 1, to = ncols, offset = unname(bbx["xmin"]),
                         delta = unname((bbx["xmax"] - bbx["xmin"]) / ncols), refsys = crs$proj4string),
    y = create_dimension(from = 1, to = nrows, offset = unname(bbx["ymax"]),
                         delta = unname((bbx["ymin"] - bbx["ymax"]) / nrows), refsys = crs$proj4string),
    band = create_dimension(values = c("red", "green", "blue")))
    
  adrop(st_as_stars(a, dimensions = create_dimensions(dimensions, get_raster())))
}
