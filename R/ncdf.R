#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom ncmeta nc_meta
#' @importFrom stats setNames
NULL

#' read ncdf file into stars object
#' 
#' read ncdf file into stars object
#' @examples 
#' f <- system.file("nc/reduced.nc", package = "stars")
#' read_ncdf(f)
#' read_ncdf(f, var = c("anom"))
#' read_ncdf(f, ncsub = cbind(start = c(1, 1, 1, 1), count = c(10, 12, 1, 1)))
#' 
#' 
#' @param .x NetCDF file or source
#' @param ... ignored
#' @param var variable name or names (they must be on matching grids)
#' @param ncsub matrix of start, count columns 
#' 
#' @details
#' If `var` is not set the first set of variables on a shared grid is used.
#' It's supposed to be the grid with the most dimensions, but there's no control
#' yet (see `ncmeta::nc_grids(.x)` for the current assumption). 
#' 
#' \code{start} and \code{count} columns of ncsub must correspond to the variable dimemsion (nrows)
#' and be valid index using `ncdf4::ncvar_get` convention (start is 1-based). 
#' @export
read_ncdf = function(.x, ..., var = NULL, ncsub = NULL) {
  meta = ncmeta::nc_meta(.x)
  if (is.null(var)) {
    var = meta$grid$variable[meta$grid$grid[1] == meta$grid$grid]
  }
  ## 
  dims_index = meta$axis$dimension[meta$axis$variable == var[1L]]
  dims = meta$dimension[match(dims_index, meta$dimension$id), ]
  
  ## need to validate existing ncsub here
  if (is.null(ncsub)) {
    ncsub = cbind(start = 1, count = dims$length)
    rownames(ncsub) = dims$name
  } else {
    ## needs more attention
    if (nrow(dims) != nrow(ncsub)) stop("input ncsub doesn't match available dims")
    if (any(ncsub[, "start"] < 1) || any((ncsub[, "count"] - ncsub[, "start"] + 1) > dims$length)) stop("start or count out of bounds") 
  }
  nc = ncdf4::nc_open(.x, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  out = lapply(var, function(.v) ncdf4::ncvar_get(nc, 
                                                   varid = .v,
                                                   start = ncsub[, "start", drop = TRUE], 
                                                   count = ncsub[, "count", drop = TRUE], 
                                                  collapse_degen = FALSE))
  out = setNames(out, var)
  dimensions = create_dimensions(dim(out[[1]]))
  ## assume we have coord dims for now
  coords = lapply(dims$name, function(.n) ncdf4::ncvar_get(nc, varid = .n))
  for (i in seq_along(coords)) {
    dimensions[[i]]$offset[1L] = coords[[i]][ncsub[i, "start"]]
    ## NaN for singleton dims, but that seems ok unless we have explicit interval?
    dimensions[[i]]$delta[1L]  = mean(diff(coords[[i]]))  ## not rectilinear yet
  }
  st_stars(out, dimensions)
}
