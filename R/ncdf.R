#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom ncmeta nc_meta
#' @importFrom stats setNames
NULL



.is_regular <- function(coords_list) {
  unlist(lapply(coords_list, function(x) regular_intervals(x)))
}


#' read ncdf file into stars object
#' 
#' Read data from a file using the NetCDF library directly. 
#' 
#' The following logic is applied to coordinate axes. If any coordinate axes have 
#' regularly spaced coordinates they are reduced to the
#' offset/delta form with 'affine = c(0, 0)', otherwise the values of the coordinates
#' are stored.   If the data has two or more dimensions and the first two are regular
#' they are nominated as the 'raster' for plotting. 
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
#' @param curvilinear length two character vector with names of subdatasets holding longitude and latitude values for all raster cells.
#' @details
#' If `var` is not set the first set of variables on a shared grid is used.
#' It's supposed to be the grid with the most dimensions, but there's no control
#' yet (see `ncmeta::nc_grids(.x)` for the current assumption). 
#' 
#' \code{start} and \code{count} columns of ncsub must correspond to the variable dimemsion (nrows)
#' and be valid index using `ncdf4::ncvar_get` convention (start is 1-based). 
#' @export
read_ncdf = function(.x, ..., var = NULL, ncsub = NULL, curvilinear = character(0)) {
  meta = ncmeta::nc_meta(.x)
  # Don't want scalar
  # todo handle choice of grid
  nas <- is.na(meta$axis$dimension)
  if (any(nas)) meta$axis$dimension[nas] <- -1
  if (is.null(var)) {
    ix <- 1
    if (meta$grid$grid[ix] == "S") {
      ix <- which(!meta$grid$grid == "S")[1L]
      
      if (length(ix) < 1)  stop("only scalar variables found, not yet supported")
    }
    var = meta$grid$variable[meta$grid$grid[ix] == meta$grid$grid]
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
    if (nrow(dims) != nrow(ncsub)) stop("input ncsub doesn't match available dims") # nocov
    if (any(ncsub[, "start"] < 1) || any((ncsub[, "count"] - ncsub[, "start"] + 1) > dims$length)) stop("start or count out of bounds")  # nocov
  }
  
  nc = ncdf4::nc_open(.x, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  out = lapply(var, function(.v) ncdf4::ncvar_get(nc, 
                                                   varid = .v,
                                                   start = ncsub[, "start", drop = TRUE], 
                                                   count = ncsub[, "count", drop = TRUE], 
                                                  collapse_degen = FALSE))
  out = setNames(out, var)
  ## cannot assume we have coord dims
  ## - so create them as 1:length if needed
  coords = setNames(vector("list", length(dims$name)), dims$name)
  for (ic in seq_along(coords)) {
    if (!is.null(ncsub)) {
      subidx <- seq(ncsub[ic, "start"], length = ncsub[ic, "count"])
    } else {
      subidx <- seq_len(length(coords[[ic]]))
    }
    ## create_dimvar means we can var_get it
    if (nc$dim[[dims$name[ic]]]$create_dimvar) {
      coords[[ic]] <- ncdf4::ncvar_get(nc, varid = dims$name[ic])[subidx]
     
    } else {
      coords[[ic]] <- subidx##seq_len(dims$length[ic])
    }
  }

  ## can we create a raster?
  raster = NULL
  ## which coords are regular
  regular = .is_regular(coords)
  if (length(coords) > 1) {
   # if (all(regular[1:2])) {
    raster = get_raster(affine = c(0, 0), 
                      dimensions = names(coords)[1:2], curvilinear = FALSE)
    #}
    
  }
  dimensions = create_dimensions(setNames(dim(out[[1]]), dims$name), raster)
  
  ## if either x, y rectilinear assume both are
  #if (sum(regular[1:2]) == 1) regular[1:2] <- c(FALSE, FALSE)
  for (i in seq_along(coords)) {
    if (isTRUE(regular[i])) {
      dx <- diff(coords[[i]][1:2])
      dimensions[[i]]$offset[1L] = coords[[i]][ncsub[i, "start"]] - dx/2
      ## NaN for singleton dims, but that seems ok unless we have explicit interval?
      dimensions[[i]]$delta[1L]  = mean(diff(coords[[i]]))  
    } else {
      dimensions[[i]]$values = coords[[i]]
      ## offset/delta for fall-back index (and for NA test )
      ## https://github.com/r-spatial/stars/blob/master/R/dimensions.R#L294-L303
      dimensions[[i]]$offset[1L] = NA
      dimensions[[i]]$delta[1L] = NA  
    }
  }
  
  
  ret = st_stars(out, dimensions)
  if (length(curvilinear) == 2) {
    curvi_coords = lapply(curvilinear, function(.v) ncdf4::ncvar_get(nc, 
                                                    varid = .v,
                                                    ## note there subtle subsetting into x,y
                                                    start = ncsub[1:2, "start", drop = TRUE], 
                                                    count = ncsub[1:2, "count", drop = TRUE], 
                                                    collapse_degen = FALSE))
    names(curvi_coords) <- c("x", "y")
    ret = st_as_stars(ret, curvilinear = curvi_coords)
  }
  ret
}
