#' @importFrom RNetCDF open.nc var.get.nc close.nc
#' @importFrom ncmeta nc_meta
#' @importFrom stats setNames
NULL



.is_regular <- function(coords_list) {
  unlist(lapply(coords_list, function(x) regular_intervals(x)))
}


#' Read NetCDF file into stars object
#' 
#' Read data from a file or URL using the RNetCDF library directly. 
#' 
#' The following logic is applied to coordinate variables. If any dimensions have 
#' regularly spaced coordinate variables they are reduced to the
#' offset/delta form with 'affine = c(0, 0)', otherwise the values of the coordinates
#' are stored and used to define an irregular rectilinear grid. 
#' 
#' If the data has two or more dimensions and the first two are regular
#' they are nominated as the 'raster' for plotting. 
#' 
#' If a grid_mapping is found in the NetCDF file, an attempt to convert it to a
#' projection is made. If no grid_mapping is found, WGS84 lat/lon is assumped.
#' 
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
#' @param ncsub matrix of start, count columns. NA count gets all. Axis order must match that of the variable that will be requested.
#' @param curvilinear length two character vector with names of subdatasets holding longitude and latitude values for all raster cells.
#' @details
#' If `var` is not set, variables that are not coordinate variables are used.
#' Data sources with non-coordinate variables that use different axes or axis orders are not supported.
#' 
#' \code{start} and \code{count} columns of ncsub must correspond to the variable dimemsion (nrows)
#' and be valid index using `RNetCDF::var.get.nc` convention (start is 1-based). 
#' @export
read_ncdf = function(.x, ..., var = NULL, ncsub = NULL, curvilinear = character(0)) {
  meta = ncmeta::nc_meta(.x)
  # Don't want scalar
  # todo handle choice of grid
  
  coord_vars <- ncmeta::nc_coord_var(.x) # coord_vars as determined by metadata/attributes
  
  cds <- check_cds(coord_vars, meta) # eliminates 2d coord vars when 1d vars exist.
  
  coord_vars <- dplyr::filter(coord_vars,
                              apply(coord_vars[, c("X", "Y", "Z", "T")], 1, 
                                    function(x) any(x %in% cds$variable)))
  
  if (is.null(var)) {
    var <- meta$variable$name[!meta$variable$name %in% cds$variable & 
                                !meta$variable$name %in% coord_vars$bounds &
                                meta$variable$ndims > 1]
    if(any(meta$grid$grid == "S")) var <- var[var != meta$grid$variable[meta$grid$grid == "S"]]
  }
  
  nas <- is.na(meta$axis$dimension)
  if (any(nas)) meta$axis$dimension[nas] <- -1
  
  # This ensures that dims and ncsub are valid for all variables we are looking at.
  dims_index <- unique(lapply(var, function(.v) meta$axis$dimension[meta$axis$variable == .v]))
  dims_index <- dims_index[sapply(dims_index, function(x) all(x != -1))]
  #browser()
  if(length(dims_index) > 1) stop("Variables with different axis orders found, select one to continue.") #nocov
  
  dims_index <- dims_index[[1]] # dims_index is in the axis order used by all data variables
  dims = meta$dimension[match(dims_index, meta$dimension$id), ]
  
  coord_var_axis <- coord_vars[!coord_vars$variable %in% var, ]
  coord_vars <- coord_vars[coord_vars$variable %in% var, ] # only data variables
  
  XYZT_dim <- suppressWarnings(sapply(c("X", "Y", "Z", "T"), 
                                      function(axis) 
                                        meta$axis[meta$axis$variable == 
                                                    coord_var_axis[[axis]][!is.na(coord_var_axis[[axis]])], ]$dimension))
  XYZT_dim <- XYZT_dim[lengths(XYZT_dim) > 0]
  
  reorder_var <- c()
  if(all(!dims$id == as.numeric(XYZT_dim))) {
    warning("Found non-canonical axis order in NetCDF unexpected bahavior may result.")
    reorder_var <- match(dims$id, as.numeric(XYZT_dim))
  }
  
  ## need to validate existing ncsub here
  if (is.null(ncsub)) {
    ncsub = cbind(start = 1, count = dims$length)
    rownames(ncsub) = dims$name
    if(prod(ncsub[, "count"]) * length(var) > 1e+08) stop("very large data requested, are you sure?")
  } else {
    ## needs more attention
    if (nrow(dims) != nrow(ncsub)) stop("input ncsub doesn't match available dims") # nocov
    not_na <- !is.na(ncsub[, "count"])
    if (any(ncsub[, "start"] < 1) || 
        any((ncsub[, "count"][not_na] - 
             ncsub[, "start"][not_na] + 1) > dims$length)) stop("start or count out of bounds")  # nocov
  }
  
  nc = RNetCDF::open.nc(.x)
  on.exit(RNetCDF::close.nc(nc), add = TRUE)
  out = lapply(var, function(.v) RNetCDF::var.get.nc(nc, 
                                                     variable = .v,
                                                     start = ncsub[, "start", drop = TRUE], 
                                                     count = ncsub[, "count", drop = TRUE], 
                                                     collapse = FALSE, 
                                                     unpack = TRUE))
  out = setNames(out, var)
  
  # If any counts are NA to get all this sets them to true size.
  # Given assumptions above, this must be true.
  ncsub[, "count"] <- dim(out[[1]])
  
  ## cannot assume we have 1d coord variables
  ## - so create them as 1:length(dim) if needed
  coords = setNames(vector("list", length(dims$name)), dims$name)
  
  for (ic in seq_along(coords)) {
    if (!is.null(ncsub)) {
      subidx <- seq(ncsub[ic, "start"], length = ncsub[ic, "count"])
    } else {
      subidx <- seq_len(length(coords[[ic]]))
    }
    ## if there is a coordinate variable that matches the dimension name
    if (dims$name[ic] %in% c(coord_vars$X, coord_vars$Y, coord_vars$Z, coord_vars$T)) {
      coords[[ic]] <- RNetCDF::var.get.nc(nc, variable = dims$name[ic])[subidx] # could use start/count here too?
      # otherwise coordinate variables are bound to variables and need to be handled differently.
    } else {
      coords[[ic]] <- subidx
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
    curvi_coords = lapply(curvilinear, function(.v) RNetCDF::var.get.nc(nc, 
                                                    variable = .v,
                                                    ## note there subtle subsetting into x,y
                                                    start = ncsub[1:2, "start", drop = TRUE], 
                                                    count = ncsub[1:2, "count", drop = TRUE], 
                                                    collapse = FALSE, unpack = TRUE))
    names(curvi_coords) <- c("x", "y")
    ret = st_as_stars(ret, curvilinear = curvi_coords)
  }
  
  try({
    gm_atts <- ncmeta::nc_grid_mapping_atts(.x)
    
    if(length(unique(gm_atts$name[gm_atts$name == "grid_mapping_name"])) > 1) {
      warning("Multiple grid mappings found for this NetCDF source. Not setting CRS.")
    }
    
    if("data_variable" %in% names(gm_atts)) {
      gm_atts <- gm_atts[gm_atts$data_variable == gm_atts$data_variable[1], 
                         c("name", "value")]
    }
    
    sf::st_crs(ret) <- sf::st_crs(ncmeta::nc_gm_to_prj(gm_atts))
  })
  
  ret
}

check_cds <- function(coord_vars, meta) {
  cds <- unique(as.vector(unlist(coord_vars[, c("X", "Y", "Z", "T")])))
  cds <- cds[!is.na(cds)]
  cds <- tibble::tibble(variable = cds) %>%
    dplyr::left_join(coord_vars, by = c("variable"))
  if(sum(!is.na(cds$X)) > 1 | sum(!is.na(cds$Y)) > 1 | sum(!is.na(cds$Z)) > 1 | sum(!is.na(cds$T)) > 1)
    cds <- dplyr::filter(cds, variable %in% X | 
                           variable %in% Y | 
                           variable %in% Z | 
                           variable %in% T)
  return(cds)
}
