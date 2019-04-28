.is_regular <- function(coords_list, eps) {
  sapply(coords_list, function(x) regular_intervals(x, epsilon = eps))
}

.unique_fuzz <- function(x, eps) {
  u = unique(x)
  if (all(diff(sort(u)) < eps))
    mean(x) # rather than mean(u)
  else
    u
}


# TODO
# - allow stars_proxy to be pulled
# - allow stars_proxy to apply select_var
# - 
#' @examples
#' f <- system.file("nc/reduced.nc", package = "stars")
#' read_stars_tidync(f)
#' read_stars_tidync(f, select_var = "anom", proxy = FALSE) ## only works if proxy = FALSE
#' read_stars_tidync(f, lon = index <= 10, lat = index <= 12, time = index < 2)
read_stars_tidync = function(.x, ..., select_var = NULL, proxy = TRUE) {
  if (length(.x) > 1) {
    warning("only first source/file used")
    .x = .x[1L]
  }
  if (!proxy) {
    x = tidync::tidync(.x) %>% 
      tidync::hyper_filter(...) %>% 
       tidync::hyper_array(select_var = select_var, drop = FALSE)  ## always keep degenerate dims
    tt = attr(x, "transforms")
    
    nms = names(tt)
    tt = lapply(tt, function(tab) tab[tab$selected, , drop = FALSE])
    dims = stars:::create_dimensions(setNames(lapply(nms, 
                                                     function(nm) stars:::create_dimension(values = tt[[nm]][[nm]])), nms))
    attr(x, "transforms") = NULL
    attr(x, "source") = NULL
    
    class(x) = "list"
    out = stars:::st_stars(x, dims)
  } else {
    x = tidync::tidync(.x) %>% tidync::hyper_filter(...) #%>% tidync::hyper_array()
    tt = tidync::hyper_transforms(x, all = FALSE)
    nms = names(tt)
    tt =  lapply(tt, function(tab) tab[tab$selected, , drop = FALSE])
    dims = stars:::create_dimensions(setNames(lapply(nms, 
                                                     function(nm) stars:::create_dimension(values = tt[[nm]][[nm]])), nms))
    
    out = structure(list(names = .x), dimensions = dims, NA_value = NA, class = c("stars_proxy", "stars"))
  }
  out
}



#' Read NetCDF into stars object
#'
#' Read data from a file (or source) using the NetCDF library directly.
#'
#' The following logic is applied to coordinates. If any coordinate axes have
#' regularly spaced coordinate variables they are reduced to the
#' offset/delta form with 'affine = c(0, 0)', otherwise the values of the coordinates
#' are stored and used to define a rectilinear grid.
#'
#' If the data has two or more dimensions and the first two are regular
#' they are nominated as the 'raster' for plotting.
#'
#' If the \code{curvilinear} argument is used it specifies the 2D arrays
#' containing coordinate values for the first two dimensions of the data read. It is currently
#' assumed that the coordinates are 2D and that they relate to the first two dimensions in
#' that order.
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
#' @param ncsub matrix of start, count columns (see Details)
#' @param curvilinear length two character vector with names of subdatasets holding longitude and latitude values for all raster cells.
#' @param eps numeric; dimension value increases are considered identical when they differ less than \code{eps}
#' @param make_time if \code{TRUE} (the default), an atttempt is made to provide a date-time class from the "time" variable
#' @param make_units if \code{TRUE} (the default), an attempt is made to set the units property of each variable
#' @details
#' If \code{var} is not set the first set of variables on a shared grid is used.
#'
#' \code{start} and \code{count} columns of ncsub must correspond to the variable dimemsion (nrows)
#' and be valid index using \code{\link[RNetCDF]{var.get.nc}} convention (start is 1-based). If the count value
#' is \code{NA} then all steps are included. Axis order must match that of the variable/s being read.
#' @export
#' @examples
#' #' precipitation data in a curvilinear NetCDF
#' prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
#' prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"))
#'
#' ##plot(prec) ## gives error about unique breaks
#' ## remove NAs, zeros, and give a large number
#' ## of breaks (used for validating in detail)
#' qu_0_omit = function(x, ..., n = 22) {
#'   x = na.omit(x)
#'   c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
#' }
#' library(dplyr)
#' prec_slice = slice(prec, index = 17, along = "time")
#' plot(prec_slice, border = NA, breaks = qu_0_omit(prec_slice[[1]]), reset = FALSE)
#' nc = sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")
#' plot(st_geometry(nc), add = TRUE, reset = FALSE, col = NA)
read_ncdf = function(.x, ..., var = NULL, ncsub = NULL, curvilinear = character(0),
    eps = 1e-12, make_time = TRUE, make_units = TRUE) {

  if (!requireNamespace("ncmeta", quietly = TRUE))
    stop("package ncmeta required, please install it first") # nocov
  if (!requireNamespace("RNetCDF", quietly = TRUE))
    stop("package RNetCDF required, please install it first") # nocov

  meta = ncmeta::nc_meta(.x)
  coord_var <- ncmeta::nc_coord_var(.x)
  
  # Don't want scalar
  # todo handle choice of grid
  nas <- is.na(meta$axis$dimension)
  if (any(nas)) meta$axis$dimension[nas] <- -1 # nocov
  if (is.null(var)) {
    ix <- 1
    if (meta$grid$grid[ix] == "S") {
      ix <- which(!meta$grid$grid == "S")[1L] # nocov

      if (length(ix) < 1)  stop("only scalar variables found, not yet supported") # nocov
    }
    if (utils::packageVersion("ncmeta") <= "0.0.3") {
     var = meta$grid$variable[meta$grid$grid[ix] == meta$grid$grid]
    } else {
     grd = meta$grid$grid[which.max(nchar(meta$grid$grid))]
     var = meta$grid$variables[[match(grd, meta$grid$grid)]]$variable
    }
    
    message(sprintf("no 'var' specified, using %s", paste(var, collapse = ", ")))
    message(sprintf("other available variables:\n %s", paste(setdiff(meta$variable$name, var), collapse = ", ")))
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

  nc = RNetCDF::open.nc(.x)
  on.exit(RNetCDF::close.nc(nc), add = TRUE)
  out = lapply(var, function(.v) RNetCDF::var.get.nc(nc,
                                                     variable = .v,
                                                     start = ncsub[, "start", drop = TRUE],
                                                     count = ncsub[, "count", drop = TRUE],
                                                     collapse = FALSE, ## keep 1-dims
                                                     unpack = TRUE,     ## offset and scale applied internally
                                                     rawchar = FALSE))

  vars = ncmeta::nc_vars(nc)
  out = setNames(out, var)
  # units:
  ## FIXME: this should be a meta-getter function, with NULL as fallback
  nc_units = meta$attribute[meta$attribute$attribute == "units", ]
  if (nrow(nc_units) > 0 && make_units) {
    for (i in var) {
      if (is.numeric(out[[i]]) && i %in% nc_units$variable) {
        uval = unlist(nc_units$value[nc_units$variable == i])
        units(out[[i]]) = try_as_units(uval[1L])
      }
    }
  }
  ## cannot assume we have coord dims
  ## - so create them as 1:length if needed
  coords = setNames(vector("list", length(dims$name)), dims$name)
  for (ic in seq_along(coords)) {
    subidx <- seq(ncsub[ic, "start"], length = ncsub[ic, "count"])

    ## create_dimvar means we can var_get it
    ## test checks if there's actuall a variable of the dim name
    if (dims$name[ic] %in% vars$name) {
      coords[[ic]] <- RNetCDF::var.get.nc(nc, variable = dims$name[ic])[subidx]

    } else {
      coords[[ic]] <- subidx##seq_len(dims$length[ic])
    }
  }

  ## can we create a raster?
  raster = NULL
  ## which coords are regular
  regular = .is_regular(coords, eps)
  if (length(coords) > 1) {
    raster = get_raster(affine = c(0, 0),
                        dimensions = names(coords)[1:2], curvilinear = FALSE)
  }

  
  ## we expect an error with "../rasterwise/extdata/R13352.nc"
  ## because of https://github.com/hypertidy/tidync/issues/75
  ## best to fix with tidync-reader
  dimensions = create_dimensions(setNames(dim(out[[1]]), dims$name), raster)

  ## if either x, y rectilinear assume both are
  #if (sum(regular[1:2]) == 1) regular[1:2] <- c(FALSE, FALSE)
  var_names = meta$variable$name
  
  to_rectilinear = FALSE
  for (i in seq_along(coords)) {
    if (names(coords)[i] %in% var_names &&
        length(bounds <- coord_var[coord_var$variable == names(coords)[i], ]$bounds) > 0 &&
        bounds %in% var_names) {

      bounds = RNetCDF::var.get.nc(nc, bounds)
      if (!is.matrix(bounds)) # single instance, returns a vector
        bounds = matrix(bounds, nrow = 2)
      is_reg = ncol(bounds) > 1 && length(u <- .unique_fuzz(apply(bounds, 2, diff), eps)) == 1 &&
        length(v <- .unique_fuzz(diff(bounds[1,]), eps)) == 1

      if (is_reg && abs(u + v) < eps) {
          warning(paste("bounds for", names(coords)[i], "seem to be reversed; reverting them"))
          bounds = apply(bounds, 2, sort) # should not be needed according to CF, but see #133
          u = v

      }
      
      if (is_reg && abs(v - u) < eps) {
        dimensions[[i]]$offset = bounds[1,1]
        dimensions[[i]]$delta = v
      } else {
        dimensions[[i]]$values = make_intervals(bounds[1,], bounds[2,])
        dimensions[[i]]$point = FALSE
        if (i %in% 1:2) # FIXME: ? hard-coding here that lon lat are in the first two dimensions:
          to_rectilinear = TRUE
    }
    } else if (regular[i]) {
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
  if (to_rectilinear)
    dimensions = as_rectilinear(dimensions)

  
  # sort out time -> POSIXct:
  ## assuming that the variable is called "time", no other option
  if (make_time) {
    if ("time" %in% names(dimensions)) {
      atts = meta$attribute[meta$attribute$variable == "time", ]
      #cal = RNetCDF::att.get.nc(nc, variable = "time", attribute = "calendar")
      ## might not exist, so default to NULL
      calendar = unlist(atts$value[atts$attribute == "calendar"])[1L]
      tunit = unlist(atts$value[atts$variable == "time"])[1L]
      dimensions <- make_cal_time(nc, dimensions, time_unit = tunit, calendar)
    }
  }

  ret = st_stars(out, dimensions)

  if (length(curvilinear) == 2) {
    curvi_coords = lapply(curvilinear, function(.v) RNetCDF::var.get.nc(nc,
                                                                        variable = .v,
                                                                        ## note there subtle subsetting into x,y
                                                                        start = ncsub[1:2, "start", drop = TRUE],
                                                                        count = ncsub[1:2, "count", drop = TRUE],
                                                                        collapse = FALSE,
                                                                        unpack = TRUE))
    names(curvi_coords)[1:2] <- names(dimensions)[1:2]
    st_as_stars(ret, curvilinear = curvi_coords)
  } else
    ret
}

as_rectilinear = function(d) {
    ed = expand_dimensions(d, center = FALSE)
    for (i in attr(d, "raster")$dimensions) {
        if (!is.na(d[[ i ]]$offset)) {
            d[[i]]$values = as_intervals(ed[[i]], add_last = TRUE)
            d[[i]]$offset = d[[i]]$delta = NA
        }
    }
    d
}

## FIXME: this function should return unit-ed times, not modify dimensions
## it should also live in the units package (or RNetCDF, or PCICt or ...)
make_cal_time <- function(nc, dimensions, time_unit = NULL, cal = NULL) {
  td = which(names(dimensions) == "time")
  if (length(td) == 1) {
    tm = get_dimension_values(dimensions[["time"]], geotransform = NA)
    u = time_unit
    if (is.null(u) || inherits(u, "try-error")) {
      warning("no variable 'time', ignoring units of time dimension")
      return(dimensions)
    }
    
    ##cal = RNetCDF::att.get.nc(nc, variable = "time", attribute = "calendar")
    if (! is.null(cal)  && cal %in% c("360_day", "365_day", "noleap")) {
      if (!requireNamespace("PCICt", quietly = TRUE))
        stop("package PCICt required, please install it first") # nocov
      t01 = set_units(0:1, u, mode = "standard")
      delta = if (grepl("months", u)) {
        if (cal == "360_day")
          set_units(30 * 24 * 3600, "s", mode = "standard")
        else
          set_units((365/12) * 24 * 3600, "s", mode = "standard")
      } else
        set_units(as_units(diff(as.POSIXct(t01))), "s", mode = "standard")
      origin = as.character(as.POSIXct(t01[1]))
      v.pcict = PCICt::as.PCICt(tm * as.numeric(delta), cal, origin)
      if (!is.null(dimensions[[td]]$values)) {
        v = dimensions[[td]]$values
        if (inherits(v, "intervals")) {
          start = PCICt::as.PCICt(v$start * as.numeric(delta), cal, origin)
          end =   PCICt::as.PCICt(v$end   * as.numeric(delta), cal, origin)
          dimensions[[td]]$values = make_intervals(start, end)
        } else
          dimensions[[td]]$values = v.pcict
      } else {
        dimensions[[td]]$offset = v.pcict[1]
        dimensions[[td]]$delta = diff(v.pcict[1:2])
      }
      dimensions[[td]]$refsys = "PCICt"
    } else { # Gregorian/Julian, POSIXct:
      if (!is.null(dimensions[[td]]$values)) {
        v = dimensions[[td]]$values
        if (inherits(v, "intervals")) {
          start = as.POSIXct(units::set_units(v$start, u, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
          end =   as.POSIXct(units::set_units(v$end,   u, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
          dimensions[[td]]$values = make_intervals(start, end)
        } else
          dimensions[[td]]$values = as.POSIXct(units::set_units(tm, u, mode = "standard")) # or: RNetCDF::utcal.nc(u, tm, "c")
      } else {
        t0 = dimensions[[td]]$offset
        t1 = dimensions[[td]]$offset + dimensions[[td]]$delta
        t.posix = as.POSIXct(units::set_units(c(t0, t1), u, mode = "standard")) # or: utcal.nc(u, c(t0,t1), "c")
        dimensions[[td]]$offset = t.posix[1]
        dimensions[[td]]$delta = diff(t.posix)
      }
      dimensions[[td]]$refsys = "POSIXct"
    }
  }
  dimensions
}



