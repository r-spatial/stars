### The functions in this file are a work in progress and are ###
### not currently used by `stars`                             ###

units_or_null <- function(u) {
  un = try(suppressWarnings(as_units(u)), silent = TRUE)
  if (inherits(un, "try-error")) {
    #warning(paste("ignoring unrecognized unit:", u), call. = FALSE)
    NULL
  } else
    un
}
unit_izer_ncmeta <- function(attribute, variable, ...) {
  unit_df <- attribute[attribute$name == "units", ]
  if (is.null(unit_df) || nrow(unit_df) < 1) return(NULL)
  unit <- lapply(unit_df$value, function(x) units_or_null(x[[1]]))
  unit_df$unit <- unit
  unit_df$unit_ok <- !unlist(lapply(unit, is.null))
  variable$unit <- unit_df$unit[match(variable$name, unit_df$variable)]
  variable
}
# TODO
# - allow stars_proxy to be pulled
# - allow stars_proxy to apply select_var
# - 
# examples
# f <- system.file("nc/reduced.nc", package = "stars")
# read_stars_tidync(f)
# read_stars_tidync(f, select_var = "anom", proxy = FALSE) ## only works if proxy = FALSE
# read_stars_tidync(f, lon = index <= 10, lat = index <= 12, time = index < 2)
#' @importFrom units set_units
read_stars_tidync = function(.x, ..., select_var = NULL, proxy = TRUE, make_time = TRUE, make_units = TRUE) {
  if (!requireNamespace("tidync", quietly = TRUE))
    stop("package tidync required, please install it first") # nocov
  
  if (length(.x) > 1) {
    warning("only first source/file used")
    .x = .x[1L]
  }
  coord_var = ncmeta::nc_coord_var(.x)
  tnc =  tidync::tidync(.x) %>%  tidync::hyper_filter(...)
  if (!is.null(select_var)) tnc = tidync::activate(tnc, select_var[1])
  ## FIXME: get this stuff from tidync object
  variable = unit_izer_ncmeta(ncmeta::nc_atts(.x), ncmeta::nc_vars(.x))

  tt = tidync::hyper_transforms(tnc, all = FALSE)
  tt =  lapply(tt, function(tab) tab[tab$selected, , drop = FALSE])
  nms = names(tt)
  
  if (make_units) {
    for (idim in seq_along(tt)) {
      if (nms[idim] %in% variable$name) {
        uit <- variable$unit[match(names(tt)[idim], variable$name)][[1]]
        if (nms[idim] %in% coord_var$variable && !nms[idim] %in% coord_var[["T"]]) {
          
      ## we have a standard units thing
      if (!is.null(uit)) {
        tt[[idim]][[nms[idim]]] = units::as_units(tt[[idim]][[nms[idim]]], uit)
      }
      
        }
      }}}
  
  curvilinear = character(0)
  ## are we curvilinear?
  XY_curvi <- NULL
  if (all(c("X", "Y") %in% names(coord_var))) {
    cvar = coord_var[coord_var$variable == tidync::hyper_vars(tnc)$name[1], ]
    XY_curvi = unlist(cvar[1, c("X", "Y")])
   
    if (all(!is.na(XY_curvi)) && all(tnc$variable$ndims[match(XY_curvi, tnc$variable$name)] == 2)) {
      ## AAND these are both 2d vars
      curvilinear = XY_curvi
    } else {
      XY_curvi <- character(0)
    }
  }
  curvi_coords = NULL
  if (length(curvilinear == 2)) {
   #grid = tnc$grid %>% tidyr::unnest()
   grid = tnc$grid %>% tidyr::unnest(cols = c(variables))
   curvi_coords = tidync::hyper_array(tnc %>%
                                       ## FIXME ...
                                       tidync::activate((grid %>% 
                                                           dplyr::filter(variable == XY_curvi[1]) %>% 
                                                           dplyr::pull(.data$grid))[1L]), select_var = XY_curvi)
  
   names(curvi_coords)[1:2] <- curvilinear
  }
  ## can we create a raster?
  raster = NULL
  if (length(tt) > 1) {
    raster = get_raster(affine = c(0, 0),
                        dimensions = names(tt)[1:2], curvilinear = length(curvilinear) == 2)
  }
  dims = vector("list", length(nms))
  for (i in seq_along(dims)) {
    nm = nms[i]
    if (nrow(tt[[nm]]) > 1) { ## we are rectilinear, or degenerate rectilinear
      dims[[i]] = create_dimension(values = tt[[nm]][[nm]])
    } else { ## we are simple offset
      uval = tt[[nm]][[nm]]  ## so we can get an NA, whether units of not
      
      dims[[i]] = create_dimension(from = 1L, to = 1L, offset = uval, delta = uval[NA])
    }
  }
  dims = create_dimensions(setNames(dims, nms), raster = raster)
  if (!is.null(curvi_coords)) {

    dims[[names(tt)[1]]]$values = curvi_coords[[1]]
    dims[[names(tt)[1]]]$offset = NA
    dims[[names(tt)[1]]]$delta = NA
    
    dims[[names(tt)[2]]]$values = curvi_coords[[2]]
    dims[[names(tt)[2]]]$offset = NA
    dims[[names(tt)[2]]]$delta = NA
    
    
  }
  if (make_time) {
   for (idim in seq_along(tt)) {
     if (nms[idim] %in% variable$name) {
       uit <- variable$unit[match(names(tt)[idim], variable$name)][[1]]
       if (nms[idim] %in% coord_var$variable && nms[idim] %in% coord_var[["T"]]) {
         ## we have a time thing
         dims[[idim]]  = make_cal_time2(dims[[idim]], nms[idim], uit)
       } 
     }
   }
  }
    
  
  
  if (!proxy) {
    x = tnc %>% 
       tidync::hyper_array(select_var = select_var, drop = FALSE)  ## always keep degenerate dims
    attr(x, "transforms") = NULL
    attr(x, "source") = NULL
    class(x) = "list"
    for (ivar in seq_along(x)) {
      if (names(x)[ivar] %in% variable$name) {
        uit <- variable$unit[match(names(x)[ivar], variable$name)][[1]]
        if (!is.null(uit)) x[[ivar]] <- units::as_units(x[[ivar]], uit)
      }
    }
    out = st_stars(x, dims)
    out = st_as_stars(out, curvilinear = curvi_coords)
    
    
  } else {

    ## if we are proxy, defer to GDAL subdatasets
    hvars = if (is.null(select_var)) tidync::hyper_vars(tnc)$name else select_var
    sds = unlist(sf::gdal_subdatasets(.x))
    if (length(sds) > 0)  {
      sds = unlist(lapply(sprintf(":%s", hvars), function(p) grep(p, sds, value = TRUE)))
    } else {
      sds = .x
    }
    out = stats::setNames(as.list(sds), hvars)
    out = st_stars_proxy(out, dims, NA_value = NA)
    #class(out) = c("ncdf_proxy", class(out))
  }
  
  
  out
}

## FIXME: this function should return unit-ed times, not modify dimensions
## it should also live in the units package (or RNetCDF, or PCICt or ...)
make_cal_time <- function(dimensions, time_name, time_unit = NULL, cal = NULL) {
  td = which(names(dimensions) == time_name)
  if (length(td) == 1) {
    tm = get_dimension_values(dimensions[[time_name]], geotransform = NA)
    u = time_unit
    if (is.null(u) || inherits(u, "try-error")) {
      warning("ignoring units of time dimension, not able to interpret")
      return(dimensions)
    }
    
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
