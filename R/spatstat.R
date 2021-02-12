check_spatstat <- function(pkg, X = NULL){
  if(!requireNamespace(pkg, quietly = TRUE)){
    stop("package ", pkg, " required, please install it (or the full spatstat package) first")
  } else{
    spst_ver <- try(packageVersion("spatstat"), silent = TRUE)
    if(!inherits(spst_ver, "try-error") && spst_ver < 2.0-0){
      stop("You have an old version of spatstat installed which is incompatible with ", pkg, 
           ". Please update spatstat (or uninstall it).")
    }
  }
  if (!is.null(X) && isTRUE(st_is_longlat(X)))
    stop("Only projected coordinates may be converted to spatstat class objects")
}

as.owin.stars = function(W, ..., fatal) {
  check_spatstat("spatstat.geom", W)
	if (length(dim(W)) != 2)
		stop("as.owin.stars requires a 2-dimensional object (single raster layer)")
	m = t(W[[1]])
	bb = st_bbox(W)
	spatstat.geom::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")], mask = m[nrow(m):1,])
}
