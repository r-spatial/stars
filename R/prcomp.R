#' @name prcomp
#' @param downsample see \link{st_as_stars}
#' @export
prcomp.stars_proxy = function(x, ..., downsample = 0) {
	prcomp(st_as_stars(x, downsample = downsample), ...)
}

#' Principle components of stars object
#'
#' Compute principle components of stars object
#' @param x object of class `stars` or `stars_proxy`
#' @param quiet logical; if `TRUE`, suppress message that PCs will be computed on last dimension; see details
#' @param ... see \link[stats]{prcomp}
#' @details if `x` has only one attribute, principle components will be computed in the space of the last dimension of `x`
#' to predict PC scores into a `stars` object, use \link{predict.stars}; see example below
#' @name prcomp
#' @returns object of class `prcomp`, see \link[stats]{prcomp}
#' @export
#' @examples
#' l7 = split(st_as_stars(L7_ETMs), 3) # use bands as features
#' l7 |> prcomp() |> plot()
#' l7 |> prcomp() |> predict(l7, model = _) |> merge() |> plot()
prcomp.stars = function(x, ..., quiet = FALSE) {
	if (length(x) == 1) {
		if (!quiet)
			message(paste0("computing PCs over dimension `", tail(names(dim(x)), 1), "'"))
		x = split(x, length(dim(x)))
	}
	prcomp(as.data.frame(x), ...)
}
